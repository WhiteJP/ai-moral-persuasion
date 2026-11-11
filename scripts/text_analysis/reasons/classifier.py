"""
Shared OpenRouter multi-label reason classifier.

Loads Study 2 free-text reasons from study2_conversations.csv, classifies
with three models via OpenRouter (async + retry + checkpoint), and writes
majority-vote CSVs. Meat/charity default to skeptics (cond_belief_pre <= 50);
lying/political violence use believers (cond_belief_pre >= 50).

Default raters are chat models with thinking off and temperature 0
(Claude Sonnet 4.5, GPT-4.1, DeepSeek V3.2).
"""

from __future__ import annotations

import argparse
import asyncio
import json
import os
import random
import shutil
from pathlib import Path
from typing import Any

import pandas as pd
from openai import AsyncOpenAI
from tqdm.asyncio import tqdm_asyncio

# ---------------------------------------------------------------------------
# Paths (repo root = four levels above this file)
# ---------------------------------------------------------------------------

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parents[2]
CONVERSATIONS_CSV = REPO_ROOT / "data" / "study_2" / "study2_conversations.csv"
OUTPUT_DIR = REPO_ROOT / "output" / "study_2" / "text_analysis"

# ---------------------------------------------------------------------------
# OpenRouter defaults
# ---------------------------------------------------------------------------

OPENROUTER_BASE_URL = "https://openrouter.ai/api/v1"
MODELS = [
    "anthropic/claude-sonnet-4.5",
    "openai/gpt-4.1",
    "deepseek/deepseek-v3.2",
]
MODEL_NAMES = ["claude", "gpt", "deepseek"]

# Thinking off. GPT-4.1 does not accept a reasoning parameter.
DEFAULT_EXTRA_BODY_BY_MODEL = {
    "anthropic/claude-sonnet-4.5": {
        "reasoning": {"enabled": False},
        "provider": {"require_parameters": True},
    },
            "openai/gpt-4.1": {
                "provider": {"require_parameters": True},
            },
    "deepseek/deepseek-v3.2": {
        "reasoning": {"enabled": False},
        "provider": {"require_parameters": True},
    },
}
DEFAULT_TEMPERATURE = 0.0
DEFAULT_MAX_TOKENS = 1500

MAX_RETRIES = 5
BASE_DELAY = 1.0
MAX_DELAY = 60.0
SEMAPHORE_LIMIT = 10

# ---------------------------------------------------------------------------
# Study 2 reasons from the conversation CSV
# ---------------------------------------------------------------------------

_BELIEF_OPS = {"<=": "<=", ">=": ">="}


def _load_conversations(csv_path: Path = CONVERSATIONS_CSV) -> pd.DataFrame:
    if not csv_path.exists():
        raise FileNotFoundError(
            f"Conversation CSV not found: {csv_path}\n"
            "Run scripts/study_2/01_wrangle_data.R first."
        )
    return pd.read_csv(csv_path)


def export_reasons(
    condition: str,
    belief_op: str = "<=",
    belief_cutoff: int = 50,
    csv_path: Path = CONVERSATIONS_CSV,
    belief_col: str = "cond_belief_pre",
) -> pd.DataFrame:
    """Load Study 2 reasons for a condition, filtered on cond_belief_pre."""
    if belief_op not in _BELIEF_OPS:
        raise ValueError(f"belief_op must be one of {sorted(_BELIEF_OPS)}, got {belief_op!r}")

    raw = _load_conversations(csv_path)
    missing = {"participant_id", "condition", "reasons", belief_col} - set(raw.columns)
    if missing:
        raise RuntimeError(
            f"{csv_path.name} is missing columns: {', '.join(sorted(missing))}"
        )

    belief = pd.to_numeric(raw[belief_col], errors="coerce")
    if belief_op == "<=":
        belief_ok = belief <= belief_cutoff
    else:
        belief_ok = belief >= belief_cutoff

    text = raw["reasons"].fillna("").astype(str).str.strip()
    keep = (
        raw["condition"].astype(str).eq(condition)
        & belief_ok
        & text.ne("")
    )
    out = pd.DataFrame(
        {
            "id": raw.loc[keep, "participant_id"].astype(str).to_numpy(),
            "text": text.loc[keep].to_numpy(),
            "belief_pre": belief.loc[keep].to_numpy(),
        }
    )
    if out.empty:
        raise RuntimeError(
            f"No texts found for condition={condition!r}, "
            f"{belief_col} {belief_op} {belief_cutoff}"
        )
    return out


def export_skeptics(
    condition: str,
    csv_path: Path = CONVERSATIONS_CSV,
) -> pd.DataFrame:
    """Load Study 2 skeptics (condition + cond_belief_pre <= 50)."""
    return export_reasons(
        condition=condition,
        belief_op="<=",
        belief_cutoff=50,
        csv_path=csv_path,
    )


# ---------------------------------------------------------------------------
# Checkpoint helpers
# ---------------------------------------------------------------------------


def load_checkpoint(checkpoint_file: Path) -> set[str]:
    if checkpoint_file.exists():
        with open(checkpoint_file) as f:
            return set(json.load(f))
    return set()


def save_checkpoint(checkpoint_file: Path, processed_ids: set[str]) -> None:
    checkpoint_file.parent.mkdir(parents=True, exist_ok=True)
    with open(checkpoint_file, "w") as f:
        json.dump(sorted(processed_ids), f)


# ---------------------------------------------------------------------------
# Classification
# ---------------------------------------------------------------------------

_GENERATION_VERIFIED: set[str] = set()


async def _report_generation_temperature(
    api_key: str, model: str, generation_id: str, requested: float | None
) -> None:
    """Print OpenRouter's recorded temperature for the first call per model."""
    if not generation_id or model in _GENERATION_VERIFIED:
        return
    _GENERATION_VERIFIED.add(model)
    try:
        import httpx

        await asyncio.sleep(0.8)
        last_error: Exception | None = None
        async with httpx.AsyncClient(timeout=30.0) as http:
            for attempt in range(5):
                r = await http.get(
                    "https://openrouter.ai/api/v1/generation",
                    params={"id": generation_id},
                    headers={"Authorization": f"Bearer {api_key}"},
                )
                if r.status_code == 404:
                    last_error = Exception(f"404 Not Found for {generation_id}")
                    await asyncio.sleep(1.5 * (attempt + 1))
                    continue
                r.raise_for_status()
                last_error = None
                data = r.json().get("data", r.json())
                recorded = data.get("temperature")
                provider = data.get("provider_name") or data.get("provider")
                print(
                    f"  OpenRouter generation check [{model}]: "
                    f"requested temperature={requested!r}, "
                    f"recorded temperature={recorded!r}, "
                    f"provider={provider!r}"
                )
                if requested is not None and recorded is not None:
                    if abs(float(recorded) - float(requested)) > 1e-6:
                        print(
                            f"  WARNING: {model} recorded temperature {recorded} "
                            f"!= requested {requested}"
                        )
                break
            else:
                if last_error:
                    raise last_error
    except Exception as e:
        print(f"  OpenRouter generation check [{model}] failed: {e}")


async def classify_with_retry(
    client: AsyncOpenAI,
    model: str,
    text: str,
    semaphore: asyncio.Semaphore,
    categories: list[str],
    system_prompt: str,
    user_prompt_template: str,
    extra_body: dict[str, Any] | None = None,
    temperature: float | None = 0.0,
    max_tokens: int | None = None,
) -> dict[str, Any]:
    """Classify one text with one model; retry with exponential backoff."""
    empty = {cat: None for cat in categories}
    empty["reasoning"] = ""
    empty["error"] = None
    empty["generation_id"] = None
    empty["temperature"] = temperature

    async with semaphore:
        for attempt in range(MAX_RETRIES):
            try:
                kwargs: dict[str, Any] = {
                    "model": model,
                    "messages": [
                        {"role": "system", "content": system_prompt},
                        {
                            "role": "user",
                            "content": user_prompt_template.format(text=text),
                        },
                    ],
                    "response_format": {"type": "json_object"},
                }
                if temperature is not None:
                    kwargs["temperature"] = temperature
                if max_tokens is not None:
                    kwargs["max_tokens"] = max_tokens
                if extra_body:
                    kwargs["extra_body"] = extra_body
                response = await client.chat.completions.create(**kwargs)
                content = response.choices[0].message.content
                parsed = json.loads(content)
                generation_id = getattr(response, "id", None)
                api_key = os.environ.get("OPENROUTER_API_KEY", "")
                if api_key and generation_id:
                    await _report_generation_temperature(
                        api_key, model, generation_id, temperature
                    )
                return {
                    **{cat: bool(parsed.get(cat, False)) for cat in categories},
                    "reasoning": parsed.get("reasoning", ""),
                    "error": None,
                    "generation_id": generation_id,
                    "temperature": temperature,
                }
            except json.JSONDecodeError as e:
                if attempt == MAX_RETRIES - 1:
                    return {**empty, "error": f"JSON decode error: {e}"}
            except Exception as e:
                if attempt == MAX_RETRIES - 1:
                    return {**empty, "error": str(e)}
                delay = min(
                    BASE_DELAY * (2**attempt) + random.uniform(0, 1),
                    MAX_DELAY,
                )
                await asyncio.sleep(delay)

    return {**empty, "error": "Max retries exceeded"}


def compute_consensus(
    result: dict[str, Any],
    categories: list[str],
    model_names: list[str] | None = None,
) -> dict[str, Any]:
    """Majority vote across non-None model responses for each category."""
    names = model_names if model_names is not None else MODEL_NAMES
    for category in categories:
        votes = [result.get(f"{category}_{m}") for m in names]
        valid = [v for v in votes if v is not None]
        if len(valid) == 0:
            result[f"{category}_consensus"] = None
        elif len(valid) == 1:
            result[f"{category}_consensus"] = valid[0]
        else:
            result[f"{category}_consensus"] = sum(valid) > len(valid) / 2
    return result


async def classify_text_all_models(
    client: AsyncOpenAI,
    text: str,
    text_id: str,
    belief_pre: float,
    semaphores: dict[str, asyncio.Semaphore],
    categories: list[str],
    system_prompt: str,
    user_prompt_template: str,
    models: list[str] | None = None,
    model_names: list[str] | None = None,
    extra_body: dict[str, Any] | None = None,
    extra_body_by_model: dict[str, dict[str, Any]] | None = None,
    temperature: float | None = 0.0,
    max_tokens: int | None = None,
) -> dict[str, Any]:
    """Classify one text with all three models and compute consensus."""
    model_ids = models if models is not None else MODELS
    names = model_names if model_names is not None else MODEL_NAMES
    results = await asyncio.gather(
        *[
            classify_with_retry(
                client,
                model,
                text,
                semaphores[model],
                categories,
                system_prompt,
                user_prompt_template,
                extra_body=(
                    extra_body_by_model.get(model, extra_body)
                    if extra_body_by_model is not None
                    else extra_body
                ),
                temperature=temperature,
                max_tokens=max_tokens,
            )
            for model in model_ids
        ]
    )

    out: dict[str, Any] = {
        "id": text_id,
        "text": text,
        "belief_pre": belief_pre,
        "temperature": temperature,
    }
    for model_name, model_result in zip(names, results):
        for category in categories:
            out[f"{category}_{model_name}"] = model_result.get(category)
        out[f"reasoning_{model_name}"] = model_result.get("reasoning", "")
        out[f"error_{model_name}"] = model_result.get("error")
        out[f"generation_id_{model_name}"] = model_result.get("generation_id")

    return compute_consensus(out, categories, model_names=names)


# ---------------------------------------------------------------------------
# Main runner
# ---------------------------------------------------------------------------


def build_arg_parser(description: str) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument(
        "--run",
        action="store_true",
        help=(
            "Call the models and write a new CSV under output/study_2/text_analysis/. "
            "Without this flag the script does not call the API or write files."
        ),
    )
    parser.add_argument(
        "--write-data",
        action="store_true",
        help=(
            "After --run, copy the new CSV into data/study_2/text_ratings/. "
            "Omit this to leave the downloaded ratings in place."
        ),
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Classify only the first N texts (smoke test)",
    )
    return parser


def regeneration_requested(args: argparse.Namespace) -> bool:
    """True only when the caller passed --run."""
    if args.run:
        return True
    print(
        "Not regenerating ratings. The paper pipeline reads the files already "
        "in data/study_2/text_ratings/. Pass --run to call the models "
        "(and --write-data to copy the new CSV over those files)."
    )
    return False


def install_rating_files(paths: list[Path]) -> None:
    """Copy regenerated CSVs into data/study_2/text_ratings/."""
    dest_dir = REPO_ROOT / "data" / "study_2" / "text_ratings"
    dest_dir.mkdir(parents=True, exist_ok=True)
    for src in paths:
        if not src.exists():
            raise FileNotFoundError(f"Nothing to install: {src}")
        dest = dest_dir / src.name
        if dest.exists() and dest.resolve() == src.resolve():
            print(f"{src.name} is already the file in {dest_dir}")
            continue
        shutil.copy2(src, dest)
        print(f"Installed {dest}")


async def run_classifier(
    *,
    condition: str,
    categories: list[str],
    system_prompt: str,
    user_prompt_template: str,
    output_file: Path,
    checkpoint_file: Path,
    progress_label: str,
    limit: int | None = None,
    belief_op: str = "<=",
    belief_cutoff: int = 50,
    ids: list[str] | None = None,
    models: list[str] | None = None,
    model_names: list[str] | None = None,
    extra_body: dict[str, Any] | None = None,
    extra_body_by_model: dict[str, dict[str, Any]] | None = None,
    temperature: float | None = DEFAULT_TEMPERATURE,
    max_tokens: int | None = DEFAULT_MAX_TOKENS,
) -> None:
    """End-to-end: export reasons → classify → write CSV."""
    api_key = os.environ.get("OPENROUTER_API_KEY")
    if not api_key:
        raise SystemExit("Error: OPENROUTER_API_KEY environment variable not set")

    model_ids = models if models is not None else MODELS
    names = model_names if model_names is not None else MODEL_NAMES
    if extra_body is None and extra_body_by_model is None:
        extra_body_by_model = DEFAULT_EXTRA_BODY_BY_MODEL

    sample_label = (
        "skeptics" if belief_op == "<=" else "believers"
    )
    print(
        f"Loading Study 2 {sample_label} from {CONVERSATIONS_CSV.name} "
        f"(condition={condition!r}, cond_belief_pre {belief_op} {belief_cutoff})..."
    )
    df = export_reasons(
        condition=condition,
        belief_op=belief_op,
        belief_cutoff=belief_cutoff,
    )
    print(f"Found {len(df)} {sample_label} texts with non-empty reasons")

    if ids is not None:
        id_order = {str(i): n for n, i in enumerate(ids)}
        df = df[df["id"].astype(str).isin(id_order)]
        df = df.assign(_ord=df["id"].astype(str).map(id_order)).sort_values("_ord")
        df = df.drop(columns="_ord")
        print(f"Restricting to {len(df)} preselected ids")
    elif limit is not None:
        df = df.head(limit)
        print(f"Limiting to first {len(df)} texts (--limit {limit})")

    processed_ids = load_checkpoint(checkpoint_file)
    if processed_ids:
        print(f"Resuming from checkpoint: {len(processed_ids)} already processed")

    df_todo = df[~df["id"].astype(str).isin(processed_ids)]
    print(f"Processing {len(df_todo)} remaining samples")
    print(f"Models: {', '.join(model_ids)}")
    print(f"Temperature: {temperature!r} (sent on every request)")
    print(f"Max tokens: {max_tokens!r}")

    if len(df_todo) == 0:
        print("All samples already processed!")
        if output_file.exists():
            _print_summary(pd.read_csv(output_file), categories)
        return

    client = AsyncOpenAI(api_key=api_key, base_url=OPENROUTER_BASE_URL)
    semaphores = {model: asyncio.Semaphore(SEMAPHORE_LIMIT) for model in model_ids}

    tasks = [
        classify_text_all_models(
            client,
            text=str(row["text"]),
            text_id=str(row["id"]),
            belief_pre=float(row["belief_pre"]),
            semaphores=semaphores,
            categories=categories,
            system_prompt=system_prompt,
            user_prompt_template=user_prompt_template,
            models=model_ids,
            model_names=names,
            extra_body=extra_body,
            extra_body_by_model=extra_body_by_model,
            temperature=temperature,
            max_tokens=max_tokens,
        )
        for _, row in df_todo.iterrows()
    ]

    results: list[dict[str, Any]] = []
    for coro in tqdm_asyncio.as_completed(
        tasks, total=len(tasks), desc=progress_label
    ):
        result = await coro
        results.append(result)
        processed_ids.add(result["id"])
        if len(results) % 10 == 0:
            save_checkpoint(checkpoint_file, processed_ids)

    save_checkpoint(checkpoint_file, processed_ids)

    results_df = pd.DataFrame(results)
    if output_file.exists():
        existing = pd.read_csv(output_file)
        # Drop any rows we re-processed (shouldn't happen, but be safe)
        existing = existing[~existing["id"].astype(str).isin(results_df["id"])]
        combined = pd.concat([existing, results_df], ignore_index=True)
    else:
        combined = results_df

    output_file.parent.mkdir(parents=True, exist_ok=True)
    combined.to_csv(output_file, index=False)
    print(f"\nResults saved to {output_file}")
    _print_summary(combined, categories)


def _print_summary(df: pd.DataFrame, categories: list[str]) -> None:
    print("\n--- Classification Summary (consensus) ---")
    for category in categories:
        col = f"{category}_consensus"
        if col not in df.columns:
            continue
        n_true = int((df[col] == True).sum())  # noqa: E712
        n_false = int((df[col] == False).sum())  # noqa: E712
        n_na = int(df[col].isna().sum())
        print(f"  {category}: {n_true} True, {n_false} False, {n_na} NA")
    print(f"  N texts: {len(df)}")
