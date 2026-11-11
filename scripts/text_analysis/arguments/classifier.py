"""
Shared OpenRouter rater for Study 2 AI conversations.

Loads transcripts from study2_conversations.csv (control excluded), formats
them the same way Study 1 did (participant reasoning, then alternating AI
responses and participant replies), and rates each conversation with three
models via OpenRouter (async + retry + checkpoint).

Ratings are ordinal (none / low / moderate / high). Consensus is the median
of the numeric ratings from the models that returned a valid label.
"""

from __future__ import annotations

import argparse
import asyncio
import json
import os
import random
import shutil
import threading
from datetime import datetime, timezone
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

# Treatment arms only. The winter control chat is not rated.
TREATMENT_CONDITIONS = ["meat", "charity", "lying", "polviol"]

# ---------------------------------------------------------------------------
# OpenRouter defaults
# ---------------------------------------------------------------------------
# Claude Opus 5.5, GPT-5.5, and Grok 4.7. The transcript plus codebook is
# long, so each call runs at high reasoning effort.

OPENROUTER_BASE_URL = "https://openrouter.ai/api/v1"
MODELS = [
    "anthropic/claude-opus-5.5",
    "openai/gpt-5.5",
    "x-ai/grok-4.7",
]
MODEL_NAMES = ["claude", "gpt", "grok"]

# Reasoning tokens count against max_tokens. High effort uses about 80%
# of that budget, and Anthropic requires max_tokens above the thinking budget.
MAX_TOKENS = 16000
REASONING = {"effort": "high", "exclude": True}

MAX_RETRIES = 5
BASE_DELAY = 1.0
MAX_DELAY = 60.0
SEMAPHORE_LIMIT = 10

# Filled by each successful call. Used to price a run from real usage.
_CALL_USAGE: list[dict[str, Any]] = []
# One JSON line per provider call: the request that was sent and the response
# that came back. Set in run_classifier, next to the ratings CSV.
_CALL_LOG: Path | None = None
_CALL_LOG_LOCK = threading.Lock()

RATING_SCALE = ["none", "low", "moderate", "high"]
RATING_TO_INT = {"none": 0, "low": 1, "moderate": 2, "high": 3}

# ---------------------------------------------------------------------------
# Study 2 conversations from the wide CSV
# ---------------------------------------------------------------------------


def _cell(value: Any) -> str | None:
    if value is None or pd.isna(value):
        return None
    text = str(value).strip()
    if not text:
        return None
    return text


def _format_conversation(row: pd.Series) -> dict[str, Any]:
    """Study 1 layout: initial reasoning, then AI responses and replies.

    system1 is the model instruction, not part of the rated transcript.
    """
    parts: list[str] = []
    n_ai = 0
    n_user = 0
    reason = _cell(row.get("reasons"))
    if reason:
        parts.extend(["\n=== PARTICIPANT'S INITIAL REASONING ===", reason])
        n_user += 1

    user_i = 0
    for i in range(1, 7):
        ai = _cell(row.get(f"assistant{i}"))
        if ai:
            n_ai += 1
            parts.extend([f"\n=== AI RESPONSE {n_ai} ===", ai])
        if i <= 5:
            user = _cell(row.get(f"user{i}"))
            if user:
                user_i += 1
                n_user += 1
                parts.extend([f"\n=== PARTICIPANT REPLY {user_i} ===", user])

    return {
        "id": str(row["participant_id"]),
        "condition": str(row["condition"]),
        "n_ai_rounds": n_ai,
        "n_user_rounds": n_user,
        "n_total_rounds": n_ai + n_user,
        "full_conversation": "\n".join(parts),
    }


def export_conversations(
    conditions: list[str] | None = None,
    csv_path: Path = CONVERSATIONS_CSV,
) -> pd.DataFrame:
    """Load treatment conversations. Control (winter) chats are excluded."""
    if not csv_path.exists():
        raise FileNotFoundError(
            f"Conversation CSV not found: {csv_path}\n"
            "Run scripts/study_2/01_wrangle_data.R first."
        )
    if conditions and "control" in conditions:
        raise ValueError("Control conversations are not included in this analysis")

    raw = pd.read_csv(csv_path)
    needed = ["participant_id", "condition", "reasons", "assistant1", "user1"]
    missing = [c for c in needed if c not in raw.columns]
    if missing:
        raise RuntimeError(
            f"{csv_path.name} is missing columns: {', '.join(missing)}"
        )

    keep = TREATMENT_CONDITIONS if not conditions else conditions
    raw = raw[raw["condition"].astype(str).isin(keep)]
    if raw.empty:
        requested = ", ".join(keep)
        raise RuntimeError(f"No conversations found (conditions: {requested})")

    return pd.DataFrame([_format_conversation(row) for _, row in raw.iterrows()])


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
# Rating
# ---------------------------------------------------------------------------


def _normalize_rating(val: Any) -> str | None:
    if val is None:
        return None
    val_lower = str(val).lower().strip()
    if val_lower in RATING_SCALE:
        return val_lower
    if val_lower in ["0", "false", "no"]:
        return "none"
    if val_lower in ["1", "true", "yes"]:
        return "low"
    return None


def _parse_rating_json(content: str | None) -> dict[str, Any]:
    """Parse the rating object. Reasoning models sometimes wrap it in fences."""
    if content is None or not str(content).strip():
        raise json.JSONDecodeError("Empty completion", "", 0)
    text = str(content).strip()
    if text.startswith("```"):
        text = text.split("\n", 1)[-1]
        text = text.removesuffix("```").strip()
    return json.loads(text)


def _as_int(value: Any) -> int | None:
    if value is None or pd.isna(value):
        return None
    return int(value)


def _append_call_log(record: dict[str, Any]) -> None:
    if _CALL_LOG is None:
        return
    _CALL_LOG.parent.mkdir(parents=True, exist_ok=True)
    line = json.dumps(record, default=str) + "\n"
    with _CALL_LOG_LOCK:
        with open(_CALL_LOG, "a") as f:
            f.write(line)


def _call_record(
    *,
    text_id: str,
    condition: str,
    model: str,
    attempt: int,
    request: dict[str, Any],
    response: Any = None,
    error: str | None = None,
) -> dict[str, Any]:
    raw = None
    if response is not None and hasattr(response, "model_dump"):
        raw = response.model_dump()
    return {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "id": text_id,
        "condition": condition,
        "model": model,
        "attempt": attempt,
        "request": request,
        "response": raw,
        "error": error,
    }


async def classify_with_retry(
    client: AsyncOpenAI,
    model: str,
    full_conversation: str,
    text_id: str,
    condition: str,
    claim: str,
    semaphore: asyncio.Semaphore,
    categories: list[str],
    system_prompt: str,
    user_prompt_template: str,
) -> dict[str, Any]:
    """Rate one conversation with one model; retry with exponential backoff."""
    empty = {cat: None for cat in categories}
    empty["reasoning"] = ""
    empty["error"] = None

    async with semaphore:
        for attempt in range(MAX_RETRIES):
            request: dict[str, Any] | None = None
            try:
                # Replace placeholders directly so braces in the conversation
                # are not treated as format fields.
                prompt = (
                    user_prompt_template.replace("{condition}", condition)
                    .replace("{claim}", claim)
                    .replace("{full_conversation}", full_conversation)
                )
                # The system prompt names only this conversation's claim.
                system = system_prompt.replace("{claim}", claim)
                request = {
                    "model": model,
                    "messages": [
                        {"role": "system", "content": system},
                        {"role": "user", "content": prompt},
                    ],
                    "max_tokens": MAX_TOKENS,
                    "response_format": {"type": "json_object"},
                    "reasoning": REASONING,
                    "provider": {"require_parameters": True},
                }
                response = await client.chat.completions.create(
                    model=request["model"],
                    messages=request["messages"],
                    max_tokens=request["max_tokens"],
                    response_format=request["response_format"],
                    extra_body={
                        "reasoning": request["reasoning"],
                        "provider": request["provider"],
                    },
                )
                _append_call_log(
                    _call_record(
                        text_id=text_id,
                        condition=condition,
                        model=model,
                        attempt=attempt + 1,
                        request=request,
                        response=response,
                    )
                )
                content = response.choices[0].message.content
                parsed = _parse_rating_json(content)
                _record_usage(model, response, len(full_conversation))
                return {
                    **{cat: _normalize_rating(parsed.get(cat)) for cat in categories},
                    "reasoning": parsed.get("reasoning", ""),
                    "error": None,
                }
            except json.JSONDecodeError as e:
                if attempt == MAX_RETRIES - 1:
                    return {**empty, "error": f"JSON decode error: {e}"}
            except Exception as e:
                _append_call_log(
                    _call_record(
                        text_id=text_id,
                        condition=condition,
                        model=model,
                        attempt=attempt + 1,
                        request=request
                        or {
                            "model": model,
                            "max_tokens": MAX_TOKENS,
                            "response_format": {"type": "json_object"},
                            "reasoning": REASONING,
                            "provider": {"require_parameters": True},
                        },
                        error=str(e),
                    )
                )
                if attempt == MAX_RETRIES - 1:
                    return {**empty, "error": str(e)}
                delay = min(
                    BASE_DELAY * (2**attempt) + random.uniform(0, 1),
                    MAX_DELAY,
                )
                await asyncio.sleep(delay)

    return {**empty, "error": "Max retries exceeded"}


def _record_usage(model: str, response: Any, n_chars: int) -> None:
    raw = response.model_dump() if hasattr(response, "model_dump") else {}
    usage = raw.get("usage") or {}
    if not isinstance(usage, dict):
        usage = {}
    _CALL_USAGE.append(
        {
            "model": model,
            "n_chars": n_chars,
            "prompt_tokens": usage.get("prompt_tokens"),
            "completion_tokens": usage.get("completion_tokens"),
            "total_tokens": usage.get("total_tokens"),
            "cost": usage.get("cost"),
            "completion_tokens_details": usage.get("completion_tokens_details"),
            "prompt_tokens_details": usage.get("prompt_tokens_details"),
        }
    )


def _print_usage() -> None:
    if not _CALL_USAGE:
        print("\nNo usage records.")
        return
    print("\n" + "=" * 60)
    print("API USAGE")
    print("=" * 60)
    by_model: dict[str, dict[str, float]] = {}
    for row in _CALL_USAGE:
        bucket = by_model.setdefault(
            row["model"],
            {"calls": 0, "prompt_tokens": 0, "completion_tokens": 0, "cost": 0.0},
        )
        bucket["calls"] += 1
        bucket["prompt_tokens"] += row.get("prompt_tokens") or 0
        bucket["completion_tokens"] += row.get("completion_tokens") or 0
        bucket["cost"] += row.get("cost") or 0.0
    total = 0.0
    for model, bucket in by_model.items():
        total += bucket["cost"]
        print(
            f"  {model}: {bucket['calls']} calls, "
            f"prompt={bucket['prompt_tokens']:,.0f}, "
            f"completion={bucket['completion_tokens']:,.0f}, "
            f"cost=${bucket['cost']:.4f}"
        )
    print(f"  Total: ${total:.4f}")
    print("=" * 60 + "\n")


def _consensus_rating(ratings: list[str | None]) -> tuple[str | None, int | None]:
    """Median of valid ordinal ratings, matching the Study 1 rater."""
    valid = [r for r in ratings if r is not None and r in RATING_SCALE]
    if len(valid) == 0:
        return None, None
    if len(valid) == 1:
        return valid[0], RATING_TO_INT[valid[0]]
    numeric = sorted(RATING_TO_INT[r] for r in valid)
    median_numeric = numeric[len(numeric) // 2]
    int_to_rating = {v: k for k, v in RATING_TO_INT.items()}
    return int_to_rating[median_numeric], median_numeric


async def classify_text_all_models(
    client: AsyncOpenAI,
    full_conversation: str,
    text_id: str,
    condition: str,
    claim: str,
    metadata: dict[str, Any],
    semaphores: dict[str, asyncio.Semaphore],
    categories: list[str],
    system_prompt: str,
    user_prompt_template: str,
) -> dict[str, Any]:
    """Rate one conversation with all three models and take the median."""
    results = await asyncio.gather(
        *[
            classify_with_retry(
                client,
                model,
                full_conversation,
                text_id,
                condition,
                claim,
                semaphores[model],
                categories,
                system_prompt,
                user_prompt_template,
            )
            for model in MODELS
        ]
    )

    preview = full_conversation
    if len(preview) > 500:
        preview = preview[:500] + "..."

    out: dict[str, Any] = {
        "id": text_id,
        "condition": condition,
        "n_chars": len(full_conversation),
        "n_ai_rounds": metadata.get("n_ai_rounds"),
        "n_user_rounds": metadata.get("n_user_rounds"),
        "n_total_rounds": metadata.get("n_total_rounds"),
        "conversation_preview": preview,
    }
    for model_name, model_result in zip(MODEL_NAMES, results):
        for category in categories:
            out[f"{category}_{model_name}"] = model_result.get(category)
        out[f"reasoning_{model_name}"] = model_result.get("reasoning", "")
        out[f"error_{model_name}"] = model_result.get("error")

    for category in categories:
        label, numeric = _consensus_rating(
            [out.get(f"{category}_{m}") for m in MODEL_NAMES]
        )
        out[f"{category}_consensus"] = label
        out[f"{category}_consensus_numeric"] = numeric
    return out


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
        help="Rate only the first N conversations (smoke test)",
    )
    parser.add_argument(
        "--condition",
        action="append",
        default=None,
        help=(
            "Treatment condition to include (repeatable). "
            "Default: meat, charity, lying, polviol. Control is excluded."
        ),
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


def _print_length_stats(df: pd.DataFrame) -> None:
    lengths = df["full_conversation"].fillna("").str.len()
    print("\n" + "=" * 60)
    print("CONVERSATION LENGTH STATISTICS")
    print("=" * 60)
    print("\nCharacter counts:")
    print(
        f"  min={lengths.min()}, max={lengths.max()}, "
        f"mean={lengths.mean():.0f}, median={lengths.median():.0f}"
    )
    if "n_ai_rounds" in df.columns:
        print("\nAI rounds per conversation:")
        print(
            f"  min={df['n_ai_rounds'].min()}, max={df['n_ai_rounds'].max()}, "
            f"mean={df['n_ai_rounds'].mean():.1f}"
        )
        print(
            "  Distribution: "
            f"{df['n_ai_rounds'].value_counts().sort_index().to_dict()}"
        )
        print("\nUser rounds per conversation:")
        print(
            f"  min={df['n_user_rounds'].min()}, max={df['n_user_rounds'].max()}, "
            f"mean={df['n_user_rounds'].mean():.1f}"
        )
        print("\nTotal turns per conversation:")
        print(
            f"  min={df['n_total_rounds'].min()}, max={df['n_total_rounds'].max()}, "
            f"mean={df['n_total_rounds'].mean():.1f}"
        )
    estimated = lengths / 4
    print("\nEstimated tokens (chars/4):")
    print(
        f"  min={estimated.min():.0f}, max={estimated.max():.0f}, "
        f"mean={estimated.mean():.0f}"
    )
    n_long = int((lengths > 30000).sum())
    if n_long:
        print(f"\nWarning: {n_long} conversations exceed 30,000 characters")
        print(f"  Longest: {int(lengths.max())} chars (~{int(lengths.max()) // 4} tokens)")
    print("=" * 60 + "\n")


def _print_summary(
    df: pd.DataFrame,
    categories: list[str],
    strategy_labels: dict[str, str],
) -> None:
    print("\n" + "=" * 60)
    print("PERSUASION STRATEGIES CLASSIFICATION SUMMARY")
    print("=" * 60)

    def print_rating_distribution(sub: pd.DataFrame, category: str, label: str) -> None:
        consensus_col = f"{category}_consensus"
        if consensus_col not in sub.columns or len(sub) == 0:
            return
        counts = sub[consensus_col].value_counts()
        n = len(sub)
        dist = ", ".join(
            f"{rating}: {int(counts.get(rating, 0))} ({100 * counts.get(rating, 0) / n:.0f}%)"
            for rating in RATING_SCALE
        )
        numeric_col = f"{category}_consensus_numeric"
        if numeric_col in sub.columns:
            mean_rating = sub[numeric_col].mean()
            print(f"  {label}: mean={mean_rating:.2f} | {dist}")
        else:
            print(f"  {label}: {dist}")

    conditions = list(dict.fromkeys(df["condition"].astype(str)))
    for condition in conditions:
        cond_df = df[df["condition"].astype(str) == condition]
        print(f"\n--- {condition} (n={len(cond_df)}) ---")
        for category in categories:
            print_rating_distribution(cond_df, category, strategy_labels[category])

    print(f"\n--- Overall (n={len(df)}) ---")
    for category in categories:
        print_rating_distribution(df, category, strategy_labels[category])

    print("\n--- Inter-Model Agreement ---")
    for category in categories:
        exact = 0
        within_one = 0
        total = 0
        for _, row in df.iterrows():
            ratings = [row.get(f"{category}_{m}") for m in MODEL_NAMES]
            valid = [r for r in ratings if isinstance(r, str) and r in RATING_SCALE]
            if len(valid) >= 2:
                total += 1
                if all(r == valid[0] for r in valid):
                    exact += 1
                numeric = [RATING_TO_INT[r] for r in valid]
                if max(numeric) - min(numeric) <= 1:
                    within_one += 1
        if total > 0:
            print(
                f"  {strategy_labels[category]}: "
                f"{100 * exact / total:.1f}% exact, "
                f"{100 * within_one / total:.1f}% within-1"
            )


async def run_classifier(
    *,
    categories: list[str],
    strategy_labels: dict[str, str],
    claims: dict[str, str],
    system_prompt: str,
    user_prompt_template: str,
    output_file: Path,
    checkpoint_file: Path,
    progress_label: str,
    limit: int | None = None,
    conditions: list[str] | None = None,
) -> None:
    """End-to-end: export conversations → rate strategies → write CSV."""
    global _CALL_LOG
    _CALL_USAGE.clear()
    _CALL_LOG = output_file.with_name(output_file.stem + "_calls.jsonl")
    api_key = os.environ.get("OPENROUTER_API_KEY")
    if not api_key:
        raise SystemExit("Error: OPENROUTER_API_KEY environment variable not set")

    unknown = [c for c in (conditions or []) if c not in claims]
    if unknown:
        known = ", ".join(claims)
        raise SystemExit(
            f"Error: unknown condition(s): {', '.join(unknown)}. Known: {known}"
        )

    label = ", ".join(conditions) if conditions else ", ".join(TREATMENT_CONDITIONS)
    print(f"Loading Study 2 conversations from {CONVERSATIONS_CSV.name} ({label})...")
    df = export_conversations(conditions=conditions)
    print(f"Found {len(df)} conversations")

    df["full_conversation"] = df["full_conversation"].fillna("")
    _print_length_stats(df)

    if limit is not None:
        df = df.head(limit)
        print(f"Limiting to first {len(df)} conversations (--limit {limit})")

    processed_ids = load_checkpoint(checkpoint_file)
    if processed_ids:
        print(f"Resuming from checkpoint: {len(processed_ids)} already processed")

    df_todo = df[~df["id"].astype(str).isin(processed_ids)]
    print(f"Processing {len(df_todo)} remaining conversations")

    if len(df_todo) == 0:
        print("All samples already processed!")
        if output_file.exists():
            _print_summary(pd.read_csv(output_file), categories, strategy_labels)
        return

    client = AsyncOpenAI(api_key=api_key, base_url=OPENROUTER_BASE_URL)
    semaphores = {model: asyncio.Semaphore(SEMAPHORE_LIMIT) for model in MODELS}

    tasks = [
        classify_text_all_models(
            client,
            full_conversation=str(row["full_conversation"]),
            text_id=str(row["id"]),
            condition=str(row["condition"]),
            claim=claims[str(row["condition"])],
            metadata={
                "n_ai_rounds": _as_int(row["n_ai_rounds"]),
                "n_user_rounds": _as_int(row["n_user_rounds"]),
                "n_total_rounds": _as_int(row["n_total_rounds"]),
            },
            semaphores=semaphores,
            categories=categories,
            system_prompt=system_prompt,
            user_prompt_template=user_prompt_template,
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
        existing = existing[~existing["id"].astype(str).isin(results_df["id"].astype(str))]
        combined = pd.concat([existing, results_df], ignore_index=True)
    else:
        combined = results_df

    output_file.parent.mkdir(parents=True, exist_ok=True)
    combined.to_csv(output_file, index=False)
    print(f"\nResults saved to {output_file}")
    print(f"Call log saved to {_CALL_LOG}")
    _print_usage()
    _print_summary(combined, categories, strategy_labels)
