"""Shared Claude runner for yes/no conversation codes."""

from __future__ import annotations

import asyncio
import os
import random
import sys
from pathlib import Path
from typing import Any

import pandas as pd
from openai import AsyncOpenAI
from tqdm.asyncio import tqdm_asyncio

SCRIPT_DIR = Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIR))

from classifier import (  # noqa: E402
    MAX_DELAY,
    MAX_RETRIES,
    MAX_TOKENS,
    OPENROUTER_BASE_URL,
    OUTPUT_DIR,
    REASONING,
    _parse_rating_json,
    export_conversations,
)

MODEL = "anthropic/claude-opus-5.5"
BASE_DELAY = 1.0
SEMAPHORE_LIMIT = 10

USER_PROMPT_TEMPLATE = """Apply the codebook to the conversation below.

{full_conversation}

=== END OF CONVERSATION ===

Return only the JSON object."""


def _quote(value: Any) -> str:
    if value is None:
        return ""
    return str(value).strip()


async def run_coder(
    *,
    system_prompt: str,
    code_key: str,
    quote_keys: list[str],
    test_ids: list[str],
    test_output: Path,
    all_output: Path,
    title: str,
    limit: int | None,
    all_lying: bool,
) -> None:
    api_key = os.environ.get("OPENROUTER_API_KEY")
    if not api_key:
        raise SystemExit("Error: OPENROUTER_API_KEY environment variable not set")

    print("Loading lying conversations...")
    df = export_conversations(conditions=["lying"])
    if not all_lying:
        wanted = set(test_ids)
        df = df[df["id"].astype(str).isin(wanted)].copy()
        missing = wanted - set(df["id"].astype(str))
        if missing:
            raise SystemExit(f"Test ids missing from the CSV: {sorted(missing)}")
        df["_order"] = df["id"].map({pid: i for i, pid in enumerate(test_ids)})
        df = df.sort_values("_order").drop(columns="_order")
        print(f"Test set: {len(df)} conversations")
    else:
        print(f"All lying conversations: {len(df)}")

    if limit is not None:
        df = df.head(limit)
        print(f"Limiting to {len(df)}")

    output_file = all_output if all_lying else test_output
    done = _load_done(output_file, code_key)
    if done:
        print(f"Resuming: {len(done)} already coded in {output_file.name}")

    df_todo = df[~df["id"].astype(str).isin(done)]
    print(f"Coding {len(df_todo)} conversations")

    client = AsyncOpenAI(api_key=api_key, base_url=OPENROUTER_BASE_URL)
    semaphore = asyncio.Semaphore(SEMAPHORE_LIMIT)
    tasks = [
        _code_one(client, semaphore, row, system_prompt, code_key, quote_keys)
        for _, row in df_todo.iterrows()
    ]
    results: list[dict[str, Any]] = list(done.values())
    order = {pid: i for i, pid in enumerate(df["id"].astype(str))}
    if tasks:
        finished = 0
        for coro in tqdm_asyncio.as_completed(tasks, total=len(tasks), desc=title):
            results.append(await coro)
            finished += 1
            if finished % 20 == 0:
                _write_results(output_file, results, order)
    out = _write_results(output_file, results, order)
    print(f"\nResults saved to {output_file}")
    _print_results(out, code_key, quote_keys, title)


def _load_done(path: Path, code_key: str) -> dict[str, dict[str, Any]]:
    if not path.exists():
        return {}
    done: dict[str, dict[str, Any]] = {}
    existing = pd.read_csv(path)
    for rec in existing.to_dict(orient="records"):
        err = rec.get("error")
        if pd.isna(err) or str(err).strip() in {"", "nan"}:
            rec["error"] = None
        if rec.get("error"):
            continue
        if str(rec.get(code_key, "")).lower() not in {"yes", "no"}:
            continue
        done[str(rec["id"])] = rec
    return done


async def _code_one(
    client: AsyncOpenAI,
    semaphore: asyncio.Semaphore,
    row: pd.Series,
    system_prompt: str,
    code_key: str,
    quote_keys: list[str],
) -> dict[str, Any]:
    text_id = str(row["id"])
    conversation = str(row["full_conversation"])
    empty: dict[str, Any] = {
        "id": text_id,
        "n_chars": len(conversation),
        "n_ai_rounds": row["n_ai_rounds"],
        code_key: None,
        "error": None,
    }
    for key in quote_keys:
        empty[key] = ""
    prompt = USER_PROMPT_TEMPLATE.replace("{full_conversation}", conversation)

    async with semaphore:
        for attempt in range(MAX_RETRIES):
            try:
                response = await client.chat.completions.create(
                    model=MODEL,
                    messages=[
                        {"role": "system", "content": system_prompt},
                        {"role": "user", "content": prompt},
                    ],
                    max_tokens=MAX_TOKENS,
                    response_format={"type": "json_object"},
                    extra_body={
                        "reasoning": REASONING,
                        "provider": {"require_parameters": True},
                    },
                )
                parsed = _parse_rating_json(response.choices[0].message.content)
                raw = str(parsed.get(code_key, "")).lower().strip()
                if raw not in {"yes", "no"}:
                    raise ValueError(f"{code_key} must be yes or no, got {raw!r}")
                coded = {code_key: raw, "error": None}
                for key in quote_keys:
                    coded[key] = _quote(parsed.get(key))
                return {**empty, **coded}
            except Exception as e:
                if attempt == MAX_RETRIES - 1:
                    return {**empty, "error": str(e)}
                delay = min(BASE_DELAY * (2**attempt) + random.uniform(0, 1), MAX_DELAY)
                await asyncio.sleep(delay)
    return {**empty, "error": "Max retries exceeded"}


def _write_results(path: Path, results: list[dict[str, Any]], order: dict[str, int]) -> pd.DataFrame:
    results.sort(key=lambda r: order.get(str(r["id"]), 10**9))
    out = pd.DataFrame(results)
    path.parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(path, index=False)
    return out


def _print_results(df: pd.DataFrame, code_key: str, quote_keys: list[str], title: str) -> None:
    print("\n" + "=" * 60)
    print(title)
    print("=" * 60)
    n = len(df)
    n_yes = int((df[code_key] == "yes").sum())
    n_err = int(df["error"].fillna("").astype(str).str.len().gt(0).sum())
    print(f"n={n}  yes={n_yes}  no={n - n_yes - n_err}  errors={n_err}")
    if n <= 15:
        for _, row in df.iterrows():
            print(f"\n{str(row['id'])[:8]}  {row[code_key]}")
            if row.get("error"):
                print(f"  error: {row['error']}")
                continue
            for key in quote_keys:
                if row.get(key):
                    print(f"  {key}: {row[key]}")
    print("=" * 60 + "\n")


async def run_flags(
    *,
    system_prompt: str,
    flag_keys: list[str],
    quote_keys: list[str],
    test_ids: list[str],
    test_output: Path,
    all_output: Path,
    title: str,
    limit: int | None,
    all_lying: bool,
    model: str | None = None,
) -> None:
    """One model call per conversation, with several yes/no flags."""
    api_key = os.environ.get("OPENROUTER_API_KEY")
    if not api_key:
        raise SystemExit("Error: OPENROUTER_API_KEY environment variable not set")

    print("Loading lying conversations...")
    df = export_conversations(conditions=["lying"])
    if not all_lying:
        wanted = set(test_ids)
        df = df[df["id"].astype(str).isin(wanted)].copy()
        missing = wanted - set(df["id"].astype(str))
        if missing:
            raise SystemExit(f"Test ids missing from the CSV: {sorted(missing)}")
        df["_order"] = df["id"].map({pid: i for i, pid in enumerate(test_ids)})
        df = df.sort_values("_order").drop(columns="_order")
        print(f"Test set: {len(df)} conversations")
    else:
        print(f"All lying conversations: {len(df)}")

    if limit is not None:
        df = df.head(limit)
        print(f"Limiting to {len(df)}")

    output_file = all_output if all_lying else test_output
    done = _load_flags_done(output_file, flag_keys)
    if done:
        print(f"Resuming: {len(done)} already coded in {output_file.name}")

    df_todo = df[~df["id"].astype(str).isin(done)]
    print(f"Coding {len(df_todo)} conversations")

    client = AsyncOpenAI(api_key=api_key, base_url=OPENROUTER_BASE_URL)
    semaphore = asyncio.Semaphore(SEMAPHORE_LIMIT)
    model_id = model or MODEL
    print(f"Model: {model_id}")
    tasks = [
        _code_flags(
            client, semaphore, row, system_prompt, flag_keys, quote_keys, model_id
        )
        for _, row in df_todo.iterrows()
    ]
    results: list[dict[str, Any]] = list(done.values())
    order = {pid: i for i, pid in enumerate(df["id"].astype(str))}
    if tasks:
        finished = 0
        for coro in tqdm_asyncio.as_completed(tasks, total=len(tasks), desc=title):
            results.append(await coro)
            finished += 1
            if finished % 20 == 0:
                _write_results(output_file, results, order)
    out = _write_results(output_file, results, order)
    print(f"\nResults saved to {output_file}")
    _print_flags(out, flag_keys, quote_keys, title)


def _load_flags_done(path: Path, flag_keys: list[str]) -> dict[str, dict[str, Any]]:
    if not path.exists():
        return {}
    done: dict[str, dict[str, Any]] = {}
    existing = pd.read_csv(path)
    for rec in existing.to_dict(orient="records"):
        err = rec.get("error")
        if pd.isna(err) or str(err).strip() in {"", "nan"}:
            rec["error"] = None
        if rec.get("error"):
            continue
        if any(str(rec.get(key, "")).lower() not in {"yes", "no"} for key in flag_keys):
            continue
        done[str(rec["id"])] = rec
    return done


async def _code_flags(
    client: AsyncOpenAI,
    semaphore: asyncio.Semaphore,
    row: pd.Series,
    system_prompt: str,
    flag_keys: list[str],
    quote_keys: list[str],
    model: str,
) -> dict[str, Any]:
    text_id = str(row["id"])
    conversation = str(row["full_conversation"])
    empty: dict[str, Any] = {
        "id": text_id,
        "n_chars": len(conversation),
        "n_ai_rounds": row["n_ai_rounds"],
        "error": None,
    }
    for key in flag_keys:
        empty[key] = None
    for key in quote_keys:
        empty[key] = ""
    prompt = USER_PROMPT_TEMPLATE.replace("{full_conversation}", conversation)

    async with semaphore:
        for attempt in range(MAX_RETRIES):
            try:
                response = await client.chat.completions.create(
                    model=model,
                    messages=[
                        {"role": "system", "content": system_prompt},
                        {"role": "user", "content": prompt},
                    ],
                    max_tokens=MAX_TOKENS,
                    response_format={"type": "json_object"},
                    extra_body={
                        "reasoning": REASONING,
                        "provider": {"require_parameters": True},
                    },
                )
                parsed = _parse_rating_json(response.choices[0].message.content)
                coded: dict[str, Any] = {"error": None}
                for key in flag_keys:
                    raw = str(parsed.get(key, "")).lower().strip()
                    if raw not in {"yes", "no"}:
                        raise ValueError(f"{key} must be yes or no, got {raw!r}")
                    coded[key] = raw
                for key in quote_keys:
                    coded[key] = _quote(parsed.get(key))
                return {**empty, **coded}
            except Exception as e:
                if attempt == MAX_RETRIES - 1:
                    return {**empty, "error": str(e)}
                delay = min(BASE_DELAY * (2**attempt) + random.uniform(0, 1), MAX_DELAY)
                await asyncio.sleep(delay)
    return {**empty, "error": "Max retries exceeded"}


def _print_flags(
    df: pd.DataFrame, flag_keys: list[str], quote_keys: list[str], title: str
) -> None:
    print("\n" + "=" * 60)
    print(title)
    print("=" * 60)
    n = len(df)
    n_err = int(df["error"].fillna("").astype(str).str.len().gt(0).sum())
    print(f"n={n}  errors={n_err}")
    for key in flag_keys:
        n_yes = int((df[key] == "yes").sum())
        print(f"  {key}: yes={n_yes}  no={n - n_yes - n_err}")
    if n <= 15:
        for _, row in df.iterrows():
            flags = "  ".join(f"{key}={row[key]}" for key in flag_keys)
            print(f"\n{str(row['id'])[:8]}  {flags}")
            if row.get("error"):
                print(f"  error: {row['error']}")
                continue
            for key in quote_keys:
                if row.get(key):
                    print(f"  {key}: {row[key]}")
    print("=" * 60 + "\n")


def add_run_args(parser: Any) -> None:
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
        "--all-lying",
        action="store_true",
        help="Code every lying conversation. Default is the built-in test set.",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Code only the first N conversations of the selected set.",
    )
