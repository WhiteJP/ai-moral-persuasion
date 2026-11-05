"""
Build the strategy-rating file used by the paper pipeline.

Reads classifications_persuasion_strategies.csv (Claude Opus 5.5, GPT-5.5,
and Grok 4.7) and writes id, condition, then for each strategy the three
ratings in that order. No conversation preview.

Without --run the script does not write. With --run it writes
output/study_2/text_analysis/strategy_ratings.csv. If that path is the same
file as the downloaded copy, it writes strategy_ratings_rerun.csv instead
unless --write-data is also set.

Usage:
    python scripts/text_analysis/arguments/write_strategy_ratings.py --run
    python scripts/text_analysis/arguments/write_strategy_ratings.py --run --write-data
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIR))

from classifier import install_rating_files, regeneration_requested  # noqa: E402

REPO_ROOT = SCRIPT_DIR.parents[2]
SRC = REPO_ROOT / "output" / "study_2" / "text_analysis"
DEST = REPO_ROOT / "data" / "study_2" / "text_ratings"

STRATEGIES = [
    "evidence_facts",
    "logical_consistency",
    "moral_principles",
    "emotional_appeal",
    "analogies_reframing",
]
MODELS = ["claude", "gpt", "grok"]
COLUMNS = ["id", "condition"] + [f"{s}_{m}" for s in STRATEGIES for m in MODELS]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--run",
        action="store_true",
        help="Write strategy_ratings.csv from the three-model ratings.",
    )
    parser.add_argument(
        "--write-data",
        action="store_true",
        help="Also copy the file into data/study_2/text_ratings/.",
    )
    args = parser.parse_args()
    if not regeneration_requested(args):
        return

    wide = SRC / "classifications_persuasion_strategies.csv"
    out = pd.read_csv(wide, usecols=COLUMNS)

    SRC.mkdir(parents=True, exist_ok=True)
    path = SRC / "strategy_ratings.csv"
    deposited = DEST / "strategy_ratings.csv"
    if (
        not args.write_data
        and deposited.exists()
        and deposited.resolve() == path.resolve()
    ):
        path = SRC / "strategy_ratings_rerun.csv"
        print(
            "data/study_2/text_ratings/strategy_ratings.csv points at the "
            "output file, so this run will not overwrite it. Pass --write-data "
            "to replace the deposited file."
        )
    out.to_csv(path, index=False)
    print(f"Wrote {path} ({len(out)} conversations)")
    if args.write_data:
        install_rating_files([path])


if __name__ == "__main__":
    main()
