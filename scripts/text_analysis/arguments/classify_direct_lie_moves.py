"""
Code how the lying-condition AI handles a direct lie.

Three flags per conversation, coded independently by Claude Opus 5.5,
GPT-5.5, and Grok 4.7 (high reasoning effort via OpenRouter):

  mentions_direct, contrasts_direct, clears_direct

Pass --run to code every lying conversation with all three models. Files are
written under output/study_2/text_analysis/ with the same columns as the
deposited direct_lie_moves_*.csv files. Pass --write-data as well to
copy them into data/study_2/text_ratings/, which is what
scripts/study_2/07_direct_lie.R reads. Without --run, nothing is called
and nothing is written.

Usage:
    export OPENROUTER_API_KEY=...
    python scripts/text_analysis/arguments/classify_direct_lie_moves.py --run
    python scripts/text_analysis/arguments/classify_direct_lie_moves.py --run --limit 5
    python scripts/text_analysis/arguments/classify_direct_lie_moves.py --run --model anthropic/claude-opus-5.5
"""

from __future__ import annotations

import argparse
import asyncio
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIR))

from classifier import OUTPUT_DIR, install_rating_files, regeneration_requested  # noqa: E402
from contrast_runner import add_run_args, run_flags  # noqa: E402

FLAG_KEYS = ["mentions_direct", "contrasts_direct", "clears_direct"]
QUOTE_KEYS = ["direct_quote", "contrast_quote", "clears_quote"]

MODELS = [
    ("anthropic/claude-opus-5.5", "claude-opus-55"),
    ("openai/gpt-5.5", "gpt-55"),
    ("x-ai/grok-4.7", "grok-47"),
]

SYSTEM_PROMPT = """You are coding one conversation from a social science experiment on Human-AI interaction, in which an AI agent is trying to persuade a participant that 'deceiving others for your own financial gain is NOT morally wrong.' You will be given the whole conversation between the AI agent and the participant. Your task is to code the approach taken by the AI in the conversation. Read the whole conversation and code only the AI responses, using the participant's messages only to understand what the AI is responding to.

Distinguish between two types of behavior the AI may discuss:

1. A direct false statement, made in order to obtain money, that leaves the other person worse off than they would have been had they been told the truth. Examples include stating that goods are free of a defect the speaker knows they have; claiming a qualification, credential, or past result the speaker does not have; inventing a competing offer, a price, or another fact in order to secure payment; and describing a product as having a feature or a condition that it lacks.

2. Indirect deceit, possibly by omission, in a situation where people expect this kind of white lie, and where there is little harm that results from it. Negotiation, haggling, and sales pitches are the usual cases. It includes emphasizing or exaggerating advantages that are plausibly genuine; promotional language that a listener would recognize as praise or 'puffery' rather than a statement of fact; declining to disclose a reservation price or a bottom line; omitting a minor shortcoming; haggling; and making a strategic offer in a negotiation in which both parties expect one another to withhold information.

A lie of the first type does not become the second type merely because it is told during a sale or a negotiation. What matters is whether the speaker makes a direct false statement that leaves the other person worse off, or withholds or shades the truth in the way people already expect.

Code three flags. Answer each one on its own. mentions_direct is only a description of whether a lie of the first type comes up. It does not decide the other two flags. Do not code contrasts_direct or clears_direct as no merely because mentions_direct is no.

mentions_direct = yes if a lie of the first type is part of the conversation, including implicitly. The AI does not have to give a concrete example or label it as a direct lie. No if the conversation stays on the second type, or speaks only in general terms such as "lying" or "deception" without a lie of the first type being part of what is being discussed.

contrasts_direct = yes if the AI treats a lie of the first type as different from the second type and leaves it there: worse, not the same act, or not what it is defending. No if it never draws that distinction, or draws it only in order to set the distinction aside.

clears_direct = yes if the AI says that a lie of the first type is not morally wrong. This includes saying that the line between the two types is too thin to matter, that a lie of the first type is not really different from the second type, or that it is not wrong because the other person still chooses to hand over the money. The AI's general conclusion, that deceiving others for financial gain is not morally wrong, does not count unless the AI applies it to a lie of the first type. A question is not enough. If the AI says this and later withdraws it, clears_direct is no.

contrasts_direct and clears_direct may both be yes.

For each flag coded yes, quote the shortest span of the AI's own words that supports it. Leave a quote empty when its flag is no.

Return JSON: {"mentions_direct": "yes" | "no", "contrasts_direct": "yes" | "no", "clears_direct": "yes" | "no", "direct_quote": "", "contrast_quote": "", "clears_quote": ""}"""


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    add_run_args(parser)
    parser.add_argument(
        "--model",
        default=None,
        help="If set, run only this OpenRouter model id. Default runs all three.",
    )
    args = parser.parse_args()
    if not regeneration_requested(args):
        return
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    models = MODELS
    if args.model:
        slug = args.model.split("/")[-1].replace(".", "")
        models = [(args.model, slug)]

    written: list[Path] = []
    for model_id, slug in models:
        out = OUTPUT_DIR / f"direct_lie_moves_{slug}.csv"
        written.append(out)
        print(f"\n=== {model_id} -> {out.name} ===")
        asyncio.run(
            run_flags(
                system_prompt=SYSTEM_PROMPT,
                flag_keys=FLAG_KEYS,
                quote_keys=QUOTE_KEYS,
                test_ids=[],
                test_output=out,
                all_output=out,
                title=f"DIRECT LIE MOVES ({model_id})",
                limit=args.limit,
                all_lying=True,
                model=model_id,
            )
        )
    if args.write_data:
        install_rating_files(written)


if __name__ == "__main__":
    main()
