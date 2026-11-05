"""
Classify Study 2 lying-believer reasons (cond_belief_pre >= 50).

Six multi-label reasons why financial deception is morally wrong:
  Harm, Broken trust, Unfair advantage, Absolute prohibition,
  Moral character, Religious duty

Uses three OpenRouter models with majority-vote consensus.

Usage:
    export OPENROUTER_API_KEY=...
    python scripts/text_analysis/reasons/classify_lying.py --run
    python scripts/text_analysis/reasons/classify_lying.py --run --limit 5
"""

from __future__ import annotations

import asyncio

from classifier import (
    OUTPUT_DIR,
    SCRIPT_DIR,
    build_arg_parser,
    install_rating_files,
    regeneration_requested,
    run_classifier,
)

CATEGORIES = [
    "harm",
    "broken_trust",
    "unfair_advantage",
    "absolute_prohibition",
    "moral_character",
    "religious_duty",
]

SYSTEM_PROMPT = """You are applying a fixed codebook to open-ended survey responses.

Each participant in a psychological study rated their agreement with the statement "Deceiving others for your own financial gain is morally wrong" (0–100) and then explained their view in their own words. The texts you will see come from respondents who accepted that statement (score ≥ 50). Your task is to record which reasons they give for why this kind of deception is morally wrong.

General coding rules:
- Code the reason given, not the respondent's overall stance. Judging deception wrong is not itself a code.
- A category is present only when that specific type of reason is stated or clearly implied.
- Code the idea, not particular words. A paraphrase counts. A matching word without that idea does not.
- The six categories are independent and multi-label. A response may contain several, one, or none. Mark each true or false on its own.
- If the text merely restates that deception is wrong, without a further reason that fits a category, all six should be false — unless they also say it is always, never, or in general wrong (ABSOLUTE_PROHIBITION).
- Frequency hedges ("typically", "generally", "usually", "most of the time") and other hedges ("except to survive", "it depends") can sit alongside other categories. They do not get their own code. They do block ABSOLUTE_PROHIBITION. "In general" and "as a general rule" are not frequency hedges.
- These categories capture reasons why deception is morally wrong. Do not apply them to defenses of lying, arguments that deception is acceptable, or claims that people ought to deceive. If a response mixes both, code only the why-wrong parts.

1. HARM
   Definition: It is wrong because someone is made worse off. The harmed party may be the deceived person or the liar later.
   Not this: Unkind, using people, or "take care of one another" with no worse-off outcome. Shame, greed, or "the kind of person you are" with no worse-off claim (MORAL_CHARACTER). "They worked for that money" / "this is theft" with no claim that anyone is worse off (UNFAIR_ADVANTAGE).
   Examples: "It harms them both financially and emotionally." "Money gained through ill means will haunt you." "They get caught, and it is bad karma."

2. BROKEN_TRUST
   Definition: It is wrong because it breaks trust or makes honesty and cooperation impossible. The point is the relationship or the practice of telling the truth, not the dollar loss.
   Not this: "Deceiving others is wrong, period" with no appeal to trust (ABSOLUTE_PROHIBITION). Harm or unfairness with no trust claim.
   Examples: "It undermines trust, which is the foundation of a healthy relationship." "Honesty is essential for trust."

3. UNFAIR_ADVANTAGE
   Definition: It is wrong because the gain is unearned. Do not infer this from the mere fact of gaining money by deception. Require an explicit claim that the money was stolen, unearned, undeserved, or not made fairly.
   Not this: "Taking advantage," "enriching yourself," "gain off of others," "get ahead," or "using them" with no desert, theft, or unearned-gain claim. A secular Golden Rule / "treat others fairly" with no claim that the money was unearned (often HARM). If they also say the other person is worse off, also mark HARM.
   Examples: "I view this the same as theft." "People work hard for their own money." "Money should be made honestly and fairly."

4. ABSOLUTE_PROHIBITION
   Definition: Deception is forbidden as a general rule. Always, never, period, "in general," or "as a general rule" count. "The money just makes it worse" still counts only if they mean the deception itself is already forbidden as a rule, not merely bad. A narrow consented exception (for example a magic show) still counts.
   Not this: Strong disapproval with no universal claim. "Lying is wrong" or "deception is wrong" without always, never, period, or in general. Frequency hedges — typically, generally, usually, most of the time — block this category. Survival hedges or "it depends" also block it. Restating the survey item is not enough.
   Examples: "Wrong is wrong. It doesn't matter how big or small." "Deceiving others is always wrong. The financial gain isn't particularly relevant." "Lying in general is wrong."

5. MORAL_CHARACTER
   Definition: It is wrong because of the kind of person it makes you. A trait or kind-of-person claim must be present: selfish, greedy, cruel, lacking empathy, sneaky, or similar.
   Not this: Bad outcomes for the liar later (HARM). "Wrong is wrong" with no character claim (ABSOLUTE_PROHIBITION). A secular Golden Rule. "Honesty is the best policy." "Using people" with no vice or trait.
   Examples: "It says a lot about the character of who you are." "It's an indication of greed." "It's cruel and selfish." "They are severely lacking in empathy."

6. RELIGIOUS_DUTY
   Definition: It is wrong because God, scripture, or faith forbids it.
   Not this: A secular "treat others as you want to be treated" with no religious source (usually HARM).
   Examples: "It goes against all Christian values." "The Bible says that we must treat our neighbors well." """

USER_PROMPT_TEMPLATE = """Apply the codebook to the response below.

Reminder: The six categories are independent and multi-label. A response may contain several, one, or none. Mark each true or false on its own.

Response:
"{text}"

Return only a JSON object of the following form:
{{
  "harm": true/false,
  "broken_trust": true/false,
  "unfair_advantage": true/false,
  "absolute_prohibition": true/false,
  "moral_character": true/false,
  "religious_duty": true/false,
  "reasoning": "brief justification"
}}"""


def main() -> None:
    parser = build_arg_parser(
        "Classify Study 2 lying-believer reasons (6 why-wrong types)"
    )
    args = parser.parse_args()
    if not regeneration_requested(args):
        return

    output_file = OUTPUT_DIR / "classifications_lying.csv"
    asyncio.run(
        run_classifier(
            condition="lying",
            categories=CATEGORIES,
            system_prompt=SYSTEM_PROMPT,
            user_prompt_template=USER_PROMPT_TEMPLATE,
            output_file=output_file,
            checkpoint_file=SCRIPT_DIR / "checkpoint_lying.json",
            progress_label="Classifying lying reasons",
            limit=args.limit,
            belief_op=">=",
        )
    )
    if args.write_data:
        install_rating_files([output_file])


if __name__ == "__main__":
    main()
