"""
Classify Study 2 political-violence-believer reasons (cond_belief_pre >= 50).

Six multi-label reasons why political violence is morally wrong:
  Harm, Ineffectiveness, Unnecessary force, Illegitimate authority,
  Absolute prohibition, Moral character

Uses three OpenRouter models with majority-vote consensus.

Usage:
    export OPENROUTER_API_KEY=...
    python scripts/text_analysis/reasons/classify_polviol.py --run
    python scripts/text_analysis/reasons/classify_polviol.py --run --limit 5
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
    "ineffectiveness",
    "unnecessary_force",
    "illegitimate_authority",
    "absolute_prohibition",
    "moral_character",
]

SYSTEM_PROMPT = """You are applying a fixed codebook to open-ended survey responses.

Each participant in a psychological study rated their agreement with the statement "In the US today, using violence to advance political goals is morally wrong" (0–100) and then explained their view in their own words. The texts you will see come from respondents who accepted that statement (score ≥ 50). Your task is to record which reasons they give for why political violence is morally wrong.

General coding rules:
- Code the reason given, not the respondent's overall stance. Opposing political violence is not itself a code.
- A category is present only when that specific type of reason is stated or clearly implied.
- Code the idea, not particular words. A paraphrase counts. A matching word without that idea does not.
- The six categories are independent and multi-label. A response may contain several, one, or none. Mark each true or false on its own.
- If the text merely restates that political violence is wrong, without a further reason that fits a category, all six should be false — unless they also say violence is always or never wrong, or only permitted in immediate self-defense of life (ABSOLUTE_PROHIBITION).
- Hedges ("last resort", "extreme situations", "it depends who") can sit alongside other categories. They do not get their own code. They do block ABSOLUTE_PROHIBITION unless the only caveat is immediate self-defense of life.
- These categories capture reasons why political violence is morally wrong. Do not apply them to defenses of political violence, arguments that it is acceptable, or claims that people ought to use it. If a response mixes both, code only the why-wrong parts.

1. HARM
   Definition: It is wrong because people are harmed.
   Not this: It fails or backfires with no claim that people are harmed (INEFFECTIVENESS). Shameful, or the kind of person it makes us (MORAL_CHARACTER).
   Examples: "It can hurt innocent people." "Violence harms families and bystanders."

2. INEFFECTIVENESS
   Definition: It is wrong because violence fails as a means. It does not achieve the political goal, or it makes outcomes worse. This is about whether it works, not about whether other methods exist, and not about who is entitled to decide.
   Not this: Other methods exist, with no claim that violence fails (UNNECESSARY_FORCE). Force is not a legitimate way to decide, with no claim that it fails (ILLEGITIMATE_AUTHORITY).
   Examples: "Political violence just breeds more violence." "It's not going to get you what you want."

3. UNNECESSARY_FORCE
   Definition: It is wrong because violence is not needed. Other methods of change are available. This is about whether alternatives exist, not about whether violence fails, and not about who is entitled to use it.
   Not this: Violence is not a legitimate way to decide, even if it were the only method (ILLEGITIMATE_AUTHORITY). Violence fails in practice (INEFFECTIVENESS). Historical claims that violence was used, or that it founded the country, without saying other methods are available now.
   Examples: "Peaceful methods like voting and protesting are always better." "There are other ways to make change."

4. ILLEGITIMATE_AUTHORITY
   Definition: It is wrong because no one is entitled to settle politics by force, even if it would work and even if it were the only method. The point is who may decide, not whether violence works, and not whether another tactic happens to exist.
   Not this: You could protest instead, with no claim about rightful procedure (UNNECESSARY_FORCE). Violence fails in practice, with no claim about who may decide (INEFFECTIVENESS). That it is illegal, by itself.
   Examples: "Usurping democracy with violence breaks the social contract." "No one has the authority to impose political goals by force."

5. ABSOLUTE_PROHIBITION
   Definition: Violence is forbidden as a general rule, including as a political means. A caveat for immediate self-defense of life still counts.
   Not this: Last resort, extreme cases, "most violence," or "almost always." Strong disapproval, or "it is unethical/illegal," with no always/never (or self-defense-only) claim.
   Examples: "I do not believe any form of violence is acceptable, no matter the cause." "Using violence is morally wrong unless it is to save your own life or the life of another."

6. MORAL_CHARACTER
   Definition: It is wrong because of the kind of person or people it makes us.
   Not this: People are harmed (HARM). It never works (INEFFECTIVENESS). Always/never with no character claim (ABSOLUTE_PROHIBITION).
   Examples: "That is not who we are." "It says a lot about your character." "Using violence to get your way is shameful." """

USER_PROMPT_TEMPLATE = """Apply the codebook to the response below.

Reminder: The six categories are independent and multi-label. A response may contain several, one, or none. Mark each true or false on its own.

Response:
"{text}"

Return only a JSON object of the following form:
{{
  "harm": true/false,
  "ineffectiveness": true/false,
  "unnecessary_force": true/false,
  "illegitimate_authority": true/false,
  "absolute_prohibition": true/false,
  "moral_character": true/false,
  "reasoning": "brief justification"
}}"""


def main() -> None:
    parser = build_arg_parser(
        "Classify Study 2 political-violence-believer reasons (6 why-wrong types)"
    )
    args = parser.parse_args()
    if not regeneration_requested(args):
        return

    output_file = OUTPUT_DIR / "classifications_polviol.csv"
    asyncio.run(
        run_classifier(
            condition="polviol",
            categories=CATEGORIES,
            system_prompt=SYSTEM_PROMPT,
            user_prompt_template=USER_PROMPT_TEMPLATE,
            output_file=output_file,
            checkpoint_file=SCRIPT_DIR / "checkpoint_polviol.json",
            progress_label="Classifying political-violence reasons",
            limit=args.limit,
            belief_op=">=",
        )
    )
    if args.write_data:
        install_rating_files([output_file])


if __name__ == "__main__":
    main()
