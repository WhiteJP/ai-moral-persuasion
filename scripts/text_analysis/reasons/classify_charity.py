"""
Classify Study 2 charity-skeptic reasons (cond_belief_pre <= 50).

Seven multi-label anti-obligation reasoning types:
  Property rights, Supererogatory, Limited scope, Skepticism,
  Systemic, Financial constraints, Relativism

Uses three OpenRouter models with majority-vote consensus.

Usage:
    export OPENROUTER_API_KEY=...
    python scripts/text_analysis/reasons/classify_charity.py --run
    python scripts/text_analysis/reasons/classify_charity.py --run --limit 5
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
    "property_rights",
    "supererogatory",
    "limited_scope",
    "skepticism",
    "systemic",
    "financial_constraints",
    "relativism",
]

SYSTEM_PROMPT = """You are applying a fixed codebook to open-ended survey responses.

Each participant in a psychological study rated their agreement with the statement "Not giving to charity is morally wrong" (0–100) and then explained their view in their own words. The texts you will see come from respondents who did not accept that statement (score ≤ 50). Your task is to record which anti-obligation reasons they give: that is, reasons why failing to give is not morally wrong, or why there is no moral duty to give.

General coding rules:
- Code the reason given, not the respondent's overall stance. Rejecting a duty to give is not itself a code.
- A category is present only when that specific type of reason is stated or clearly implied.
- Code the idea, not particular words. A paraphrase counts. A matching word without that idea does not.
- The seven categories are independent and multi-label. A response may contain several, one, or none. Mark each true or false on its own.
- If the text merely restates that giving is optional, without a further reason that fits a category, all seven should be false.
- These categories capture reasons *against* a duty to give. Do not apply them to arguments that people ought to give, that giving is required, or that failing to give is wrong. If a response mixes both, code only the anti-obligation parts.

1. PROPERTY_RIGHTS
   Definition: There is no duty to give because people have a right to what they have earned; others are not entitled to it.
   Not this: Can't afford to give (FINANCIAL_CONSTRAINTS). Giving is nice but optional (SUPEREROGATORY) with no claim of ownership or entitlement. "Personal choice" as moral subjectivity (RELATIVISM).
   Examples: "It's my money, I earned it." "Nobody has a right to tell me what to do with my property."

2. SUPEREROGATORY
   Definition: Giving is good or praiseworthy, but it is not morally required. Both halves must be present, or clearly implied: (a) giving is a good/nice/admirable thing, and (b) you are not wrong for not doing it.
   Not this: "It's a choice," "you aren't forced to," or "that's okay if you don't" with no claim that giving is good. That is only denying a duty — leave it uncoded, or use another category if one fits. Can't afford it, by itself (FINANCIAL_CONSTRAINTS). "It's my money" (PROPERTY_RIGHTS). Morality is subjective (RELATIVISM).
   Examples: "It's nice but not obligatory." "Giving is good but not required." "Donating is nice, but people shouldn't be judged if they don't."

3. LIMITED_SCOPE
   Definition: Any duty to help stops at family or close others; it does not extend to distant strangers.
   Not this: Prioritizing family because one is broke (FINANCIAL_CONSTRAINTS). Government, not individuals, should help strangers (SYSTEMIC).
   Examples: "I'm only responsible for my own family." "No duty to help strangers."

4. SKEPTICISM
   Definition: Giving does not actually help.
   Not this: Charity is the wrong kind of solution because institutions should fix the problem (SYSTEMIC). Personal inability to give (FINANCIAL_CONSTRAINTS).
   Examples: "Most charity money is wasted." "It doesn't actually solve problems."

5. SYSTEMIC
   Definition: This is not an individual's duty. Government or other institutions should handle it, or charity papers over structural problems.
   Not this: Charities waste money or fail in practice (SKEPTICISM). One cannot afford to give (FINANCIAL_CONSTRAINTS).
   Examples: "That's what taxes/government are for." "Charity is a band-aid on systemic issues."

6. FINANCIAL_CONSTRAINTS
   Definition: The person cannot reasonably give, or must provide for themselves or their household first.
   Not this: Family-only duty even if one could give (LIMITED_SCOPE). Right to keep what one has earned, regardless of means (PROPERTY_RIGHTS). Vague "people have reasons not to give" with no claim about money or means.
   Examples: "I can barely afford my own bills." "Need to take care of myself first."

7. RELATIVISM
   Definition: There is no universal moral duty here because morality is subjective.
   Not this: Giving is optional but still morally good (SUPEREROGATORY). "It's my choice" as a property-rights claim (PROPERTY_RIGHTS). That the speaker personally gives, or has a personal policy, without claiming morality is subjective.
   Examples: "That's just your opinion." "Morality is subjective." """

USER_PROMPT_TEMPLATE = """Apply the codebook to the response below.

Reminder: The seven categories are independent and multi-label. A response may contain several, one, or none. Mark each true or false on its own.

Response:
"{text}"

Return only a JSON object of the following form:
{{
  "property_rights": true/false,
  "supererogatory": true/false,
  "limited_scope": true/false,
  "skepticism": true/false,
  "systemic": true/false,
  "financial_constraints": true/false,
  "relativism": true/false,
  "reasoning": "brief justification"
}}"""


def main() -> None:
    parser = build_arg_parser(
        "Classify Study 2 charity-skeptic reasons (7 anti-obligation types)"
    )
    args = parser.parse_args()
    if not regeneration_requested(args):
        return

    output_file = OUTPUT_DIR / "classifications_charity.csv"
    asyncio.run(
        run_classifier(
            condition="charity",
            categories=CATEGORIES,
            system_prompt=SYSTEM_PROMPT,
            user_prompt_template=USER_PROMPT_TEMPLATE,
            output_file=output_file,
            checkpoint_file=SCRIPT_DIR / "checkpoint_charity.json",
            progress_label="Classifying charity reasons",
            limit=args.limit,
        )
    )
    if args.write_data:
        install_rating_files([output_file])


if __name__ == "__main__":
    main()
