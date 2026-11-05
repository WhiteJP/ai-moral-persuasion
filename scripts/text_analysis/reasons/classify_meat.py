"""
Classify Study 2 meat-skeptic reasons (cond_belief_pre <= 50).

Seven multi-label categories in one pass:
  4Ns (Piazza et al., 2015): Natural, Normal, Necessary, Nice
  Other justifications: Religious, Amoral, Conditional

Uses three OpenRouter models with majority-vote consensus.

Usage:
    export OPENROUTER_API_KEY=...
    python scripts/text_analysis/reasons/classify_meat.py --run
    python scripts/text_analysis/reasons/classify_meat.py --run --limit 5
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
    "natural",
    "normal",
    "necessary",
    "nice",
    "religious",
    "amoral",
    "conditional",
]

SYSTEM_PROMPT = """You are applying a fixed codebook to open-ended survey responses.

Each participant in a psychological study rated their agreement with the statement "Eating meat is morally wrong" (0–100) and then explained their view in their own words. The texts you will see come from respondents who did not accept that statement (score ≤ 50). Your task is to record which justifications they give: that is, reasons why eating meat is acceptable, or why it is not a moral problem.

General coding rules:
- Code the reason given, not the respondent's overall stance. Rejecting the claim that eating meat is wrong is not itself a code.
- A category is present only when that specific type of reason is stated or clearly implied.
- Code the idea, not particular words. A paraphrase counts. A matching word without that idea does not.
- The seven categories are independent and multi-label. A response may contain several, one, or none. Mark each true or false on its own.
- If the text merely restates that eating meat is acceptable or optional, without a further reason that fits a category, all seven should be false.
- These categories capture justifications for eating meat. Do not apply them to arguments that eating meat is wrong, that people ought not eat meat, or that failing to eat meat is required. If a response mixes both, code only the justification parts.

1. NATURAL
   Definition: Eating meat is acceptable because it is part of human nature or biology.
   Not this: That people have long eaten meat, or that it is a tradition (NORMAL). Health need (NECESSARY). Taste or enjoyment (NICE).
   Examples: "Humans evolved as omnivores." "It's part of the natural food chain."

2. NORMAL
   Definition: Eating meat is acceptable because it is a social, cultural, or family norm.
   Not this: Biology or the food chain (NATURAL). "God allows it" (RELIGIOUS).
   Examples: "Everyone eats meat." "It's part of my culture and heritage." "That's how I was raised."

3. NECESSARY
   Definition: Eating meat is acceptable because it is needed for health, strength, or survival.
   Not this: Enjoyment (NICE). Mere preference with no health claim.
   Examples: "We need protein from meat." "Meat provides essential nutrients." "You can't be healthy without meat."

4. NICE
   Definition: Eating meat is acceptable because it is enjoyable.
   Not this: Need for health (NECESSARY). Habit or culture (NORMAL). Still eating meat, cutting back, or feeling conflicted, with no claim that it tastes good or is enjoyable.
   Examples: "Meat tastes good." "I enjoy eating meat."

5. RELIGIOUS
   Definition: Eating meat is acceptable because God, scripture, or faith permits or commands it.
   Not this: Noting that some religions restrict meat. A non-religious appeal to nature or tradition. "Animals were put here for us to eat" with no God, scripture, or faith.
   Examples: "God provided animals for us to eat." "The Bible says it's fine to eat meat."

6. AMORAL
   Definition: Eating meat is not a moral issue.
   Not this: A moral defense of eating meat (any of the other categories). "Personal choice" only counts if they are dismissing the moral framing, not if they mean it is an optional but still moral good.
   Examples: "Eating meat has nothing to do with morals." "I don't see this as a moral issue."

7. CONDITIONAL
   Definition: Eating meat is acceptable only if ethical conditions are met.
   Not this: Blanket approval with no conditions. Blanket disapproval. Taste, health, or tradition offered without an ethical if-then. Permission from culture or religion (NORMAL or RELIGIOUS). Eating only some kinds of meat, with no claim about how animals are treated.
   Examples: "Eating meat is fine if the animals are treated humanely." "It depends on how the animals are raised." """

USER_PROMPT_TEMPLATE = """Apply the codebook to the response below.

Reminder: The seven categories are independent and multi-label. A response may contain several, one, or none. Mark each true or false on its own.

Response:
"{text}"

Return only a JSON object of the following form:
{{
  "natural": true/false,
  "normal": true/false,
  "necessary": true/false,
  "nice": true/false,
  "religious": true/false,
  "amoral": true/false,
  "conditional": true/false,
  "reasoning": "brief justification"
}}"""


def main() -> None:
    parser = build_arg_parser(
        "Classify Study 2 meat-skeptic reasons (4Ns + religious/amoral/conditional)"
    )
    args = parser.parse_args()
    if not regeneration_requested(args):
        return

    output_file = OUTPUT_DIR / "classifications_meat.csv"
    asyncio.run(
        run_classifier(
            condition="meat",
            categories=CATEGORIES,
            system_prompt=SYSTEM_PROMPT,
            user_prompt_template=USER_PROMPT_TEMPLATE,
            output_file=output_file,
            checkpoint_file=SCRIPT_DIR / "checkpoint_meat.json",
            progress_label="Classifying meat reasons",
            limit=args.limit,
        )
    )
    if args.write_data:
        install_rating_files([output_file])


if __name__ == "__main__":
    main()
