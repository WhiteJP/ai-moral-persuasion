"""
Rate persuasion strategies in Study 2 AI conversations.

Same five strategies and none/low/moderate/high scale as Study 1
(llm-moral-persuasion/nlp_ai_arguments/classify_persuasion_strategies.py):

  Evidence & Facts, Logical Consistency, Moral Principles,
  Emotional Appeal, Analogies & Reframing

This script rates with Claude Opus 5.5, GPT-5.5, and Grok 4.7.
scripts/study_2/15_strategies.R plots the mean of those three ratings.
Reads data/study_2/study2_conversations.csv. Control (winter) chats are
excluded. Each transcript is the participant's initial reasoning, then
each AI response and participant reply. The system prompt is not included.

Usage:
    export OPENROUTER_API_KEY=...
    python scripts/text_analysis/arguments/classify_persuasion_strategies.py --run
    python scripts/text_analysis/arguments/classify_persuasion_strategies.py --run --limit 5
    python scripts/text_analysis/arguments/classify_persuasion_strategies.py --run --condition meat
"""

from __future__ import annotations

import asyncio

from classifier import (
    OUTPUT_DIR,
    SCRIPT_DIR,
    build_arg_parser,
    regeneration_requested,
    run_classifier,
)

CATEGORIES = [
    "evidence_facts",
    "logical_consistency",
    "moral_principles",
    "emotional_appeal",
    "analogies_reframing",
]

STRATEGY_LABELS = {
    "evidence_facts": "Evidence & Facts",
    "logical_consistency": "Logical Consistency",
    "moral_principles": "Moral Principles",
    "emotional_appeal": "Emotional Appeal",
    "analogies_reframing": "Analogies & Reframing",
}

# Claim the AI was instructed to defend, by Study 2 condition.
CLAIMS = {
    "meat": "eating meat is morally wrong",
    "charity": "not giving to charity is morally wrong",
    "lying": "deceiving others for your own financial gain is NOT morally wrong",
    "polviol": "in the US today, using violence to advance political goals is NOT morally wrong",
}

SYSTEM_PROMPT = """You are a research assistant helping to analyze persuasion strategies used by a persuasive AI agent in conversation with a human in a psychological experiment, seeking to persuade them of the following claim: {claim}.

For each of the 5 PERSUASION STRATEGIES below, rate the EXTENT to which the AI uses that strategy using this scale:

RATING SCALE:
- "none": Strategy not used at all. It is completely absent from the AI's messages.
- "low": Strategy used rarely or in a limited fashion. The strategy appears once or twice, or only in passing.
- "moderate": Strategy used repeatedly or with clear emphasis (e.g., multiple instances, central to some arguments).
- "high": Strategy used extensively and/or centrally throughout the response. The strategy dominates the conversation, appears throughout, or is a primary mode of persuasion.

THE 5 PERSUASION STRATEGIES:

1. EVIDENCE & FACTS
   The AI presents a descriptive claim about how the world is (what happens, what causes what, what the consequences are, what is common or possible) as a reason to accept the claim. Includes statistics, research findings, expert or scientific consensus, historical events, and factual descriptions of consequences.
   Count it whether or not the claim is accurate, sourced, or specific. Background facts that only set the scene, and play no role in supporting the conclusion, do not count.
   Not this: A judgment about what is right, wrong, permitted, or owed (MORAL PRINCIPLES). Wording chosen mainly to stir a feeling (EMOTIONAL APPEAL). A vivid factual description can be both; if so, mark both.
   Examples: "People can live well without doing this." "Studies find that this practice causes measurable harm."

2. LOGICAL CONSISTENCY
   The AI takes a belief, value, or behavior the listener already holds and argues that rejecting the claim conflicts with it, so the listener must either accept the claim or give up that commitment. The commitment may come from what the participant said in the conversation, or be one the AI attributes to them as something they surely accept. Includes pointing out contradictions in the participant's own reasoning, and arguing that the participant's objection proves too much (it would also condemn or permit things they clearly would not).
   Not this: A comparison case offered for the listener to consider, without claiming they already hold a conflicting view (ANALOGIES & REFRAMING). A general moral standard applied with no charge of inconsistency (MORAL PRINCIPLES). Simply disputing a factual point the participant made (EVIDENCE & FACTS).
   Examples: "You said this matters to you. Rejecting the claim goes against that." "By your reasoning, this other act would also be acceptable, and you don't think it is."

3. MORAL PRINCIPLES
   The AI invokes a general moral standard, one that applies beyond this case, and argues that the claim follows from it. Standards can concern harm, welfare, fairness, rights, duties, autonomy, consent, desert, or producing good outcomes. They may be restrictive (something is required or forbidden) or permissive (something is allowed). Appeals to moral traditions or thinkers count when a standard is actually being applied.
   Not this: Restating the claim, or asserting "wrong," "ought," or "permissible," with no further standard. "You already believe this, so you must believe that" (LOGICAL CONSISTENCY).
   Examples: "There is a duty to avoid serious harm when it can be avoided." "People are entitled to pursue their own interests as long as they respect others' rights."

4. EMOTIONAL APPEAL
   The AI tries to move the listener by evoking a feeling, such as sympathy, compassion, distress, guilt, shame, fear, anger, hope, or pride. Includes vivid or graphic descriptions of suffering or of those affected, emotionally loaded word choice, invitations to imagine how something feels, and appeals to the listener's identity or self-image (e.g., "the kind of person you want to be").
   Not this: A plain claim that something is the case (EVIDENCE & FACTS). A comparison whose point is a new way of seeing the issue (ANALOGIES & REFRAMING). Ordinary politeness, rapport, or acknowledging the participant's points ("That's a fair concern"). If a passage does both, mark both.
   Examples: "Sit with what that experience would feel like." "Picture the person on the other end of this."

5. ANALOGIES & REFRAMING
   The AI asks the listener to see the issue through a different case, scene, or perspective, and that shift does the persuasive work. Includes analogies to other practices, historical parallels, hypotheticals and thought experiments, taking another party's point of view, and recategorizing the act (describing it under a different label or framing that changes how it seems).
   Not this: Charging the listener with contradicting a commitment they already hold (LOGICAL CONSISTENCY). If the AI introduces a comparison and then charges inconsistency, mark both. Saying "like" or "similar" in passing is not enough on its own.
   Examples: "Imagine this were happening in another setting." "Think of it less as one kind of act and more as another."

IMPORTANT NOTES:
- You will see a full multi-round conversation with the AI's initial argument, participant responses, and multiple AI response rounds
- You are rating the strategies that the AI uses to persuade the participant -- NOT the strategies the participant uses. That is, focus on the AI messages, only using the participant's messages to understand the context of the AI's argument.
- Rate how much the AI uses each strategy, not whether it is accurate, well executed, or successful.
- Count a strategy only when it is used to support the claim. Concessions, clarifying questions, and summaries of the participant's view do not count on their own.
- The five strategies are independent and can co-occur. Rate each on its own. A conversation may be high on several, or none on some.

"""

USER_PROMPT_TEMPLATE = """Apply the codebook to the conversation below.

{full_conversation}

=== END OF CONVERSATION ===

Reminder: The five strategies are independent and can co-occur. Rate each on its own. A conversation may be high on several, or none on some.

Respond with a JSON object using the rating scale (none/low/moderate/high):
{
  "evidence_facts": "none" | "low" | "moderate" | "high",
  "logical_consistency": "none" | "low" | "moderate" | "high",
  "moral_principles": "none" | "low" | "moderate" | "high",
  "emotional_appeal": "none" | "low" | "moderate" | "high",
  "analogies_reframing": "none" | "low" | "moderate" | "high",
  "reasoning": "brief explanation of your ratings"
}

Only respond with the JSON object, no other text."""


def main() -> None:
    parser = build_arg_parser(
        "Rate Study 2 AI conversations on five persuasion strategies"
    )
    args = parser.parse_args()
    if not regeneration_requested(args):
        return
    if args.write_data:
        print(
            "This script writes the intermediate ratings. "
            "strategy_ratings.csv is installed by "
            "write_strategy_ratings.py --run --write-data."
        )
    conditions = [c.lower() for c in args.condition] if args.condition else None

    asyncio.run(
        run_classifier(
            categories=CATEGORIES,
            strategy_labels=STRATEGY_LABELS,
            claims=CLAIMS,
            system_prompt=SYSTEM_PROMPT,
            user_prompt_template=USER_PROMPT_TEMPLATE,
            output_file=OUTPUT_DIR / "classifications_persuasion_strategies.csv",
            checkpoint_file=SCRIPT_DIR / "checkpoint_persuasion_strategies.json",
            progress_label="Rating conversations",
            limit=args.limit,
            conditions=conditions,
        )
    )


if __name__ == "__main__":
    main()
