# Text analysis (optional LLM re-runs)

These scripts are Study 2 only. Study 1 ratings stay the files downloaded into `data/study_1/`.

By default `source("main.R")` does **not** call these scripts. It reads the frozen rating CSVs from `data/study_2/text_ratings/` (downloaded from OSF). That is enough to reproduce every Study 2 number that depends on the text ratings. Set `rerun_text_analysis <- TRUE` in `main.R` only if you want the pipeline to regenerate the Study 2 ratings.

Use this folder only if you want to **regenerate** ratings with current OpenRouter models. Re-runs:

- Need **Python 3**, the pip packages below, and `OPENROUTER_API_KEY`
- Cost money and can take hours
- **May not perfectly reproduce** the deposited CSVs (model revisions, provider routing, and residual non-determinism). Prefer the OSF files for exact reproduction of the paper

## Setup

```bash
python3 -m pip install -r scripts/text_analysis/reasons/requirements.txt
python3 -m pip install -r scripts/text_analysis/arguments/requirements.txt
export OPENROUTER_API_KEY=...
```

Survey conversation exports must already be present under `data/study_2/` (run `scripts/study_2/00_download_data.R` and `01_wrangle_data.R` first, or the full `main.R` once).

The classifiers do not call the API unless you pass `--run`. Add `--write-data` to copy the new paper CSVs into `data/study_2/text_ratings/`. Without that flag the downloaded files stay as they are. The same switch is `rerun_text_analysis <- TRUE` at the top of `main.R` (leave it `FALSE` for ordinary reproduction).

## Participant reasons

Models: Claude Sonnet 4.5 (`anthropic/claude-sonnet-4.5`), GPT-4.1 (`openai/gpt-4.1`), DeepSeek V3.2 (`deepseek/deepseek-v3.2`). Temperature 0, thinking off. Majority vote. Columns match `classifications_{meat,charity,lying,polviol}.csv`.

```bash
python scripts/text_analysis/reasons/classify_meat.py --run
python scripts/text_analysis/reasons/classify_charity.py --run
python scripts/text_analysis/reasons/classify_lying.py --run
python scripts/text_analysis/reasons/classify_polviol.py --run
```

These write under `output/study_2/text_analysis/`. Pass `--write-data` on each command to copy them into `data/study_2/text_ratings/`, then re-source `scripts/study_2/14_reasons.R`.

## Persuasion strategies

Models: Claude Opus 5.5 (`anthropic/claude-opus-5.5`), GPT-5.5 (`openai/gpt-5.5`), and Grok 4.7 (`x-ai/grok-4.7`), high reasoning effort.

```bash
python scripts/text_analysis/arguments/classify_persuasion_strategies.py --run
python scripts/text_analysis/arguments/write_strategy_ratings.py --run --write-data
```

The second command writes `strategy_ratings.csv`: `id`, `condition`, then for each strategy `claude`, `gpt`, and `grok`. Then re-source `scripts/study_2/15_strategies.R`.

## Direct-lie coding (lying condition)

Models: Claude Opus 5.5, GPT-5.5, Grok 4.7 (high reasoning effort). Same columns as `direct_lie_moves_{claude-opus-55,gpt-55,grok-47}.csv`.

```bash
python scripts/text_analysis/arguments/classify_direct_lie_moves.py --run --write-data
```

Then re-source `scripts/study_2/07_direct_lie.R`.
