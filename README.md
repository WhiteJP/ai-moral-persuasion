# ai-moral-persuasion

Complete code to reproduce all analyses, figures, and tables for:

**White, J. P., Allen, C., Caviola, L., & Rand, D. G. Using Human-AI Dialogues for Moral Persuasion.**

**Study 1** is the pilot. It is reported briefly in the main text and in full in the supplementary materials. **Study 2** is the main study reported in the paper.

## Quick start

1. Clone this repository and open `ai-moral-persuasion.Rproj` in RStudio (or set the working directory to the repo root).

2. Install R packages at the locked versions:

```r
renv::restore()
```

3. Run the full analysis pipeline:

```r
source("main.R")
```

That downloads Study 1 and Study 2 survey data from OSF (if needed), including the frozen Study 2 LLM text ratings, runs every analysis script in paper order, prints reported statistics to the console, and writes figures and tables under `output/`.

**Default reproduction uses the deposited ratings.** You do not need Python or an API key to reproduce the paper numbers, figures, and tables.

Expected runtime after packages are installed: on the order of tens of minutes (Study 2 mediation bootstraps dominate).

### Optional: copy outputs into the manuscript repo

At the top of `main.R`:

```r
save_to_paper_repo <- FALSE   # set TRUE only while updating the manuscript
```

When `TRUE`, selected PDFs and `.tex` tables are also copied into the sibling repo `../llm-moral-persuasion/paper/`. Leave it `FALSE` for a public clone (the committed default). Analysis outputs always go to `output/` in this repository.

## Data

OSF project: <https://osf.io/rwn89>

| Path on OSF | Local destination |
|-------------|-------------------|
| `study_1/data` | `data/study_1/` |
| `study_2/data` | `data/study_2/` |
| `study_2/text_ratings` | `data/study_2/text_ratings/` |

Raw Qualtrics exports and pre-computed LLM ratings are downloaded automatically. They are gitignored.

Study 2 text-rating files used by the paper:

- `strategy_ratings.csv` — persuasion-strategy ratings (Claude, GPT, and Grok)
- `classifications_{meat,charity,lying,polviol}.csv` — participant reason codes
- `direct_lie_moves_{claude-opus-55,gpt-55,grok-47}.csv` — direct-lie flags for the lying arm

## Optional: re-run Study 2 LLM text coding

Study 1 ratings are only the files downloaded into `data/study_1/`. There is no re-run path for them.

Study 2 reads the frozen CSVs above. You can optionally regenerate those ratings with the Python scripts under [`scripts/text_analysis/`](scripts/text_analysis/). That path is **not** required for reproducibility of the reported results.

Re-running the classifiers:

- Requires **Python 3**, the packages in `scripts/text_analysis/*/requirements.txt`, and an **OpenRouter API key** (`OPENROUTER_API_KEY`)
- Calls paid LLM APIs and can take a long time / cost money
- **Need not match the deposited ratings exactly.** Model versions, provider routing, and non-determinism can change labels even at temperature 0. Treat a re-run as a robustness check, not a bit-for-bit reproduction

See [`scripts/text_analysis/README.md`](scripts/text_analysis/README.md) for setup and commands. The Python scripts do nothing unless you pass `--run`. To regenerate ratings inside the pipeline, set `rerun_text_analysis <- TRUE` at the top of `main.R` (the committed default is `FALSE`). That calls the classifiers and copies the new CSVs into `data/study_2/text_ratings/` before the Study 2 analyses. A re-run need not match the deposited labels exactly.

## Repository structure

```
ai-moral-persuasion/
├── main.R
├── DESCRIPTION / renv.lock     # R package dependencies (locked)
├── scripts/
│   ├── functions/              # shared R helpers
│   ├── study_1/                # pilot (briefly in the main text; full results in the supplement)
│   ├── study_2/                # main study, paper order
│   └── text_analysis/          # optional Python LLM reruns
├── data/                       # downloaded at runtime (gitignored)
└── output/                     # generated figures and tables (gitignored)
```

Exploratory work lives in `extra_analyses/` (gitignored) and is **not** sourced by `main.R`. Ignore that folder when reproducing the paper.

Shared Study 2 display labels/colors live in `scripts/functions/study2_style.R` (condition name for the lying arm is **Lying** everywhere).

## Study 2 script order

| Script | Paper content |
|--------|----------------|
| `00_download_data.R` | OSF download |
| `01_wrangle_data.R` | Analysis samples |
| `02_sample_descriptives.R` | Sample flow, demographics |
| `03_attrition.R` | Attrition tests and supplement tables |
| `04_main_analyses.R` | Orchestrator → `04a` immediate + `04b` follow-up / persistence |
| `05_main_figures.R` | `fig-meat`, `fig-charity`, `fig-lying`, `fig-polviol` |
| `06_belief_behavior_correlations.R` | SI belief–behavior correlations |
| `07_direct_lie.R` | Direct-lie shares and heads rates |
| `08_pro_v_anti.R` | Pro- vs antisocial contrasts |
| `09_belief_mediation.R` | Belief mediation table |
| `10_moderation.R` | Moderation and Holm table |
| `11_corrective.R` | Orchestrator → `11a` corrective + `11b` antisocial recontact |
| `12_spillover_effects.R` | Spillover matrices and TE figure |
| `13_rating_mediators.R` | Conversation-rating mediation table |
| `14_reasons.R` | Orchestrator → `14a` prevalence + `14b` outcomes + `14c` agreement |
| `15_strategies.R` | Strategy figure and agreement table |

Hand-authored manuscript assets (study design figure, example dialogues, `conditions.tex`, `justification-codes.tex`) live in the paper repository and are not generated here.

## System requirements

**Required for `source("main.R")`:**

- **R 4.5.x** (the lockfile was built with R 4.5.3)
- RStudio recommended
- R packages via `renv::restore()` (see `DESCRIPTION` / `renv.lock`)

`renv.lock` pins package **versions** from CRAN. On restore, renv installs those versions for your OS (macOS, Windows, or Linux). You do not need the authors' local `renv/library/` folders.

**Platform notes**

- Paths use `here` / `file.path`, so Windows path separators are fine.
- Figure PDFs use Quartz + Arial on macOS when available; elsewhere they fall back to `cairo_pdf`. On Linux you may need a Cairo-capable R build (common with CRAN binaries). Figure layout can differ slightly by OS/font, but the underlying estimates do not.
- Optional PDF trimming looks for `python3` or `python` plus `pypdf`; if missing, untrimmed PDFs are still written.
- Compiled packages (e.g. dplyr, readr, estimatr) install from CRAN binaries when available. On Windows, install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) only if a package must be compiled from source.

**Optional (LLM re-runs only):**

- Python 3 and `pip install -r scripts/text_analysis/*/requirements.txt`
- OpenRouter API key (`OPENROUTER_API_KEY`; on Windows PowerShell: `$env:OPENROUTER_API_KEY="..."`)
