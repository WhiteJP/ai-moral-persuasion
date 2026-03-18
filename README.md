# ai-moral-persuasion

This repository contains the complete code to reproduce all analyses, figures, and tables for our research paper:

**White, J. P., Allen, C., Caviola, L., & Rand, D. G. (2026). Using Human-AI Dialogues for Moral Persuasion.**

## Quick Start

### Prerequisites

- **R 4.4.0+** — Download from [r-project.org](https://cran.r-project.org/)
- **RStudio** (recommended) — Download from [posit.co](https://posit.co/download/rstudio-desktop/)

### Reproduce all analyses

1. **Clone the repository:**

```bash
git clone https://github.com/WhiteJP/ai-moral-persuasion.git
cd ai-moral-persuasion
```

2. **Open the RStudio project:**
   Double-click `ai-moral-persuasion.Rproj`, or open RStudio and go to File → Open Project.

3. **Install dependencies:**

```r
renv::restore()
```

This will install all R packages at the exact versions used in our analyses. It may take 5–10 minutes on a first run.

4. **Run all analyses:**

```r
source("main.R")
```

This downloads the data from OSF (if not already present), runs every analysis script in order, prints all statistical output to the console, and saves the main figure.

### Expected Output

- **Console output**: Sample flow, demographics, attrition chi-squared tests, all preregistered analyses with effect sizes, and persistence rates — all printed to the console.
- **Figure**: `output/figures/fig-combined.png` (Figure 1 from the paper).
- **Runtime**: ~2–5 minutes total (after packages are installed).

### Alternative: Run scripts individually

If you prefer to explore interactively, run each script in RStudio in the order shown in `main.R`:

```r
source("scripts/00_download_data.R")       # Download data from OSF
source("scripts/01_wrangle_data.R")        # Wrangle raw data, create analysis samples
source("scripts/02_sample_descriptives.R") # Sample flow, demographics
source("scripts/03_attrition_analysis.R")  # Attrition chi-squared tests
source("scripts/04_main_analyses.R")       # Preregistered analyses, persistence rates
source("scripts/05_figure.R")             # Figure 1 (6-panel combined figure)
```

## Data

The data is hosted on the Open Science Framework (OSF): <https://osf.io/rwn89>

**Automatic download**: Running `main.R` (or `scripts/00_download_data.R`) automatically downloads all data files into the `data/` directory. If files already exist locally, the download is skipped.

### Data files

| File | Description |
|------|-------------|
| `study1_initial.csv` | Qualtrics export from the initial study (N = 559 pre-exclusion) |
| `study1_followup.csv` | Qualtrics export from the one-month follow-up study |
| `classifications_4ns.csv` | LLM classifications of meat "4Ns" reasoning (Natural, Normal, Necessary, Nice) |
| `classifications_other.csv` | LLM classifications of other meat reasoning (Amoral, Conditional, Religious) |
| `classifications_reasoning.csv` | LLM classifications of charity reasoning types |
| `classifications_persuasion_strategies.csv` | LLM ratings of AI persuasion strategies across conversations |

The first two files are raw Qualtrics survey exports. The remaining four contain pre-computed NLP classifications produced by three LLMs (Claude, GPT, DeepSeek) with majority-vote consensus scoring. These are used for Figure 1 panels D–F.

## Repository Structure

```
ai-moral-persuasion/
├── main.R                              # Master script (runs everything)
├── DESCRIPTION                         # R package dependencies
├── renv.lock                           # Locked package versions
├── renv/                               # renv library management
├── scripts/
│   ├── functions/
│   │   ├── agreement_functions.R       # ICC & Cronbach's alpha (from effective-giving-AI-persuasion)
│   │   ├── read_csv_qualtrics.R        # Qualtrics CSV reader
│   │   ├── download_data.R             # OSF download helper
│   │   └── helpers.R                   # Shared utilities (consensus, demographics, etc.)
│   ├── 00_download_data.R              # Download data from OSF
│   ├── 01_wrangle_data.R              # Data wrangling & sample definitions
│   ├── 02_sample_descriptives.R       # Sample flow, demographics
│   ├── 03_attrition_analysis.R        # Differential attrition tests
│   ├── 04_main_analyses.R             # Preregistered analyses & persistence rates
│   ├── 04b_llm_agreement.R           # LLM classification agreement (ICC, alpha, % agree)
│   └── 05_figure.R                    # Figure 1 (6-panel combined figure)
├── data/                               # Downloaded at runtime (gitignored)
└── output/
    └── figures/                        # Generated figures (gitignored)
```

## What Each Script Produces

| Script | Paper Section | Output |
|--------|---------------|--------|
| `02_sample_descriptives.R` | Methods: Participants | Sample sizes (559 → 528 → 500), demographics |
| `03_attrition_analysis.R` | SM: Participants | Chi-squared tests for differential attrition |
| `04_main_analyses.R` | Results | All 6 preregistered ATEs with Cohen's d, persistence rates |
| `04b_llm_agreement.R` | SM: LLM reliability | % all-3-agree (reasoning), ICC & Cronbach's alpha (strategies) |
| `05_figure.R` | Results: Figure 1 | Combined 6-panel figure (belief change, donation, voucher, reasoning, strategies) |

## Preregistered Analyses

The study was preregistered at <https://aspredicted.org/t28hb8.pdf>. Script `04_main_analyses.R` runs all preregistered analyses on both the full sample and the preregistered exclusion sample (excluding participants scoring >75 on their conversation-relevant pre-treatment belief).

| Analysis | Outcome | Model |
|----------|---------|-------|
| 1 | Belief difference (charity − meat) | Lin estimator |
| 2a | Meat belief change | Lin estimator |
| 2b | Charity belief change | Lin estimator |
| 3 | Donation to AMF (cents) | Lin estimator |
| 4 | Voucher choice (veg vs. beef) | Logistic regression |
| 5a–b | Reflection DVs (meat consumption, donation) | Lin estimator (winsorized) |
| 6a–d | Follow-up versions of analyses 1–4 | Same as above |

## System Requirements

**Software**: R (≥ 4.4.0), RStudio (recommended)

**R packages** (automatically installed via `renv`): dplyr, tidyr, readr, ggplot2, stringr, purrr, tibble, lubridate, estimatr, effsize, patchwork, ggtext, ggdist, cowplot, osfr, here, fs

**Hardware**: Any standard desktop or laptop.

**OS**: Tested on Windows 11, but should work on macOS and Linux.
