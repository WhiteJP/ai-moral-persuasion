# =============================================================================
# AI Moral Persuasion: Master Analysis Script
# =============================================================================
#
# This script runs all analyses reported in the paper in sequence.
# It can be run end-to-end via source("main.R"), which will:
#   - Download data from OSF (if not already present)
#   - Wrangle and save analysis datasets
#   - Print sample descriptives and attrition tests
#   - Run all preregistered analyses and persistence rates
#   - Save Figure 1 to output/figures/fig-combined.png
#
# For a more exploratory experience, step through each script interactively
# in RStudio (in the order shown below).
#
# Prerequisites: run renv::restore() first to install required packages.
# =============================================================================

library(here)

cat("====================================================================\n")
cat("AI Moral Persuasion: Reproducible Analysis Pipeline\n")
cat("====================================================================\n\n")

# Step 0: Download data from OSF
cat("--- Step 0: Downloading data from OSF ---\n")
source(here("scripts", "00_download_data.R"))

# Step 1: Wrangle data
cat("\n--- Step 1: Wrangling data ---\n")
source(here("scripts", "01_wrangle_data.R"))

# Step 2: Sample descriptives
cat("\n--- Step 2: Sample descriptives ---\n")
source(here("scripts", "02_sample_descriptives.R"))

# Step 3: Attrition analysis
cat("\n--- Step 3: Attrition analysis ---\n")
source(here("scripts", "03_attrition_analysis.R"))

# Step 4: Main analyses (preregistered + persistence rates)
cat("\n--- Step 4: Main analyses ---\n")
source(here("scripts", "04_main_analyses.R"))

# Step 4b: LLM classification agreement
cat("\n--- Step 4b: LLM agreement analysis ---\n")
source(here("scripts", "04b_llm_agreement.R"))

# Step 5: Figure 1
cat("\n--- Step 5: Figure ---\n")
source(here("scripts", "05_figure.R"))

cat("\n====================================================================\n")
cat("All analyses complete.\n")
cat("Figure saved to: output/figures/fig-combined.png\n")
cat("====================================================================\n")
