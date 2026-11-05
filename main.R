# =============================================================================
# AI Moral Persuasion: Master Analysis Script
# =============================================================================
#
# This script runs all analyses reported in the paper in sequence.
# It can be run end-to-end via source("main.R"), which will:
#   - Download and wrangle Study 1, then Study 2
#   - Run every Study 2 analysis (the main experiment in the paper)
#   - Run every Study 1 analysis (the pilot)
#   - Save figures and tables under output/
#
# Prerequisites: run renv::restore() first to install required packages.
#
# Optional: set save_to_paper_repo <- TRUE to also copy paper figures/tables
# into the sibling manuscript repo (../llm-moral-persuasion/paper). That is
# only for updating the manuscript while drafting; leave FALSE for a public
# clone. Default when unset is FALSE.
#
# Optional: set mediation_bootstrap <- FALSE for a fast smoke test of the
# mediation analyses. mediate() then uses 20 quasi-Bayesian draws instead of
# the reported 1,000-simulation bootstrap. Regression coefficients match a
# full run. The indirect effect, direct effect, total effect, and percent
# mediated are averages of those draws, so they will not match the paper.
# Nothing is copied to the manuscript repo while this is FALSE.
# Default when unset is TRUE.
#
# Optional: set rerun_text_analysis <- TRUE to regenerate Study 2 LLM text
# ratings before the analyses. Leave FALSE to use the downloaded CSVs.
# =============================================================================

library(here)

# Wide console line width so sourced scripts' printed tables/tibbles wrap less
# (default is often 80; respects already-wider terminals via max).
options(width = max(getOption("width"), 200L))

# Copy figures/tables into ../llm-moral-persuasion/paper when TRUE.
# Leave FALSE for public reproduction; set TRUE only while updating the manuscript.
save_to_paper_repo <- FALSE

# Reported mediation intervals use a nonparametric bootstrap (1,000 sims, seed 42).
# Set FALSE for a fast smoke test: 20 quasi-Bayesian draws, no manuscript copy.
# Leave TRUE for numbers that go in the paper.
mediation_bootstrap <- TRUE

# Set TRUE to regenerate Study 2 LLM text ratings (paid OpenRouter calls).
# Leave FALSE to use the ratings downloaded into data/study_2/text_ratings/.
rerun_text_analysis <- FALSE

cat("====================================================================\n")
cat("AI Moral Persuasion: Reproducible Analysis Pipeline\n")
cat("====================================================================\n\n")

if (!isTRUE(mediation_bootstrap)) {
  options(mediation.smoke.announced = TRUE)
  cat("====================================================================\n")
  cat("SMOKE TEST: mediation_bootstrap is FALSE.\n")
  cat("Mediation uses 20 quasi-Bayesian draws, not the reported bootstrap.\n")
  cat("Regression coefficients match a full run. Effects from mediate() do not.\n")
  cat("Figures and tables will not be copied to the manuscript repo.\n")
  cat("====================================================================\n\n")
}

# ---------------------------------------------------------------------------
# Download and wrangle both studies before any analysis. The paper reports
# Study 2 first; Study 1 is the pilot and runs after it.
# ---------------------------------------------------------------------------

cat("============================== Data ==============================\n")
cat("Both studies are prepared first. Analyses then follow the paper: Study 2, then the pilot.\n\n")

cat("--- Study 1 · 00 Download ---\n")
source(here("scripts", "study_1", "00_download_data.R"))

cat("\n--- Study 1 · 01 Wrangle ---\n")
source(here("scripts", "study_1", "01_wrangle_data.R"))

cat("\n--- Study 2 · 00 Download ---\n")
source(here("scripts", "study_2", "00_download_data.R"))

cat("\n--- Study 2 · 01 Wrangle ---\n")
source(here("scripts", "study_2", "01_wrangle_data.R"))

if (isTRUE(rerun_text_analysis)) {
  cat("\n--- Study 2 · text ratings (regenerate) ---\n")
  source(here("scripts", "text_analysis", "rerun_text_analysis.R"))
}

# ---------------------------------------------------------------------------
# Study 2 (main experiment)
# ---------------------------------------------------------------------------

cat("\n============================== Study 2 ==============================\n\n")

cat("--- Study 2 · 02 Sample descriptives ---\n")
source(here("scripts", "study_2", "02_sample_descriptives.R"))

cat("\n--- Study 2 · 03 Attrition ---\n")
source(here("scripts", "study_2", "03_attrition.R"))

cat("\n--- Study 2 · 04 Main analyses (immediate, then follow-up). The paper's condition figures are the next step. ---\n")
source(here("scripts", "study_2", "04_main_analyses.R"))

cat("\n--- Study 2 · 05 Condition figures ---\n")
source(here("scripts", "study_2", "05_main_figures.R"))

cat("\n--- Study 2 · 06 Belief–behavior correlations ---\n")
source(here("scripts", "study_2", "06_belief_behavior_correlations.R"))

cat("\n--- Study 2 · 07 Direct-lie coding ---\n")
source(here("scripts", "study_2", "07_direct_lie.R"))

cat("\n--- Study 2 · 08 Pro- vs antisocial persuasion ---\n")
source(here("scripts", "study_2", "08_pro_v_anti.R"))

cat("\n--- Study 2 · 09 Belief mediation ---\n")
source(here("scripts", "study_2", "09_belief_mediation.R"))

cat("\n--- Study 2 · 10 Moderation ---\n")
source(here("scripts", "study_2", "10_moderation.R"))

cat("\n--- Study 2 · 11 Corrective conversation and antisocial recontact ---\n")
source(here("scripts", "study_2", "11_corrective.R"))

cat("\n--- Study 2 · 12 Spillover effects ---\n")
source(here("scripts", "study_2", "12_spillover_effects.R"))

cat("\n--- Study 2 · 13 Conversation-rating mediators ---\n")
source(here("scripts", "study_2", "13_rating_mediators.R"))

cat("\n--- Study 2 · 14 Participant reasons ---\n")
source(here("scripts", "study_2", "14_reasons.R"))

cat("\n--- Study 2 · 15 AI persuasion strategies ---\n")
source(here("scripts", "study_2", "15_strategies.R"))

# ---------------------------------------------------------------------------
# Study 1 (pilot)
# ---------------------------------------------------------------------------

cat("\n============================== Study 1 ==============================\n\n")

cat("--- Study 1 · 02 Sample descriptives ---\n")
source(here("scripts", "study_1", "02_sample_descriptives.R"))

cat("\n--- Study 1 · 03 Attrition ---\n")
source(here("scripts", "study_1", "03_attrition.R"))

cat("\n--- Study 1 · 04 Main analyses ---\n")
source(here("scripts", "study_1", "04_main_analyses.R"))

cat("\n--- Study 1 · 04b LLM agreement ---\n")
source(here("scripts", "study_1", "04b_llm_agreement.R"))

cat("\n--- Study 1 · 05 Figures ---\n")
source(here("scripts", "study_1", "05_figures.R"))

cat("\n--- Study 1 · 06 Preregistered exclusion table ---\n")
source(here("scripts", "study_1", "06_preregistered_exclusion_table.R"))

cat("\n====================================================================\n")
cat("All analyses complete.\n")
cat("Study 1 figures: output/study_1/figures/\n")
cat("Study 1 tables:  output/study_1/tables/\n")
cat("Study 2 figures: output/study_2/figures/ and output/figures/\n")
cat("Study 2 tables:  output/study_2/tables/\n")
if (isTRUE(save_to_paper_repo) && isTRUE(mediation_bootstrap)) {
  cat("Also copied selected figures/tables to ../llm-moral-persuasion/paper/\n")
} else if (isTRUE(save_to_paper_repo)) {
  cat("Skipped manuscript repo copies because mediation_bootstrap is FALSE.\n")
}
cat("====================================================================\n")
