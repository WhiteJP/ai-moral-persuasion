# =============================================================================
# 04b · LLM Classification Agreement Analysis
# =============================================================================
# Analyzes inter-rater agreement between the three LLMs (Claude, GPT, DeepSeek)
# for:
#   1. Reasoning classifications: % where all 3 models agreed (pre score <= 50)
#   2. Persuasion strategy ratings: ICC and Cronbach's alpha (numeric)
#
# Uses agreement functions from:
#   https://github.com/WhiteJP/effective-giving-AI-persuasion
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("scripts", "functions", "helpers.R"))
source(here("scripts", "functions", "agreement_functions.R"))

d1 <- readRDS(here("data", "study_1", "d1.rds"))

# =============================================================================
# HELPER: Convert logical-like columns (handles "True"/"False" strings)
# =============================================================================

as_logical_safe <- function(x) {
  if (is.logical(x)) return(x)
  if (is.character(x)) {
    x <- tolower(trimws(x))
    out <- rep(NA, length(x))
    out[x %in% c("true", "t", "1", "yes")] <- TRUE
    out[x %in% c("false", "f", "0", "no")] <- FALSE
    return(out)
  }
  as.logical(x)
}

# =============================================================================
# 1. REASONING CLASSIFICATIONS: % ALL 3 MODELS AGREED (pre <= 50)
# =============================================================================

#' Compute % agreement (all 3 models same) for a single classification
pct_all_three_agree <- function(d, col_base, models = c("claude", "gpt", "deepseek")) {
  cols <- paste0(col_base, "_", models)
  if (!all(cols %in% names(d))) return(tibble(n_valid = 0L, n_agree = 0L, pct_agree = NA_real_))

  sub <- d |>
    dplyr::select(all_of(cols)) |>
    mutate(across(everything(), as_logical_safe))

  n_valid <- sum(complete.cases(sub))
  if (n_valid == 0) return(tibble(n_valid = 0L, n_agree = 0L, pct_agree = NA_real_))

  all_same <- sub |>
    mutate(
      all_true  = Reduce(`&`, across(everything(), ~ .x == TRUE)),
      all_false = Reduce(`&`, across(everything(), ~ .x == FALSE)),
      agree = all_true | all_false
    ) |>
    pull(agree)

  n_agree <- sum(all_same, na.rm = TRUE)
  tibble(
    n_valid = n_valid,
    n_agree = n_agree,
    pct_agree = round(100 * n_agree / n_valid, 1)
  )
}

section("LLM AGREEMENT: REASONING CLASSIFICATIONS (pre score <= 50)", char = "#")

# --- Meat group: 4Ns + other ---

df_4ns <- read_csv(here("data", "study_1", "classifications_4ns.csv"), show_col_types = FALSE)
df_other <- read_csv(here("data", "study_1", "classifications_meat_other.csv"), show_col_types = FALSE)

d_meat <- d1 |>
  filter(condition == "Meat", meat_wrong_pre <= 50) |>
  dplyr::select(PROLIFIC_PID, meat_wrong_pre) |>
  left_join(
    df_4ns |> dplyr::select(id, natural_claude, natural_gpt, natural_deepseek,
                     normal_claude, normal_gpt, normal_deepseek,
                     necessary_claude, necessary_gpt, necessary_deepseek,
                     nice_claude, nice_gpt, nice_deepseek),
    by = c("PROLIFIC_PID" = "id")
  ) |>
  left_join(
    df_other |> dplyr::select(id, religious_claude, religious_gpt, religious_deepseek,
                       amoral_claude, amoral_gpt, amoral_deepseek,
                       conditional_claude, conditional_gpt, conditional_deepseek),
    by = c("PROLIFIC_PID" = "id")
  )

meat_4n_cats <- c("natural", "normal", "necessary", "nice")
meat_other_cats <- c("religious", "amoral", "conditional")

meat_agreement <- bind_rows(
  purrr::map_dfr(meat_4n_cats, function(cat) {
    pct_all_three_agree(d_meat, cat) |>
      mutate(classification = cat, group = "Meat")
  }),
  purrr::map_dfr(meat_other_cats, function(cat) {
    # df_other uses different column names - check
    col_base <- cat
    pct_all_three_agree(d_meat, col_base) |>
      mutate(classification = cat, group = "Meat")
  })
)

cat("\n--- Meat reasoning (pre <= 50, N =", nrow(d_meat), ") ---\n")
print(meat_agreement)

# --- Charity group ---

df_charity <- read_csv(here("data", "study_1", "classifications_charity_reasoning.csv"),
                       show_col_types = FALSE)

charity_cats <- c(
  "financial_constraints", "supererogatory", "skepticism", "relativism",
  "property_rights", "systemic", "limited_scope"
)

d_charity <- d1 |>
  filter(condition == "Charity", charity_wrong_pre <= 50) |>
  dplyr::select(PROLIFIC_PID, charity_wrong_pre) |>
  left_join(df_charity |> dplyr::select(id, contains("_claude"), contains("_gpt"), contains("_deepseek")),
            by = c("PROLIFIC_PID" = "id"))

charity_agreement <- purrr::map_dfr(charity_cats, function(cat) {
  pct_all_three_agree(d_charity, cat) |>
    mutate(classification = cat, group = "Charity")
})

cat("\n--- Charity reasoning (pre <= 50, N =", nrow(d_charity), ") ---\n")
print(charity_agreement)

# --- Summary table ---

reasoning_agreement_summary <- bind_rows(meat_agreement, charity_agreement) |>
  dplyr::select(group, classification, n_valid, n_agree, pct_agree)

section("REASONING AGREEMENT SUMMARY: % where all 3 models agreed", char = "#")
print(as.data.frame(reasoning_agreement_summary), row.names = FALSE, right = FALSE)

# --- pct_agree summary by group: mean, sd, min, max ---

reasoning_pct_summary <- reasoning_agreement_summary |>
  group_by(group) |>
  summarise(
    mean_pct = round(mean(pct_agree, na.rm = TRUE), 1),
    sd_pct   = round(sd(pct_agree, na.rm = TRUE), 1),
    min_pct  = round(min(pct_agree, na.rm = TRUE), 1),
    max_pct  = round(max(pct_agree, na.rm = TRUE), 1),
    .groups  = "drop"
  )

section("REASONING pct_agree: mean, SD, min, max by group", char = "#")
print(as.data.frame(reasoning_pct_summary), row.names = FALSE, right = FALSE)

# =============================================================================
# 2. PERSUASION STRATEGIES: ICC AND CRONBACH'S ALPHA (numeric)
# =============================================================================

section("LLM AGREEMENT: PERSUASION STRATEGY RATINGS (ICC & Cronbach's alpha)", char = "#")

d_strat <- read_csv(here("data", "study_1", "classifications_persuasion_strategies.csv"),
                    show_col_types = FALSE)

strategies <- c("evidence_facts", "logical_consistency", "moral_principles",
                "emotional_appeal", "analogies_reframing")
models <- c("claude", "gpt", "deepseek")
rating_to_num <- c(none = 0, low = 1, moderate = 2, high = 3)

# Build numeric matrix for each strategy (rows = conversations, cols = 3 models)
for (s in strategies) {
  model_cols <- paste0(s, "_", models)
  d_strat[[paste0(s, "_claude_num")]] <- rating_to_num[d_strat[[model_cols[1]]]]
  d_strat[[paste0(s, "_gpt_num")]]    <- rating_to_num[d_strat[[model_cols[2]]]]
  d_strat[[paste0(s, "_deepseek_num")]] <- rating_to_num[d_strat[[model_cols[3]]]]
}

# Helper: extract ICC2k from psych::ICC
get_icc2k <- function(mat) {
  if (nrow(mat) < 2) return(NA_real_)
  res <- psych::ICC(mat)$results
  res$ICC[res$type == "ICC2k"]
}

# Build ICC2k table: strategy, ICC2k, n (all conversations)
icc2k_table <- purrr::map_dfr(strategies, function(s) {
  num_cols <- c(
    paste0(s, "_claude_num"),
    paste0(s, "_gpt_num"),
    paste0(s, "_deepseek_num")
  )
  mat <- d_strat |>
    dplyr::select(all_of(num_cols)) |>
    tidyr::drop_na() |>
    as.matrix()
  tibble(
    strategy = s,
    ICC2k    = round(get_icc2k(mat), 3),
    n        = nrow(mat)
  )
})

section("ICC2k BY STRATEGY (all conversations)", char = "#")
print(as.data.frame(icc2k_table), row.names = FALSE, right = FALSE)

# ICC2k summary: mean, SD, min, max across strategies
icc2k_summary <- icc2k_table |>
  summarise(
    mean_ICC2k = round(mean(ICC2k, na.rm = TRUE), 3),
    sd_ICC2k   = round(sd(ICC2k, na.rm = TRUE), 3),
    min_ICC2k  = round(min(ICC2k, na.rm = TRUE), 3),
    max_ICC2k  = round(max(ICC2k, na.rm = TRUE), 3),
    .groups    = "drop"
  )

cat("\n--- ICC2k distribution (across 5 strategies): mean, SD, min, max ---\n")
print(as.data.frame(icc2k_summary), row.names = FALSE, right = FALSE)

# Run full agreement_analysis per strategy (for console output)
for (s in strategies) {
  num_cols <- c(
    paste0(s, "_claude_num"),
    paste0(s, "_gpt_num"),
    paste0(s, "_deepseek_num")
  )
  mat <- d_strat |>
    dplyr::select(all_of(num_cols)) |>
    tidyr::drop_na() |>
    as.matrix()
  if (nrow(mat) < 2) next
  cat("\n--- Strategy:", s, "(N =", nrow(mat), ") ---\n")
  agreement_analysis(mat)
}

section("LLM AGREEMENT ANALYSIS COMPLETE", char = "#")
