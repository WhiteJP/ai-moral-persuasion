# =============================================================================
# 03 · Attrition Analysis
# =============================================================================
# Tests for differential attrition between treatment groups at each stage
# using chi-squared tests on 2x2 (condition × stayed/dropped) matrices.
#
# Three tests:
#   1. ITT → final initial sample (post-randomization dropout)
#   2. ITT → follow-up sample
#   3. Final initial sample → follow-up sample

library(dplyr)
library(tibble)

source(here::here("scripts", "functions", "helpers.R"))

d1_itt <- readRDS(here::here("data", "study_1", "d1_itt.rds"))
d1     <- readRDS(here::here("data", "study_1", "d1.rds"))
d      <- readRDS(here::here("data", "study_1", "d.rds"))

# Helper to build and test a 2x2 attrition matrix
test_attrition <- function(pre_data, post_data, label) {
  mat <- pre_data |>
    count(condition) |>
    left_join(
      post_data |> count(condition),
      by = "condition",
      suffix = c("_pre", "_post")
    ) |>
    transmute(
      condition,
      dropped = n_pre - n_post,
      stayed  = n_post
    ) |>
    column_to_rownames("condition") |>
    as.matrix()

  section(label)
  cat("\nAttrition matrix:\n")
  print(mat)
  cat("\n")
  print(chisq.test(mat))
}

# =============================================================================
# TEST 1: ITT → Final initial sample (post-randomization dropout) (prereg)
# =============================================================================

test_attrition(d1_itt, d1, "ATTRITION: ITT -> FINAL INITIAL SAMPLE")

# =============================================================================
# TEST 2: ITT → Follow-up sample (prereg)
# =============================================================================

test_attrition(d1_itt, d, "ATTRITION: ITT -> FOLLOW-UP SAMPLE")

# =============================================================================
# TEST 3: Final initial sample → Follow-up sample (exploratory)
# =============================================================================

test_attrition(d1, d, "ATTRITION: FINAL INITIAL SAMPLE -> FOLLOW-UP SAMPLE")
