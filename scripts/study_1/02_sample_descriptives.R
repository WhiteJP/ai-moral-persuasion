# =============================================================================
# 02 · Sample Descriptives
# =============================================================================
# Prints sample flow, demographics for the initial and follow-up samples,
# and mean time between initial and follow-up studies.

library(dplyr)
library(lubridate)

source(here::here("scripts", "functions", "helpers.R"))

d1_all <- readRDS(here::here("data", "study_1", "d1_all.rds"))
d1_itt <- readRDS(here::here("data", "study_1", "d1_itt.rds"))
d1     <- readRDS(here::here("data", "study_1", "d1.rds"))
d      <- readRDS(here::here("data", "study_1", "d.rds"))

# =============================================================================
# SAMPLE FLOW
# =============================================================================

d1_treated <- d1_all |> filter(!is.na(prev_meat))

section("SAMPLE FLOW")
cat(sprintf("  Recruited:              %d\n", nrow(d1_all)))
cat(sprintf("  Reached treatment:      %d  (-%d pre-randomization)\n",
            nrow(d1_treated), nrow(d1_all) - nrow(d1_treated)))
cat(sprintf("  Passed attention check: %d  (-%d failed)\n",
            nrow(d1_itt), nrow(d1_treated) - nrow(d1_itt)))
cat(sprintf("  Completed survey (d1):  %d  (-%d withdrew post-randomization)\n",
            nrow(d1), nrow(d1_itt) - nrow(d1)))
cat(sprintf("  Follow-up completers:   %d\n", nrow(d)))

# =============================================================================
# DEMOGRAPHICS - INITIAL SAMPLE (d1)
# =============================================================================

section("DEMOGRAPHICS: INITIAL SAMPLE (d1)")

cat("\nGender:\n")
print(d1 |> count(gender))
cat("\nGender (recoded):\n")
print(d1 |> count(gender_recoded))

cat("\nAge:\n")
d1 |>
  summarise(
    M    = mean(age, na.rm = TRUE),
    SD   = sd(age, na.rm = TRUE),
    Min  = min(age, na.rm = TRUE),
    Max  = max(age, na.rm = TRUE)
  ) |>
  as.data.frame() |>
  print()

cat("\nCondition counts:\n")
print(d1 |> count(condition))

# =============================================================================
# DEMOGRAPHICS - FOLLOW-UP SAMPLE (d)
# =============================================================================

section("DEMOGRAPHICS: FOLLOW-UP SAMPLE (d)")

cat("\nGender:\n")
print(d |> count(gender))
cat("\nGender (recoded):\n")
print(d |> count(gender_recoded))

cat("\nAge:\n")
d |>
  summarise(
    M    = mean(age, na.rm = TRUE),
    SD   = sd(age, na.rm = TRUE),
    Min  = min(age, na.rm = TRUE),
    Max  = max(age, na.rm = TRUE)
  ) |>
  as.data.frame() |>
  print()

cat("\nCondition counts:\n")
print(d |> count(condition))

# =============================================================================
# TIME BETWEEN STUDIES
# =============================================================================

section("TIME BETWEEN INITIAL AND FOLLOW-UP")

d |>
  summarise(
    mean_days = mean(as.numeric(EndDate_t2 - EndDate, units = "days"), na.rm = TRUE),
    sd_days   = sd(as.numeric(EndDate_t2 - EndDate, units = "days"), na.rm = TRUE)
  ) |>
  as.data.frame() |>
  print()
