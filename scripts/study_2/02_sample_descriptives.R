# =============================================================================
# 02 · Sample Descriptives
# =============================================================================
# Prints sample flow and demographics for the initial Study 2 sample,
# plus follow-up completion of the belief items and the two behaviour items.

library(dplyr)

source(here::here("scripts", "functions", "helpers.R"))

d1_all        <- readRDS(here::here("data", "study_2", "study2_d1_all.rds"))
d1_attnpassed <- readRDS(here::here("data", "study_2", "study2_d1_attnpassed.rds"))
d1_treated    <- readRDS(here::here("data", "study_2", "study2_d1_treated.rds"))
d1_itt        <- readRDS(here::here("data", "study_2", "study2_d1_itt.rds"))
d1            <- readRDS(here::here("data", "study_2", "study2_d1.rds"))
d2_invited    <- readRDS(here::here("data", "study_2", "study2_d2_fu_invited.rds"))
d2_matched    <- readRDS(here::here("data", "study_2", "study2_d2_matched.rds"))

# =============================================================================
# SAMPLE FLOW
# =============================================================================
# Stages match 01_wrangle_data.R. study2_d1_all.rds is unique participants
# after duplicate-session resolution (not raw Qualtrics rows).

section("SAMPLE FLOW")
cat(sprintf("  Unique after dedupe:       %d\n", nrow(d1_all)))
cat(sprintf("  Passed attention check:    %d  (-%d failed / never reached)\n",
            nrow(d1_attnpassed), nrow(d1_all) - nrow(d1_attnpassed)))
cat(sprintf("  Assigned to treatment:     %d  (-%d not assigned)\n",
            nrow(d1_treated), nrow(d1_attnpassed) - nrow(d1_treated)))
cat(sprintf("  ITT (passed bot check):    %d  (-%d failed bot check)\n",
            nrow(d1_itt), nrow(d1_treated) - nrow(d1_itt)))
cat(sprintf("  Completed survey (d1):     %d  (-%d incomplete / no AI conversation)\n",
            nrow(d1), nrow(d1_itt) - nrow(d1)))

fu_both_beliefs <- !is.na(d2_matched$belief_meat_t2) & !is.na(d2_matched$belief_charity_t2)
fu_both_behav <- !is.na(d2_matched$voucher_is_veg_t2) & !is.na(d2_matched$donation_cents_t2)
n_fu_invited <- nrow(d2_invited)
n_fu_returned <- nrow(d2_matched)
n_fu_beliefs <- sum(fu_both_beliefs)
n_fu_behav <- sum(fu_both_behav)

cat(sprintf("  Follow-up invited:         %d  (control / meat / charity completers)\n",
            n_fu_invited))
cat(sprintf("  Follow-up returned:        %d  (-%d did not return; %.0f%% of those invited)\n",
            n_fu_returned, n_fu_invited - n_fu_returned,
            100 * n_fu_returned / n_fu_invited))
cat(sprintf("  Completed both beliefs:    %d  (-%d returned without both beliefs)\n",
            n_fu_beliefs, n_fu_returned - n_fu_beliefs))
cat(sprintf("  Completed both behaviours: %d  (-%d returned without voucher and donation)\n",
            n_fu_behav, n_fu_returned - n_fu_behav))

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

cat("\nRace (select all that apply; % of sample):\n")
race_labels <- c(
  race_white            = "White",
  race_black            = "Black or African American",
  race_asian            = "Asian",
  race_native_american  = "Native American or Alaska Native",
  race_pacific_islander = "Native Hawaiian or Pacific Islander",
  race_hispanic         = "Hispanic or Latino",
  race_middle_eastern   = "Middle Eastern or North African",
  race_other            = "Other"
)
for (nm in names(race_labels)) {
  cat(sprintf("  %-40s %5.1f%%\n", race_labels[[nm]], 100 * mean(d1[[nm]], na.rm = TRUE)))
}
n_race_multi <- sum(rowSums(d1[names(race_labels)]) >= 2)
cat(sprintf("  Selected more than one: %d (%.1f%%)\n", n_race_multi, 100 * n_race_multi / nrow(d1)))

cat("\nEducation:\n")
print(d1 |> count(edu_cat))

cat("\nIncome category:\n")
print(d1 |> count(income_cat))

cat("\nPolitical ideology (1–7):\n")
d1 |>
  summarise(
    M    = mean(pol_orientation, na.rm = TRUE),
    SD   = sd(pol_orientation, na.rm = TRUE),
    Min  = min(pol_orientation, na.rm = TRUE),
    Max  = max(pol_orientation, na.rm = TRUE)
  ) |>
  as.data.frame() |>
  print()
cat("\n")
print(d1 |> count(pol_orientation))

cat("\nParty ID (main):\n")
print(d1 |> count(party_id))

cat("\nParty ID (detailed):\n")
print(d1 |> count(party_id_detailed))

cat("\nParty ID (collapsed):\n")
print(d1 |> count(party_id_collapsed))

cat("\nCondition counts:\n")
print(d1 |> count(condition))

# =============================================================================
# DEMOGRAPHICS - FOLLOW-UP SAMPLE (returners)
# =============================================================================
# Round 1 demographics of every follow-up returner matched to a completer.

section("DEMOGRAPHICS: FOLLOW-UP SAMPLE (returners)")

d1_fu <- d1 |> filter(participantId %in% d2_matched$participantId)

cat(sprintf("\nReturners: %d of %d invited (%.1f%%)\n",
            nrow(d1_fu), n_fu_invited, 100 * nrow(d1_fu) / n_fu_invited))

cat("\nGender:\n")
print(d1_fu |> count(gender))

cat("\nAge:\n")
d1_fu |>
  summarise(
    M    = mean(age, na.rm = TRUE),
    SD   = sd(age, na.rm = TRUE),
    Min  = min(age, na.rm = TRUE),
    Max  = max(age, na.rm = TRUE)
  ) |>
  as.data.frame() |>
  print()

cat("\nCondition counts:\n")
print(d1_fu |> count(condition))

# =============================================================================
# FIELDWORK DATES
# =============================================================================

section("FIELDWORK DATES")
cat(sprintf("  Initial study (completers):  %s to %s\n",
            min(as.Date(d1$StartDate)), max(as.Date(d1$StartDate))))
cat(sprintf("  Follow-up (returners):       %s to %s\n",
            min(as.Date(d2_matched$StartDate_t2)), max(as.Date(d2_matched$StartDate_t2))))

subsection("Follow-up lag (start of follow-up minus start of round 1)")
d_lag <- d2_matched |>
  filter(
    !is.na(belief_meat_t2), !is.na(belief_charity_t2),
    !is.na(voucher_is_veg_t2), !is.na(donation_cents_t2)
  ) |>
  inner_join(
    d1 |> select(participantId, StartDate_r1 = StartDate),
    by = "participantId"
  )
fu_lag_days <- as.numeric(difftime(d_lag$StartDate_t2, d_lag$StartDate_r1, units = "days"))
fu_lag_q <- quantile(fu_lag_days, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
n_fu_lag_na <- sum(is.na(fu_lag_days))
cat("Decimal days. Follow-up completers with both beliefs and both behaviours.\n")
if (n_fu_lag_na > 0) {
  cat(sprintf("Missing a start time: %d\n", n_fu_lag_na))
}
print(
  data.frame(
    min = round(fu_lag_q[["0%"]], 2),
    Q1 = round(fu_lag_q[["25%"]], 2),
    median = round(fu_lag_q[["50%"]], 2),
    mean = round(mean(fu_lag_days, na.rm = TRUE), 2),
    Q3 = round(fu_lag_q[["75%"]], 2),
    max = round(fu_lag_q[["100%"]], 2),
    sd = round(sd(fu_lag_days, na.rm = TRUE), 2)
  ),
  row.names = FALSE
)
cat(sprintf("n = %d\n", sum(!is.na(fu_lag_days))))
