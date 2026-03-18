# =============================================================================
# 01 · Data Wrangling
# =============================================================================
# Loads raw Qualtrics CSVs (initial + follow-up), creates analysis variables,
# defines sample stages, and saves analysis datasets.
#
# Produces (saved to data/):
#   d1_all.rds   - all initial-study rows
#   d1_itt.rds   - treated + passed attention check (ITT sample)
#   d1.rds       - ITT + finished survey (final initial sample, N ≈ 500)
#   d2.rds       - finished follow-up participants
#   d.rds        - d1 merged with follow-up data (N ≈ 372)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)

source(here::here("scripts", "functions", "read_csv_qualtrics.R"))
source(here::here("scripts", "functions", "helpers.R"))

# =============================================================================
# INITIAL STUDY
# =============================================================================

d1_raw <- read_csv_qualtrics(
  here::here("data", "study1_initial.csv"),
  show_col_types = FALSE
)

d1_all <- d1_raw |>

  # --- Demographics ----------------------------------------------------------
  mutate(
    gender = case_match(
      gender, 1 ~ "Man", 2 ~ "Woman", 3 ~ "Other", 4 ~ "Prefer not to say"
    ),
    gender_recoded = case_when(
      gender == "Man"   ~ "Man",
      gender == "Woman" ~ "Woman",
      is.na(gender) | gender == "Prefer not to say" ~ NA_character_,
      TRUE ~ "Other"
    ),
    edu_cat = factor(education, levels = 1:7, labels = edu_labels, ordered = TRUE),
    education = ifelse(education < 7, education, NA_integer_),
    income_cat = factor(income, levels = 1:19, labels = income_labels, ordered = TRUE),
    income_numeric = income_numeric(income),
    pol_orientation = politics_1
  ) |>
  mutate(multi_hot_df(race, race_codes, "race_")) |>

  # --- Pre-treatment covariates ----------------------------------------------
  mutate(
    crt_batball_cor    = CRT_1 == 0.05 | CRT_1 == 5,
    AOT_stick_opinions = 8 - AOT_1_1,
    AOT_change_weak    = 8 - AOT_2_1,
    AOT_mean           = (AOT_stick_opinions + AOT_change_weak) / 2
  ) |>

  # --- Condition & main measures ---------------------------------------------
  mutate(
    condition = factor(ifelse(grepl("meat", moralBelief), "Meat", "Charity")),
    meat_wrong_pre       = ifelse(condition == "Meat", Initial_Belief_1, Initial_Belief_Other_1),
    charity_wrong_pre    = ifelse(condition == "Charity", Initial_Belief_1, Initial_Belief_Other_1),
    meat_wrong_post      = ifelse(condition == "Meat", Final_Belief_1, Final_Belief_Other_1),
    charity_wrong_post   = ifelse(condition == "Charity", Final_Belief_1, Final_Belief_Other_1),
    meat_wrong_change    = meat_wrong_post - meat_wrong_pre,
    charity_wrong_change = charity_wrong_post - charity_wrong_pre,
    burger_voucher       = ifelse(voucher_DV == 1, "meat", "meatless"),
    voucher_is_veg       = burger_voucher == "meatless",
    charity_donation_cents = donation_DV_1,
    charity_donation_perc  = charity_donation_cents / 20
  ) |>

  # --- Free-text reasons & AI argument --------------------------------------
  mutate(
    meat_reasons              = ifelse(condition == "Meat", userInitialReasoning, NA),
    meat_reasons_summarised   = ifelse(condition == "Meat", SummarizedInitialReasoning, NA),
    charity_reasons           = ifelse(condition == "Charity", userInitialReasoning, NA),
    charity_reasons_summarised = ifelse(condition == "Charity", SummarizedInitialReasoning, NA),
    meat_ai_argument          = ifelse(condition == "Meat", preliminaryArgument, NA),
    charity_ai_argument       = ifelse(condition == "Charity", preliminaryArgument, NA)
  ) |>

  # --- Post-treatment perception items ---------------------------------------
  rename(
    ai_understood  = understood_1,
    ai_respectful  = respectful_1,
    ai_originality = originality_1
  ) |>

  # --- Attention check -------------------------------------------------------
  mutate(attncheck_passed = attention == 1) |>

  # --- Parse dates -----------------------------------------------------------
  mutate(across(contains("Date"), ~ mdy_hm(.x, tz = "UTC")))

# --- Define sample stages ----------------------------------------------------
d1_treated <- d1_all |> filter(!is.na(prev_meat))
d1_itt     <- d1_treated |> filter(attncheck_passed)
d1         <- d1_itt |> filter(Finished == 1)

# =============================================================================
# FOLLOW-UP STUDY
# =============================================================================

d2_raw <- read_csv_qualtrics(
  here::here("data", "study1_followup.csv"),
  show_col_types = FALSE
)

d2_all <- d2_raw |>
  left_join(d1 |> select(PROLIFIC_PID, condition), by = "PROLIFIC_PID") |>
  filter(!is.na(condition)) |>
  mutate(
    cond2 = factor(ifelse(grepl("meat", moralBelief), "Meat", "Charity")),
    meat_wrong_t2             = ifelse(cond2 == "Meat", Initial_Belief_1, Initial_Belief_Other_1),
    charity_wrong_t2          = ifelse(cond2 == "Charity", Initial_Belief_1, Initial_Belief_Other_1),
    burger_voucher_t2         = ifelse(voucher_DV == 1, "meat", "meatless"),
    voucher_is_veg_t2         = burger_voucher_t2 == "meatless",
    charity_donation_cents_t2 = donation_DV_1,
    charity_donation_perc_t2  = charity_donation_cents_t2 / 20
  ) |>
  select(-cond2) |>
  mutate(across(c(StartDate, EndDate), ~ mdy_hm(.x, tz = "UTC"), .names = "{.col}_t2"))

d2 <- d2_all |> filter(Finished == 1)

# =============================================================================
# COMBINED (initial + follow-up)
# =============================================================================

d_all <- d1 |>
  left_join(d2 |> select(PROLIFIC_PID, contains("t2")), by = "PROLIFIC_PID")

d <- d_all |> filter(!is.na(meat_wrong_t2))

# =============================================================================
# SAVE
# =============================================================================

saveRDS(d1_all, here::here("data", "d1_all.rds"))
saveRDS(d1_itt, here::here("data", "d1_itt.rds"))
saveRDS(d1,     here::here("data", "d1.rds"))
saveRDS(d2,     here::here("data", "d2.rds"))
saveRDS(d,      here::here("data", "d.rds"))

cat(sprintf(paste0(
  "Data wrangling complete.\n",
  "  All initial rows:      %d\n",
  "  Reached treatment:     %d\n",
  "  ITT sample:            %d\n",
  "  Final initial sample:  %d\n",
  "  Follow-up completers:  %d\n",
  "  Combined sample:       %d\n"
), nrow(d1_all), nrow(d1_treated), nrow(d1_itt), nrow(d1), nrow(d2), nrow(d)))
