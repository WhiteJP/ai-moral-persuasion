# =============================================================================
# 01 · Data Wrangling
# =============================================================================
# Loads raw Qualtrics CSVs (initial + follow-up), creates analysis variables,
# defines sample stages, and saves analysis datasets.
#
# Produces (saved to data/study_2/):
#   study2_d1_all.rds         - unique participants after session dedupe
#   study2_d1_attnpassed.rds  - passed the attention check
#   study2_d1_treated.rds     - assigned a condition
#   study2_d1_itt.rds         - treated + passed the bot check
#   study2_d1.rds             - completers (final initial sample)
#   study2_d2_fu_invited.rds  - completers invited to the one-month follow-up
#   study2_d2_matched.rds     - every follow-up returner matched to a round 1 completer
#   study2_d2.rds             - those returners with both beliefs, the voucher,
#                               and the donation
#   study2_d.rds              - those returners, round 1 row plus follow-up outcomes
#   study2_conversations.csv  - one row per completer, chat turns in wide form
#   study2_d_antisocial.rds   - separate recontact of the lying and political-violence
#                               arms, one row per respondent, chats as JSON
#   study2_antisocial_fu_conversations.csv - recontact chats, turns in wide form
#   output/study_2/d1_fu_invited_cloudconnect_ids.csv - platform IDs for that invite list

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(forcats)
library(purrr)

source(here::here("scripts", "functions", "read_csv_qualtrics.R"))
source(here::here("scripts", "functions", "helpers.R"))


#### shared vars
conditions <- c("control", "meat", "charity", "polviol", "lying")
condition_labels <- c("Control", "Meat", "Charity", "Political Violence", "Lying")

orders <- 1:6
order_labels <- c(
  "meat → charity → lying", "meat → lying → charity", "charity → meat → lying",
  "charity → lying → meat", "lying → meat → charity", "lying → charity → meat"
)


seconds_to_minsecs <- function(secs) {
  out <- sprintf("%dm %ds", secs %/% 60, round(secs %% 60))
  out[is.na(secs)] <- NA_character_
  out
}

# Classify checkCDP() cdpMethod strings (first matching test wins in the JS)
cdp_method_class <- function(m) {
  m <- as.character(m)
  dplyr::case_when(
    is.na(m) | !nzchar(m) ~ "unknown",
    startsWith(m, "Multiple indicators") ~ "multiple_indicators",
    startsWith(m, "Chrome runtime") ~ "chrome_runtime",
    startsWith(m, "CDP timing") ~ "timing",
    grepl("webdriver", m, ignore.case = TRUE) ~ "webdriver",
    grepl("stack leak|Runtime.enable", m) ~ "stack_leak",
    TRUE ~ "other"
  )
}

# Keep one Qualtrics session per participant_id. Preference order:
#   1. failed attention check (screenout; drop from ITT per prereg)
#   2. assigned a condition (reached treatment)
#   3. if two treated sessions, flag and keep the farther-along row
#   4. higher Progress (pre-treatment dropouts)
# Missing participant_id rows are left as-is.
dedupe_participant_sessions <- function(d) {
  na_id <- filter(d, is.na(participant_id))
  ranked <- d |>
    filter(!is.na(participant_id)) |>
    mutate(.row_id = row_number()) |>
    arrange(
      participant_id,
      desc(coalesce(attention == 0, FALSE)),
      desc(!is.na(condition)),
      desc(Progress),
      desc(coalesce(Finished == 1, FALSE)),
      desc(StartDate),
      .row_id
    ) |>
    distinct(participant_id, .keep_all = TRUE) |>
    select(-.row_id)
  bind_rows(na_id, ranked)
}

# =============================================================================
# INITIAL STUDY
# =============================================================================

d1_raw <- read_csv_qualtrics(
  here::here("data", "study_2", "AI_Moral_Persuasion_Study_2.csv"),
  show_col_types = FALSE,
  # ensure all chathistories are read in as characters
  col_types = cols(
    chathistory1 = col_character(),
    chathistory2 = col_character(),
    chathistory3 = col_character(),
    chathistory4 = col_character(),
    chathistory5 = col_character(),
    chathistory6 = col_character()
  )
)


d1_all <- d1_raw |>
  # --- Remove _1 suffix from vars --------------------------------------------
  select(-any_of(
    # these embedded-data copies are less complete than the actual Qs
    # as we have these even for dropouts before embedded data written
    c("summer_v_winter_pre", "belief_meat_pre", "belief_charity_pre", "belief_lying_pre", "belief_polviol_pre")
  )) |>
  rename_with(~ str_remove(.x, "_1$"), ends_with("_1")) |>
  # --- Demographics ----------------------------------------------------------
  mutate(
    # age on slider to exclude bots (later)
    age = Age_2,
    gender = case_match(
      gender, 1 ~ "Man", 2 ~ "Woman", 3 ~ "Other", 4 ~ "Prefer not to say"
    ),
    gender_recoded = case_when(
      gender == "Man" ~ "Man",
      gender == "Woman" ~ "Woman",
      is.na(gender) | gender == "Prefer not to say" ~ NA_character_,
      TRUE ~ "Other"
    ),
    edu_cat = factor(education, levels = 1:7, labels = edu_labels, ordered = TRUE),
    # 1–6 as numeric; 7 = prefer not to say
    education = ifelse(education %in% 1:6, as.integer(education), NA_integer_),
    income_cat = factor(income, levels = 1:19, labels = income_labels, ordered = TRUE),
    income_numeric = income_numeric(income),
    # 1–17 brackets as numeric; 18 DK, 19 PNS, Qualtrics -66 / other → NA
    income = ifelse(income %in% 1:17, as.integer(income), NA_integer_),
    pol_orientation = pol_ideo,
    # party_id (raw 1–4): 1 Democrat, 2 Republican, 3 Independent, 4 Other (write-in)
    # party_id      — 4-levels (Other stays Other)
    # party_id_text — labels for 1–3, raw write-in for Other
    # party_id_detailed  — recode write-ins into granular party-ID buckets (not ideology)
    # party_id_collapsed — Green / Dem. socialist / Communist / Leftist-progressive → Other left;
    #                      Libertarian + Conservative → Other right;
    #                      Unaffiliated + Apolitical → Unaffiliated
    party_id_text = case_when(
      party_id == 1L ~ "Democrat",
      party_id == 2L ~ "Republican",
      party_id == 3L ~ "Independent",
      party_id == 4L ~ na_if(str_squish(party_id_4_TEXT), ""),
      TRUE ~ NA_character_
    ),
    party_id_detailed = factor(
      case_when(
        party_id %in% 1:3 ~ party_id_text,
        is.na(party_id_text) ~ NA_character_,
        str_detect(str_to_lower(party_id_text), "libertarian") ~ "Libertarian",
        str_detect(str_to_lower(party_id_text), "conservative") ~ "Other right",
        str_detect(str_to_lower(party_id_text), "green|environmentalist") ~ "Green",
        str_detect(str_to_lower(party_id_text), "dsa|democratic socialist") ~ "Democratic socialist",
        str_detect(str_to_lower(party_id_text), "communist|psl") ~ "Communist",
        str_detect(
          str_to_lower(party_id_text),
          "leftist|progressive|socialist|working famil"
        ) ~ "Leftist/progressive",
        str_detect(
          str_to_lower(party_id_text),
          "apolit|dont vote|i hate politics|^idc$|n/p|non political"
        ) ~ "Apolitical",
        str_detect(
          str_to_lower(party_id_text),
          paste(
            "none|unaffil|neither|niether|neutral|no party|nonpart",
            "no pref|undecid|unknown|^na$|refuse",
            sep = "|"
          )
        ) ~ "Unaffiliated",
        TRUE ~ "Other"
      ),
      levels = c(
        "Democrat", "Republican", "Independent",
        "Libertarian", "Green", "Democratic socialist", "Communist",
        "Leftist/progressive", "Other right",
        "Unaffiliated", "Apolitical", "Other"
      )
    ),
    party_id_collapsed = factor(
      case_when(
        party_id_detailed %in% c("Democrat", "Republican", "Independent") ~
          as.character(party_id_detailed),
        party_id_detailed %in% c("Libertarian", "Other right") ~ "Other right",
        party_id_detailed %in% c(
          "Green", "Democratic socialist", "Communist", "Leftist/progressive"
        ) ~ "Other left",
        party_id_detailed %in% c("Unaffiliated", "Apolitical") ~ "Unaffiliated",
        TRUE ~ "Other"
      ),
      levels = c(
        "Democrat", "Republican", "Independent",
        "Other left", "Other right", "Unaffiliated", "Other"
      )
    ),
    # 7-point party ID (ANES-style). Use numeric party_id (1–4) before converting
    # to a labelled factor: 1 strong Dem … 7 strong Rep.
    dem_rep_num = case_when(
      party_id == 1L ~ as.numeric(dem_strength),              # 1 strong dem, 2 not very strong
      party_id %in% c(3L, 4L) & party_lean == 2 ~ 3,          # leans dem
      party_id %in% c(3L, 4L) & party_lean == 3 ~ 4,          # no lean
      party_id %in% c(3L, 4L) & party_lean == 1 ~ 5,          # leans rep
      party_id == 2L & rep_strength == 2 ~ 6,                 # not very strong rep
      party_id == 2L & rep_strength == 1 ~ 7                  # strong rep
    ),
    party_id = factor(
      party_id,
      levels = 1:4,
      labels = c("Democrat", "Republican", "Independent", "Other")
    )
  ) |>
  # Qualtrics writes this multi-select as one column per choice (1 = selected,
  # otherwise missing). rename_with() above turns race_1 into race.
  # These choice numbers are not the Study 1 comma-separated codes.
  mutate(
    race_white            = as.integer(race == 1),
    race_black            = as.integer(race_2 == 1),
    race_asian            = as.integer(race_3 == 1),
    race_native_american  = as.integer(race_4 == 1),
    race_pacific_islander = as.integer(race_5 == 1),
    race_hispanic         = as.integer(race_6 == 1),
    race_middle_eastern   = as.integer(race_7 == 1),
    race_other            = as.integer(race_8 == 1)
  ) |>
  mutate(across(
    c(
      race_white, race_black, race_asian, race_native_american,
      race_pacific_islander, race_hispanic, race_middle_eastern, race_other
    ),
    ~ coalesce(.x, 0L)
  )) |>
  select(-race, -race_2, -race_3, -race_4, -race_5, -race_6, -race_7, -race_8) |>
  # --- Pre-treatment covariates ----------------------------------------------
  mutate(
    crt_batball_cor    = CRT == 0.05 | CRT == 5,
    AOT_stick_opinions = 8 - AOT_1,
    AOT_change_weak    = 8 - AOT_2,
    AOT_mean           = (AOT_stick_opinions + AOT_change_weak) / 2,
    trust_ai_pre       = trust_ai_pre
  ) |>
  # --- Condition & main measures ---------------------------------------------
  mutate(
    condition = factor(condition, levels = conditions),
    belief_meat_change = belief_meat_post - belief_meat_pre,
    belief_charity_change = belief_charity_post - belief_charity_pre,
    belief_lying_change = belief_lying_post - belief_lying_pre,
    belief_polviol_change = belief_polviol_post - belief_polviol_pre,
    # The belief item that matches the assigned conversation. Control is NA.
    cond_belief_pre = case_when(
      condition == "meat" ~ belief_meat_pre,
      condition == "charity" ~ belief_charity_pre,
      condition == "lying" ~ belief_lying_pre,
      condition == "polviol" ~ belief_polviol_pre
    ),
    cond_belief_post = case_when(
      condition == "meat" ~ belief_meat_post,
      condition == "charity" ~ belief_charity_post,
      condition == "lying" ~ belief_lying_post,
      condition == "polviol" ~ belief_polviol_post
    ),
    # Qualtrics recodes both display orders so 0 = beef, 1 = veggie
    voucher_raw = coalesce(voucher_DV_beeffirst, voucher_DV_vegfirst),
    voucher_choice = factor(voucher_raw, levels = 0:1, labels = c("beef", "veggie")),
    voucher_is_veg = as.integer(voucher_raw == 1),
    donation_cents = donation_DV,
    donation_perc = donation_cents / 20,
    pv_participation = pv_participation,
    # Reverse punishment (1 = no criminal charges) so the composite is
    # pro-violence (justification, support, and reversed punishment).
    pv_punishment_rev = 1 - pv_punishment,
    pv_composite = as.numeric(
      (scale(pv_justification) + scale(pv_support) + scale(pv_punishment_rev)) / 3
    ),
    # 0 = tails (honest; coin is predetermined tails), 1 = heads (lied)
    coin_flip = factor(lying_DV, levels = 0:1, labels = c("tails", "heads")),
    coin_heads = as.integer(lying_DV == 1)
  ) |>
  # --- Free-text reasons -----------------------------------------------------
  # note. we extract AI conversations later
  rename(
    initial_reasons_control = Control_Reasoning,
    inital_reasons_treatment = Initial_Reasoning
  ) |>
  mutate(
    reasons = coalesce(initial_reasons_control, inital_reasons_treatment)
  ) |>
  # --- Order conditions ------------------------------------------------------
  mutate(
    order = factor(order, levels = orders),
    order_labelled = forcats::lvls_revalue(order, order_labels)
  ) |>
  # --- Post-treatment perception items ---------------------------------------
  rename(
    ai_understood  = understood,
    ai_respectful  = respectful,
    ai_originality = originality
  ) |>
  # --- Attention check -------------------------------------------------------
  mutate(attncheck_passed = attention == 1) |>
  # --- Bot checks -------------------------------------------------------------
  mutate(
    # Qualtrics session metadata (once at start), not stored in per-page __js_metaObj
    browser = factor(meta_Browser),
    botchecks_parsed_json = purrr::map(`__js_metaObj`, ~ {
      if (is.na(.x) || !nzchar(.x)) NULL else jsonlite::fromJSON(.x)
    }),
    # general touchscreen check, age specific one for prereg below
    # (they almost always agree, and never don't agree when moves < 10,
    # which would affect itt sample)
    # TRUE if any page they reached reported touchscreen.
    is_touchscreen = purrr::map_lgl(botchecks_parsed_json, function(parsed) {
      if (is.null(parsed) || length(parsed) == 0L) {
        return(NA)
      }
      any(purrr::map_lgl(parsed, function(p) {
        isTRUE(as.numeric(p$touchscreen) >= 1)
      }))
    }),
    botchecks = purrr::map(botchecks_parsed_json, ~ {
      if (is.null(.x) || length(.x) == 0L) {
        NULL
      } else {
        rlang::set_names(.x, paste0(names(.x), "_botchecks"))
      }
    }),
    botchecks_to_unnest = botchecks
  ) |>
  tidyr::unnest_wider(botchecks_to_unnest) |>
  # exploratory/descriptive botchecks
  mutate(
    # Exploratory CDP summary (not preregistered): any page where checkCDP() fired
    cdp_hits = purrr::map(botchecks_parsed_json, function(parsed) {
      if (is.null(parsed) || length(parsed) == 0L) {
        return(NULL)
      }
      fired <- parsed[purrr::map_lgl(parsed, function(p) {
        isTRUE(as.numeric(p$cdp) >= 1)
      })]
      if (length(fired) == 0L) {
        return(character(0))
      }
      purrr::imap_chr(fired, function(p, nm) {
        method <- p$cdpMethod
        if (is.null(method) || !nzchar(method)) method <- "unknown"
        paste0(nm, ": ", method)
      })
    }),
    # CDP detected: had any page where checkCDP() fired
    cdp_detected = purrr::map_lgl(cdp_hits, function(x) {
      if (is.null(x)) NA else length(x) > 0L
    }),
    # CDP detected count: number of pages where checkCDP() fired
    cdp_detected_count = purrr::map_int(cdp_hits, function(x) {
      if (is.null(x)) NA_integer_ else length(x)
    }),
    # CDP reasons: "page: method | page: method"
    cdp_reasons = purrr::map_chr(cdp_hits, function(x) {
      if (is.null(x) || length(x) == 0L) NA_character_ else paste(x, collapse = " | ")
    }),
    # total number of pages with checkCDP() that participant reached
    cdp_n_pages = purrr::map_int(botchecks_parsed_json, function(parsed) {
      if (is.null(parsed)) NA_integer_ else length(parsed)
    }),
    # Unique method classes among pages that fired (empty if none)
    cdp_method_classes = purrr::map(cdp_hits, function(x) {
      if (is.null(x) || length(x) == 0L) {
        character(0)
      } else {
        unique(cdp_method_class(sub("^[^:]+: ", "", x)))
      }
    }),
    cdp_n_unique_methods = purrr::map_int(cdp_method_classes, length),
    # CDP fired on every page they reached 89% of those with any hits
    # others basically always fire once (10%), and almost always timing anomaly, so will treat as outlier
    # and just use cbp persistent
    cdp_persistent = dplyr::case_when(
      is.na(cdp_detected) ~ NA,
      TRUE ~ cdp_detected & cdp_detected_count == cdp_n_pages
    ),
    # Same reason on every firing page (TRUE only if there was at least one hit)
    cdp_same_reason = dplyr::case_when(
      is.na(cdp_detected) ~ NA,
      !cdp_detected ~ NA,
      TRUE ~ cdp_n_unique_methods & cdp_persistent == 1L
    ),
    # everyone persistent has the same reason each time, so we will classify those further
    # multiple indicators needs two of: no plugins, no languages, headless UA, navigator.webdriver !== undefined.
    # In ordinary Chrome, webdriver is false, and false !== undefined is true, so automationControlled is already on.
    # Empty plugins is also common. Those two together are the usual payload (noPlugins + automationControlled).
    # so when this fires it is likely to be “recent Chrome,” not “Puppeteer” or selenium or other automation.
    # 97.6% of people with this were chrome
    cdp_persistent_multiple_indicators = dplyr::case_when(
      is.na(cdp_persistent) ~ NA,
      TRUE ~ cdp_persistent &
        purrr::map_lgl(cdp_method_classes, ~ "multiple_indicators" %in% .x)
    ),
    # This tests if chrome.runtime exists but id is missing.
    # That is rarer (~2%), more specific, and not the known method-4 false positive.
    # It can still be Brave, some Chromium forks, or an extension stub.
    # Treat it as “unusual Chrome object,” not “automated.”
    cdp_persistent_chrome_runtime = dplyr::case_when(
      is.na(cdp_persistent) ~ NA,
      TRUE ~ cdp_persistent &
        purrr::map_lgl(cdp_method_classes, ~ "chrome_runtime" %in% .x)
    ),
    # More diagnostic checkCDP() tests, among persistent only.
    # Timing people are not persistent, so webdriver is not first-match-masked here.
    # Persistent + MI or chrome-runtime already means tests 1–3 lost on every page.
    # If stack leak / webdriver had won, they would be the persistent method instead.
    # nobody has any of these
    cdp_persistent_stack_leak = dplyr::case_when(
      is.na(cdp_persistent) ~ NA,
      TRUE ~ cdp_persistent &
        purrr::map_lgl(cdp_method_classes, ~ "stack_leak" %in% .x)
    ),
    cdp_persistent_webdriver = dplyr::case_when(
      is.na(cdp_persistent) ~ NA,
      TRUE ~ cdp_persistent &
        purrr::map_lgl(cdp_method_classes, ~ "webdriver" %in% .x)
    ),
    # most diagnostic of bots would be both stack leak and webdriver
    cdp_persistent_diagnostic = dplyr::case_when(
      is.na(cdp_persistent) ~ NA,
      TRUE ~ cdp_persistent_stack_leak | cdp_persistent_webdriver
    )
  ) |>
  # overall cbp metrics
  # ~6% persistent firing
  # ~4% of that is likely false positives
  # ~2% of that is chrome runtime check, which is likely anomaly
  # ~0% on more diagnostic check

  # preregistered botchecks, age slider moves < 10 AND not touchscreen = fail
  mutate(
    age_moves = purrr::map_dbl(demos_age_botchecks, function(x) {
      if (is.null(x) || is.null(x$moves)) NA_real_ else as.numeric(x$moves)
    }),
    age_is_touchscreen = purrr::map_lgl(demos_age_botchecks, function(x) {
      if (is.null(x) || is.null(x$touchscreen)) {
        NA
      } else {
        isTRUE(as.numeric(x$touchscreen) >= 1)
      }
    }),
    botcheck_passed = !(age_moves < 10 & !age_is_touchscreen),
  ) |>
  # --- Parse dates and times -----------------------------------------------------------
  mutate(across(contains("Date"), ~ lubridate::ymd_hms(.x, tz = "UTC"))) |>
  mutate(
    duration_seconds_start_end = as.numeric(difftime(EndDate, StartDate, units = "secs")),
    duration_seconds_as_recorded = `Duration (in seconds)`,
    duration_seconds_difference = duration_seconds_start_end - duration_seconds_as_recorded,
    # difference is 0-1 seconds, so just use recorded duration
    duration_seconds = duration_seconds_as_recorded,
    duration_min_secs = seconds_to_minsecs(duration_seconds)
  )

## remove duplicate participant_ids (same people starting multiple sessions)
all_pids <- d1_all |> pull(participant_id) # 3462 total
unique_pids <- all_pids[!duplicated(all_pids)] # 3438 total - 24 duplicates
dup_pids <- all_pids[duplicated(all_pids)] # all 24 duplicates occur only once

## resolve dupes by (in this order)
# 1. failed attention check preferred - if failed not attentive participant, and removed as per prereg
# 2. assigned to treatment !is.na(condition) preferred over is.na(condition), as then part of itt
# 3. if two separate treatments, flag (none in this data)
# 4. if both not treated, take one with most pages reached(pre-treatment dropouts anyway)
dup_resolution <- d1_all |>
  filter(!is.na(participant_id), participant_id %in% unique(dup_pids)) |>
  group_by(participant_id) |>
  summarise(
    n_rows = n(),
    n_attn_fail = sum(attention == 0, na.rm = TRUE),
    n_treated = sum(!is.na(condition)),
    n_conditions = n_distinct(condition[!is.na(condition)]),
    max_progress = max(Progress, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    resolved_by = case_when(
      n_attn_fail > 0 ~ "1 attention-fail preferred",
      n_treated == 1 ~ "2 treated preferred over NA",
      n_treated >= 2 ~ "3 two treatments (flagged)",
      TRUE ~ "4 most Progress (both untreated)"
    )
  )

resolved_by_levels <- c(
  "1 attention-fail preferred",
  "2 treated preferred over NA",
  "3 two treatments (flagged)",
  "4 most Progress (both untreated)"
)
dup_resolution_counts <- dup_resolution |>
  mutate(resolved_by = factor(resolved_by, levels = resolved_by_levels)) |>
  count(resolved_by, name = "n_pids", .drop = FALSE)
cat("Duplicate participant_ids:", nrow(dup_resolution), "PIDs (", length(dup_pids), "extra rows)\n")
print(dup_resolution_counts, n = Inf)

two_treatments <- dup_resolution |> filter(resolved_by == "3 two treatments (flagged)")
if (nrow(two_treatments) > 0) {
  warning(
    nrow(two_treatments), " participant_id(s) have two treated sessions; ",
    "keeping the farther-along row. PIDs: ",
    paste(two_treatments$participant_id, collapse = ", "),
    call. = FALSE
  )
  print(
    d1_all |>
      filter(participant_id %in% two_treatments$participant_id) |>
      select(participant_id, StartDate, Progress, Finished, condition, attention),
    n = Inf
  )
}

d1_all_deduped <- dedupe_participant_sessions(d1_all)
cat(sprintf("After dedupe: %d rows\n", nrow(d1_all_deduped)))

# --- Define sample stages ----------------------------------------------------
# Stage sizes are printed at the end of this script. Do not hard-code them.
d1_attnpassed <- d1_all_deduped |> filter(attncheck_passed)
d1_treated <- d1_attnpassed |> filter(!is.na(condition))
d1_itt <- d1_treated |> filter(botcheck_passed)

# to get final completers sample as per prereg
# 1. exclude participants who didn't complete all 8 main outcomes measures (last of which is always pv_participation)
# 2. exclude participants who were assigned treatment, but had conversation error -- didn't receive at least one message from the AI

# 2972 completers
d1 <- d1_itt |>
  filter(!is.na(pv_participation)) |> # didn't complete all 8 main outcomes measures
  # empty or junk first-chat (not a saved conversation; real chats contain "assistant")
  filter(!is.na(chathistory1), str_detect(chathistory1, "assistant"))

# =============================================================================
# FOLLOW-UP INVITE LIST
# =============================================================================
# Completers in the one-month follow-up arms. Lying and political violence
# are not recontacted.

d2_fu_invited <- d1 |>
  filter(condition %in% c("control", "meat", "charity"))

# Platform IDs passed into Qualtrics (participant, assignment, project).
fu_id_export <- d2_fu_invited |>
  select(participant_id, assignmentId, projectId) |>
  arrange(participant_id)

fu_export_path <- here::here("output", "study_2", "d1_fu_invited_cloudconnect_ids.csv")
dir.create(dirname(fu_export_path), recursive = TRUE, showWarnings = FALSE)
write_csv(fu_id_export, fu_export_path)

cat(sprintf(
  "Follow-up invite list: %d completers (control / meat / charity)\n",
  nrow(d2_fu_invited)
))
print(count(d2_fu_invited, condition), n = Inf)
cat(sprintf("Platform IDs written to %s\n", fu_export_path))

# =============================================================================
# FOLLOW-UP STUDY
# =============================================================================

d2_raw <- read_csv_qualtrics(
  here::here(
    "data", "study_2",
    "AI_Moral_Persuasion_Study_2_Meat_Charity_Followup.csv"
  ),
  show_col_types = FALSE
)

# One row per person. Keep the earliest StartDate among sessions that have
# both beliefs, the voucher, and the donation. A later finished session does
# not replace an earlier one that already has those measures. If none of a
# person's sessions have the measures, keep the earliest start.
d2_raw_deduped <- d2_raw |>
  mutate(
    .row_id = row_number(),
    .measures_done = !is.na(belief_meat_1) & !is.na(belief_charity_1) &
      !is.na(donation_DV_1) &
      (!is.na(voucher_DV_beeffirst) | !is.na(voucher_DV_vegfirst))
  ) |>
  arrange(
    participant_id,
    desc(.measures_done),
    StartDate,
    .row_id
  ) |>
  distinct(participant_id, .keep_all = TRUE) |>
  select(-.row_id, -.measures_done)

n_d2_dupes <- nrow(d2_raw) - nrow(d2_raw_deduped)
if (n_d2_dupes > 0) {
  cat(sprintf(
    "Follow-up dedupe: dropped %d extra session(s); kept the earliest start with both beliefs, the voucher, and the donation.\n",
    n_d2_dupes
  ))
}

d2_all <- d2_raw_deduped |>
  rename_with(~ str_remove(.x, "_1$"), ends_with("_1")) |>
  rename(
    ResponseId_t2 = ResponseId,
    StartDate_t2 = StartDate,
    EndDate_t2 = EndDate,
    RecordedDate_t2 = RecordedDate,
    Finished_t2 = Finished,
    Progress_t2 = Progress,
    order_t2 = order,
    pos1_t2 = pos1,
    pos2_t2 = pos2,
    assignmentId_t2 = assignmentId,
    projectId_t2 = projectId,
    feedback_t2 = feedback,
    duration_seconds_t2 = `Duration (in seconds)`
  ) |>
  mutate(
    across(
      c(StartDate_t2, EndDate_t2, RecordedDate_t2),
      ~ if (inherits(.x, "POSIXt")) lubridate::with_tz(.x, "UTC") else lubridate::ymd_hms(.x, tz = "UTC")
    ),
    # Same recode as round 1: 0 = beef, 1 = veggie, in either display order.
    voucher_raw_t2 = coalesce(voucher_DV_beeffirst, voucher_DV_vegfirst),
    voucher_choice_t2 = factor(voucher_raw_t2, levels = 0:1, labels = c("beef", "veggie")),
    voucher_is_veg_t2 = as.integer(voucher_raw_t2 == 1),
    belief_meat_t2 = belief_meat,
    belief_charity_t2 = belief_charity,
    donation_cents_t2 = donation_DV,
    donation_perc_t2 = donation_cents_t2 / 20,
    prev_meat_t2 = suppressWarnings(as.numeric(prev_meat)),
    prev_donate_t2 = suppressWarnings(as.numeric(prev_donate)),
    order_t2 = factor(as.character(order_t2), levels = c("1", "2")),
    order_labelled_t2 = forcats::lvls_revalue(
      order_t2, c("meat → charity", "charity → meat")
    )
  )

n_prev_meat_na <- sum(is.na(d2_all$prev_meat_t2) & !is.na(d2_all$prev_meat))
n_prev_donate_na <- sum(is.na(d2_all$prev_donate_t2) & !is.na(d2_all$prev_donate))
if (n_prev_meat_na + n_prev_donate_na > 0) {
  warning(
    "Non-numeric follow-up behaviour text: ",
    n_prev_meat_na, " prev_meat, ", n_prev_donate_na, " prev_donate.",
    call. = FALSE
  )
}
d2_matched <- d2_all |>
  inner_join(
    d1 |> select(participant_id, condition, belief_meat_pre, belief_charity_pre),
    by = "participant_id"
  ) |>
  mutate(
    belief_meat_change_t2 = belief_meat_t2 - belief_meat_pre,
    belief_charity_change_t2 = belief_charity_t2 - belief_charity_pre
  )

n_unmatched <- nrow(d2_all) - nrow(d2_matched)
if (n_unmatched > 0) {
  warning(
    n_unmatched, " follow-up participant_id(s) are not in the round 1 completer sample.",
    call. = FALSE
  )
}

fu_both_beliefs <- !is.na(d2_matched$belief_meat_t2) & !is.na(d2_matched$belief_charity_t2)
fu_both_behav <- !is.na(d2_matched$voucher_is_veg_t2) & !is.na(d2_matched$donation_cents_t2)
n_fu_returned <- nrow(d2_matched)
n_fu_beliefs <- sum(fu_both_beliefs)
n_fu_behav <- sum(fu_both_behav)

n_not_invited <- sum(!d2_matched$participant_id %in% d2_fu_invited$participant_id)
if (n_not_invited > 0) {
  warning(
    n_not_invited, " matched follow-up participant_id(s) were not on the invite list.",
    call. = FALSE
  )
}

# Matched to a round 1 completer, and the follow-up session has both beliefs,
# the voucher, and the donation. Qualtrics Finished is not required: the
# self-reports and the last page can be blank.
d2 <- d2_matched |>
  filter(
    !is.na(belief_meat_t2),
    !is.na(belief_charity_t2),
    !is.na(voucher_is_veg_t2),
    !is.na(donation_cents_t2)
  ) |>
  select(
    participant_id, condition,
    belief_meat_pre, belief_charity_pre,
    belief_meat_t2, belief_charity_t2,
    belief_meat_change_t2, belief_charity_change_t2,
    voucher_choice_t2, voucher_is_veg_t2,
    donation_cents_t2, donation_perc_t2,
    prev_meat_t2, prev_donate_t2,
    order_t2, order_labelled_t2, pos1_t2, pos2_t2,
    StartDate_t2, EndDate_t2, RecordedDate_t2,
    Finished_t2, Progress_t2, duration_seconds_t2,
    any_of("ResponseId_t2"), feedback_t2,
    assignmentId_t2, projectId_t2
  )

# =============================================================================
# COMBINED (initial + follow-up)
# =============================================================================
# Round 1 completers who returned. Follow-up columns are suffixed _t2 so
# condition, order, and the round 1 CloudResearch ids stay as they were.

d <- d1 |>
  inner_join(
    d2 |> select(participant_id, ends_with("_t2")),
    by = "participant_id"
  )

# =============================================================================
# ANTISOCIAL RECONTACT (lying and political violence)
# =============================================================================
# A separate check on the lying and political-violence arms, not part of the
# main experiment. Rows are recontact respondents only, and they are not
# counted in the round-1 or follow-up samples. Each row carries that
# person's round-1 measures so every timepoint sits side by side:
#   t0  round-1 pretreatment
#   t1  round-1 after the first conversation
#   t2  round-1 after the corrective conversation
#   t3  recontact, before the new conversation
#   t4  recontact, after the new conversation
# Vignette items were not asked at t0 or t2. The corrective conversation was
# only about the person's own arm, so the other arm's belief has no t2.
#
# Everyone gives the t3 belief (and, for political violence, the t3 vignette
# items). People at the top of the slider skip the reason, conversation, and
# t4 measures, which are left missing. ceiling_skip flags them.
#
# One row per person. Chats stay as Qualtrics JSON in chathistory1-6.

as_fu_paths <- c(
  lying = here::here(
    "data", "study_2",
    "AI_Moral_Persuasion_Study_2_Lying_Followup.csv"
  ),
  polviol = here::here(
    "data", "study_2",
    "AI_Moral_Persuasion_Study_2_PV_Followup.csv"
  )
)

read_as_fu <- function(path) {
  read_csv_qualtrics(
    path,
    show_col_types = FALSE,
    col_types = cols(
      chathistory1 = col_character(),
      chathistory2 = col_character(),
      chathistory3 = col_character(),
      chathistory4 = col_character(),
      chathistory5 = col_character(),
      chathistory6 = col_character(),
      .default = col_guess()
    )
  )
}

as_raw <- bind_rows(lapply(as_fu_paths, read_as_fu), .id = ".arm_file")

# Embedded copies of the pretreatment belief and reason. The question
# columns, suffixed _1, are the ones to keep.
as_raw <- as_raw |>
  select(-any_of(c(
    "belief_lying_pre", "belief_polviol_pre", "belief_condition_pre",
    "initial_reasoning_for_chat"
  ))) |>
  rename_with(~ str_remove(.x, "_1$"), ends_with("_1")) |>
  mutate(
    across(
      c(StartDate, EndDate, RecordedDate),
      ~ if (inherits(.x, "POSIXt")) lubridate::with_tz(.x, "UTC") else lubridate::ymd_hms(.x, tz = "UTC")
    ),
    # Sessions abandoned on the first pages have no embedded condition. Each
    # survey only went to one arm, so the file gives it.
    condition = coalesce(condition, .arm_file),
    .pre_done = !is.na(belief_lying_pre) | !is.na(belief_polviol_pre),
    .post_done = !is.na(belief_lying_post) | !is.na(belief_polviol_post)
  )

# One session per person. Prefer the earliest start, unless that session
# ended before the first belief and a later session has it — then take the
# earliest session that has the first belief.
as_deduped <- as_raw |>
  filter(!is.na(participant_id)) |>
  mutate(.row_id = row_number()) |>
  arrange(participant_id, desc(.pre_done), StartDate, .row_id) |>
  distinct(participant_id, .keep_all = TRUE) |>
  select(-.row_id)

n_as_no_pid <- sum(is.na(as_raw$participant_id))
n_as_dupes <- nrow(as_raw) - n_as_no_pid - nrow(as_deduped)

# People who left before giving the first belief are not respondents.
n_as_no_pre <- sum(!as_deduped$.pre_done)
as_deduped <- as_deduped |>
  filter(.pre_done) |>
  select(-.pre_done, -.post_done)
n_as_both_files <- as_raw |>
  distinct(participant_id, .arm_file) |>
  count(participant_id) |>
  filter(n > 1) |>
  nrow()
if (n_as_both_files > 0) {
  warning(
    n_as_both_files,
    " participant_id(s) appear in both the lying and political-violence recontact files.",
    call. = FALSE
  )
}

as_r1 <- d1 |>
  filter(condition %in% c("polviol", "lying")) |>
  transmute(
    participant_id,
    condition_r1 = as.character(condition),
    belief_lying_t0 = belief_lying_pre,
    belief_lying_t1 = belief_lying_post,
    belief_lying_t2 = belief_lying_post2,
    belief_polviol_t0 = belief_polviol_pre,
    belief_polviol_t1 = belief_polviol_post,
    belief_polviol_t2 = belief_polviol_post2,
    pv_support_t1 = pv_support,
    pv_justification_t1 = pv_justification,
    pv_participation_t1 = pv_participation,
    pv_punishment_t1 = pv_punishment
  )

d_as <- as_deduped |>
  select(-.arm_file) |>
  mutate(
    across(
      any_of(c(
        "belief_lying_pre", "belief_lying_post",
        "belief_polviol_pre", "belief_polviol_post",
        "pv_support", "pv_justification", "pv_participation", "pv_punishment",
        "pv_support_post", "pv_justification_post", "pv_participation_post", "pv_punishment_post"
      )),
      ~ suppressWarnings(as.numeric(.x))
    ),
    condition = factor(condition, levels = c("polviol", "lying")),
    ceiling_skip = ceiling_skip %in% c(1, "1"),
    party_id = factor(
      suppressWarnings(as.integer(party_id)),
      levels = 1:4,
      labels = c("Democrat", "Republican", "Independent", "Other")
    )
  ) |>
  rename(
    reasons = Initial_Reasoning,
    duration_seconds = `Duration (in seconds)`,
    belief_lying_t3 = belief_lying_pre,
    belief_lying_t4 = belief_lying_post,
    belief_polviol_t3 = belief_polviol_pre,
    belief_polviol_t4 = belief_polviol_post,
    pv_support_t3 = pv_support,
    pv_justification_t3 = pv_justification,
    pv_participation_t3 = pv_participation,
    pv_punishment_t3 = pv_punishment,
    pv_support_t4 = pv_support_post,
    pv_justification_t4 = pv_justification_post,
    pv_participation_t4 = pv_participation_post,
    pv_punishment_t4 = pv_punishment_post
  ) |>
  left_join(as_r1, by = "participant_id")

# Recontact invites went only to round-1 completers in these two arms.
n_as_unmatched <- sum(is.na(d_as$condition_r1))
if (n_as_unmatched > 0) {
  warning(
    n_as_unmatched,
    " recontact participant_id(s) are not round-1 lying or political-violence completers.",
    call. = FALSE
  )
}
n_as_condition_mismatch <- sum(
  !is.na(d_as$condition_r1) & as.character(d_as$condition) != d_as$condition_r1,
  na.rm = TRUE
)
if (n_as_condition_mismatch > 0) {
  warning(
    n_as_condition_mismatch,
    " recontact row(s) have a condition that does not match round 1.",
    call. = FALSE
  )
}

arm_belief <- function(lying, polviol) {
  case_when(
    d_as$condition == "lying" ~ lying,
    d_as$condition == "polviol" ~ polviol
  )
}

# _last is the latest measure of the person's own arm (belief, and for
# political violence the vignette items). last_measure is the timepoint of
# that measure.
d_as <- d_as |>
  select(-condition_r1) |>
  mutate(
    cond_belief_t0 = arm_belief(belief_lying_t0, belief_polviol_t0),
    cond_belief_t1 = arm_belief(belief_lying_t1, belief_polviol_t1),
    cond_belief_t2 = arm_belief(belief_lying_t2, belief_polviol_t2),
    cond_belief_t3 = arm_belief(belief_lying_t3, belief_polviol_t3),
    cond_belief_t4 = arm_belief(belief_lying_t4, belief_polviol_t4),
    last_measure = factor(
      case_when(
        !is.na(cond_belief_t4) ~ "t4",
        !is.na(cond_belief_t3) ~ "t3",
        !is.na(cond_belief_t2) ~ "t2",
        !is.na(cond_belief_t1) ~ "t1",
        !is.na(cond_belief_t0) ~ "t0"
      ),
      levels = c("t0", "t1", "t2", "t3", "t4")
    ),
    belief_lying_last = if_else(
      condition == "lying",
      coalesce(belief_lying_t4, belief_lying_t3, belief_lying_t2, belief_lying_t1, belief_lying_t0),
      NA_real_
    ),
    belief_polviol_last = if_else(
      condition == "polviol",
      coalesce(belief_polviol_t4, belief_polviol_t3, belief_polviol_t2, belief_polviol_t1, belief_polviol_t0),
      NA_real_
    ),
    # Highest own-arm belief across the recontact (t3, t4). last_max is the
    # last measure, except that a recontact last measure is replaced by
    # followup_max.
    belief_lying_followup_max = if_else(
      condition == "lying",
      pmax(belief_lying_t3, belief_lying_t4, na.rm = TRUE),
      NA_real_
    ),
    belief_polviol_followup_max = if_else(
      condition == "polviol",
      pmax(belief_polviol_t3, belief_polviol_t4, na.rm = TRUE),
      NA_real_
    ),
    belief_lying_last_max = if_else(
      last_measure %in% c("t3", "t4"),
      belief_lying_followup_max,
      belief_lying_last
    ),
    belief_polviol_last_max = if_else(
      last_measure %in% c("t3", "t4"),
      belief_polviol_followup_max,
      belief_polviol_last
    ),
    pv_support_last = if_else(
      condition == "polviol",
      coalesce(pv_support_t4, pv_support_t3, pv_support_t1),
      NA_real_
    ),
    pv_justification_last = if_else(
      condition == "polviol",
      coalesce(pv_justification_t4, pv_justification_t3, pv_justification_t1),
      NA_real_
    ),
    pv_participation_last = if_else(
      condition == "polviol",
      coalesce(pv_participation_t4, pv_participation_t3, pv_participation_t1),
      NA_real_
    ),
    pv_punishment_last = if_else(
      condition == "polviol",
      coalesce(pv_punishment_t4, pv_punishment_t3, pv_punishment_t1),
      NA_real_
    )
  ) |>
  relocate(
    participant_id, condition, ceiling_skip,
    cond_belief_t0, cond_belief_t1, cond_belief_t2, cond_belief_t3, cond_belief_t4,
    belief_lying_t0, belief_lying_t1, belief_lying_t2, belief_lying_t3, belief_lying_t4,
    belief_polviol_t0, belief_polviol_t1, belief_polviol_t2, belief_polviol_t3, belief_polviol_t4,
    pv_support_t1, pv_support_t3, pv_support_t4,
    pv_justification_t1, pv_justification_t3, pv_justification_t4,
    pv_participation_t1, pv_participation_t3, pv_participation_t4,
    pv_punishment_t1, pv_punishment_t3, pv_punishment_t4,
    last_measure,
    belief_lying_last, belief_polviol_last,
    belief_lying_followup_max, belief_polviol_followup_max,
    belief_lying_last_max, belief_polviol_last_max,
    pv_support_last, pv_justification_last, pv_participation_last, pv_punishment_last,
    reasons,
    ingroup_party, vignette_ingroup_party, party_id,
    StartDate, EndDate, duration_seconds, Finished, Progress,
    any_of(c("ResponseId", "assignmentId", "projectId"))
  ) |>
  arrange(condition, participant_id)

n_as_pv_t4_mismatch <- d_as |>
  filter(condition == "polviol") |>
  filter(!is.na(pv_support_t4) != (last_measure %in% "t4")) |>
  nrow()
if (n_as_pv_t4_mismatch > 0) {
  warning(
    n_as_pv_t4_mismatch,
    " political-violence row(s) have a t4 vignette measure that does not match last_measure.",
    call. = FALSE
  )
}

cat(sprintf(
  paste0(
    "Antisocial recontact: %d respondents ",
    "(%d rows read, %d without an id, %d duplicate sessions dropped, ",
    "%d left before the first belief).\n"
  ),
  nrow(d_as), nrow(as_raw), n_as_no_pid, n_as_dupes, n_as_no_pre
))

# =============================================================================
# SAVE
# =============================================================================

saveRDS(d1_all_deduped, here::here("data", "study_2", "study2_d1_all.rds"))
saveRDS(d1_attnpassed, here::here("data", "study_2", "study2_d1_attnpassed.rds"))
saveRDS(d1_treated, here::here("data", "study_2", "study2_d1_treated.rds"))
saveRDS(d1_itt, here::here("data", "study_2", "study2_d1_itt.rds"))
saveRDS(d1, here::here("data", "study_2", "study2_d1.rds"))
saveRDS(d2_fu_invited, here::here("data", "study_2", "study2_d2_fu_invited.rds"))
saveRDS(d2_matched, here::here("data", "study_2", "study2_d2_matched.rds"))
saveRDS(d2, here::here("data", "study_2", "study2_d2.rds"))
saveRDS(d, here::here("data", "study_2", "study2_d.rds"))
saveRDS(d_as, here::here("data", "study_2", "study2_d_antisocial.rds"))

# Chat transcripts, one row per completer. Qualtrics splits long JSON across
# chathistory1-6; conv_from_hause_chatbot stitches and parses them.
d_conv <- bind_cols(
  d1,
  conv_from_hause_chatbot(
    d1$chathistory1, d1$chathistory2, d1$chathistory3,
    d1$chathistory4, d1$chathistory5, d1$chathistory6
  )
)

d_conv_wide <- d_conv |>
  tidyr::unnest(messages) |>
  select(
    participant_id, condition, reasons,
    cond_belief_pre, cond_belief_post,
    belief_meat_pre, belief_meat_post, belief_meat_change,
    belief_charity_pre, belief_charity_post, belief_charity_change,
    belief_lying_pre, belief_lying_post, belief_lying_change,
    belief_polviol_pre, belief_polviol_post, belief_polviol_change,
    role, content
  ) |>
  group_by(participant_id, role) |>
  mutate(
    n = row_number(),
    role = paste0(role, n)
  ) |>
  arrange(participant_id, n, role) |>
  select(-n) |>
  pivot_wider(names_from = role, values_from = content) |>
  relocate(
    cond_belief_pre, cond_belief_post, reasons, system1,
    assistant1, user1, assistant2, user2, assistant3, user3,
    assistant4, user4, assistant5, user5, assistant6,
    .after = belief_polviol_change
  )

conv_export_path <- here::here("data", "study_2", "study2_conversations.csv")
write_csv(d_conv_wide, conv_export_path)
cat(sprintf(
  "Conversations written to %s (%d participants)\n",
  conv_export_path, nrow(d_conv_wide)
))

d_as_chat <- d_as |>
  filter(str_detect(coalesce(chathistory1, ""), "assistant"))

d_as_conv_wide <- bind_cols(
  d_as_chat,
  conv_from_hause_chatbot(
    d_as_chat$chathistory1, d_as_chat$chathistory2, d_as_chat$chathistory3,
    d_as_chat$chathistory4, d_as_chat$chathistory5, d_as_chat$chathistory6
  )
) |>
  tidyr::unnest(messages) |>
  select(
    participant_id, condition, reasons,
    cond_belief_t0, cond_belief_t1, cond_belief_t2, cond_belief_t3, cond_belief_t4,
    role, content
  ) |>
  group_by(participant_id, role) |>
  mutate(
    n = row_number(),
    role = paste0(role, n)
  ) |>
  arrange(participant_id, n, role) |>
  select(-n) |>
  pivot_wider(names_from = role, values_from = content) |>
  ungroup() |>
  relocate(
    any_of(c(
      "system1",
      "assistant1", "user1", "assistant2", "user2", "assistant3", "user3",
      "assistant4", "user4", "assistant5", "user5", "assistant6"
    )),
    .after = cond_belief_t4
  )

as_conv_export_path <- here::here("data", "study_2", "study2_antisocial_fu_conversations.csv")
write_csv(d_as_conv_wide, as_conv_export_path)
cat(sprintf(
  "Antisocial recontact conversations written to %s (%d participants)\n",
  as_conv_export_path, nrow(d_as_conv_wide)
))

n_attn_fail <- sum(d1_all_deduped$attention == 0, na.rm = TRUE)
n_withdrew_before_attn <- sum(is.na(d1_all_deduped$attention))
n_withdrew_after_attn <- sum(
  d1_all_deduped$attncheck_passed %in% TRUE & is.na(d1_all_deduped$condition)
)
n_withdrew_pre <- n_withdrew_before_attn + n_withdrew_after_attn
n_bot_fail <- sum(!d1_treated$botcheck_passed, na.rm = TRUE)

cat(sprintf(
  paste0(
    "\nParticipant flow (unique after dedupe):\n",
    "  Unique participants:                    %d\n",
    "  Failed attention check:                 %d\n",
    "  Withdrew pre-treatment:                 %d\n",
    "    Never reached the attention check:    %d\n",
    "    Passed attention, never assigned:     %d\n",
    "  Remaining (assigned to treatment):      %d\n",
    "  Failed bot check:                       %d\n",
    "  ITT sample:                             %d\n"
  ),
  nrow(d1_all_deduped),
  n_attn_fail,
  n_withdrew_pre,
  n_withdrew_before_attn,
  n_withdrew_after_attn,
  nrow(d1_treated),
  n_bot_fail,
  nrow(d1_itt)
))

cat(sprintf(
  paste0(
    "Data wrangling complete.\n",
    "  All initial rows:         %d\n",
    "  Unique after dedupe:      %d\n",
    "  Passed attention check:   %d\n",
    "  Assigned to treatment:    %d\n",
    "  ITT sample:               %d\n",
    "  Final initial sample:     %d\n",
    "  Follow-up invite list:    %d\n",
    "  Follow-up rows read:      %d\n",
    "  Follow-up returned:       %d\n",
    "    Both beliefs:           %d\n",
    "    Both behaviours:        %d\n",
    "  Combined sample:          %d\n"
  ), nrow(d1_all), nrow(d1_all_deduped), nrow(d1_attnpassed),
  nrow(d1_treated), nrow(d1_itt), nrow(d1), nrow(d2_fu_invited),
  nrow(d2_raw), n_fu_returned, n_fu_beliefs, n_fu_behav, nrow(d)
))
