# =============================================================================
# 03 · Attrition
# =============================================================================
# Section 1 — Preregistered attrition:
#   Chi-squared tests (ITT → T1, ITT/T1 → T2 in recontact arms); pretreatment
#   predictors of attrition (console tables with Holm/FDR).
#
# Section 2 — Supplement: zero-effect ITT robustness models and console print.
#   Own-arm ATEs for T1/T2 completers and ITT columns with zero-effect imputation.
#   Missing belief change is set to 0. Missing behaviour is set to the
#   control-arm mean. The preregistration instead said to fill a missing
#   follow-up behaviour with that person's time-1 value; Section 2 does not
#   do that. Section 2 is SI Table S7 even when chi-squared attrition tests
#   are not significant.
#
# PAPER TABLES (end of file) — hand-built LaTeX for:
#   attrition-predictors.tex/.csv, attrition-differential.tex/.csv,
#   ate-attrition-robustness.tex/.csv

library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(estimatr)

source(here::here("scripts", "functions", "helpers.R"))

# =============================================================================
# SECTION 1 · Preregistered attrition analysis
# =============================================================================

d1_itt <- readRDS(here::here("data", "study_2", "study2_d1_itt.rds"))
d1     <- readRDS(here::here("data", "study_2", "study2_d1.rds"))
d_fu   <- readRDS(here::here("data", "study_2", "study2_d.rds"))

fu_arms <- c("control", "meat", "charity")

# Helper to build and test a condition × stayed/dropped attrition matrix
test_attrition <- function(pre_data, post_data, label) {
  counts <- pre_data |>
    count(condition) |>
    left_join(
      post_data |> count(condition),
      by = "condition",
      suffix = c("_pre", "_post")
    ) |>
    transmute(
      condition,
      n_base  = n_pre,
      dropped = n_pre - coalesce(n_post, 0L),
      stayed  = coalesce(n_post, 0L),
      pct_dropped = 100 * dropped / n_pre
    )

  mat <- counts |>
    select(condition, dropped, stayed) |>
    column_to_rownames("condition") |>
    as.matrix()

  section(label)
  cat("\nAttrition matrix:\n")
  print(mat)
  cat("\nPercent dropped:\n")
  print(
    counts |>
      mutate(pct_dropped = sprintf("%.1f%%", pct_dropped)) |>
      select(condition, n_base, dropped, stayed, pct_dropped) |>
      as.data.frame(),
    row.names = FALSE
  )
  cat(sprintf(
    "  Overall: %.1f%%  (%d / %d)\n\n",
    100 * sum(counts$dropped) / sum(counts$n_base),
    sum(counts$dropped), sum(counts$n_base)
  ))
  print(chisq.test(mat))
}

# Linear-probability tests: does x predict attrition overall, and does that
# association differ by condition (x × condition vs x + condition)?
predictor_row <- function(dat, var, label, as_pct = FALSE) {
  d <- dat |>
    filter(!is.na(.data[[var]]), !is.na(attrited), !is.na(condition)) |>
    mutate(x = as.numeric(.data[[var]]))

  m_comp <- mean(d$x[d$attrited == 0])
  m_attr <- mean(d$x[d$attrited == 1])

  p_overall <- anova(lm(attrited ~ 1, data = d), lm(attrited ~ x, data = d))$`Pr(>F)`[2]
  p_int <- anova(
    lm(attrited ~ x + condition, data = d),
    lm(attrited ~ x * condition, data = d)
  )$`Pr(>F)`[2]

  scale <- if (as_pct) 100 else 1
  digits <- if (as_pct) 1 else 2
  tibble(
    predictor      = label,
    n              = nrow(d),
    completers     = round(m_comp * scale, digits),
    attriters      = round(m_attr * scale, digits),
    p_overall      = p_overall,
    p_by_condition = p_int
  )
}

factor_predictor_row <- function(dat, var, label) {
  d <- dat |>
    filter(!is.na(.data[[var]]), !is.na(attrited), !is.na(condition)) |>
    mutate(x = .data[[var]])

  tab <- table(d$x, factor(d$attrited, levels = 0:1, labels = c("completed", "attrited")))
  chi <- suppressWarnings(chisq.test(tab))
  if (any(chi$expected < 5)) {
    chi <- chisq.test(tab, simulate.p.value = TRUE, B = 10000)
  }
  p_int <- anova(
    lm(attrited ~ x + condition, data = d),
    lm(attrited ~ x * condition, data = d)
  )$`Pr(>F)`[2]

  list(
    tab = tab,
    n   = nrow(d),
    row = tibble(
      predictor      = label,
      n              = nrow(d),
      completers     = NA_real_,
      attriters      = NA_real_,
      p_overall      = chi$p.value,
      p_by_condition = p_int
    )
  )
}

# =============================================================================
# TEST 1: ITT → t1 completion (post-randomization dropout)
# =============================================================================

test_attrition(d1_itt, d1, "ATTRITION: ITT -> T1 COMPLETION")

# Lying and political violence had a separate follow-up, so they are not
# treated as attriters in these preregistered recontact tables.
d1_itt_fu <- d1_itt |> filter(condition %in% fu_arms)
d1_fu     <- d1 |> filter(condition %in% fu_arms)

test_attrition(
  d1_itt_fu, d_fu,
  "ATTRITION: ITT -> T2 COMPLETION (control, meat, charity)"
)
test_attrition(
  d1_fu, d_fu,
  "ATTRITION: T1 COMPLETERS -> T2 COMPLETION (invited arms)"
)

# =============================================================================
# Pretreatment predictors of attrition
# =============================================================================
# Same predictor list for each transition: demographics, AOT, CRT, political
# measures, AI trust, and moral beliefs.

run_attrition_predictors <- function(d_attr, title) {
section(title)

cat(sprintf(
  "\nn = %d; attrited = %d (%.1f%%); completed = %d\n",
  nrow(d_attr), sum(d_attr$attrited), 100 * mean(d_attr$attrited),
  sum(d_attr$attrited == 0)
))
cat(paste0(
  "Overall: does the variable predict attrition (LPM vs intercept-only)?\n",
  "By condition: does that association differ by condition (x × condition vs x + condition)?\n",
  "Race is White vs non-White (checked White on the multi-select).\n",
  "p-value adjustments (Holm FWER; BH FDR) are applied separately within the overall\n",
  "family and within the by-condition family, each of size k = n predictors.\n"
))

pred_specs <- list(
  list(var = "age",                label = "Age",                         as_pct = FALSE),
  list(var = "education",          label = "Education (1–6)",             as_pct = FALSE),
  list(var = "income_numeric",     label = "Income (midpoint $)",         as_pct = FALSE),
  list(var = "pol_orientation",    label = "Political ideology (1–7)",    as_pct = FALSE),
  list(var = "AOT_mean",           label = "AOT (mean)",                  as_pct = FALSE),
  list(var = "trust_ai_pre",       label = "AI trust (pre)",              as_pct = FALSE),
  list(var = "belief_meat_pre",    label = "Belief: meat (pre)",          as_pct = FALSE),
  list(var = "belief_charity_pre", label = "Belief: charity (pre)",       as_pct = FALSE),
  list(var = "belief_lying_pre",   label = "Belief: lying (pre)",         as_pct = FALSE),
  list(var = "belief_polviol_pre", label = "Belief: pol. violence (pre)", as_pct = FALSE),
  list(var = "crt_batball_cor",    label = "CRT correct (%)",             as_pct = TRUE),
  list(var = "race_white",         label = "White vs non-White (%)",      as_pct = TRUE)
)

factor_specs <- list(
  list(var = "gender_recoded", label = "Gender (recoded)"),
  list(var = "party_id",       label = "Party ID (main)")
)

pred_rows <- bind_rows(lapply(pred_specs, function(s) {
  predictor_row(d_attr, s$var, s$label, as_pct = s$as_pct)
}))
factor_fits <- lapply(factor_specs, function(s) factor_predictor_row(d_attr, s$var, s$label))
factor_rows <- bind_rows(lapply(factor_fits, `[[`, "row"))

all_rows <- bind_rows(pred_rows, factor_rows) |>
  mutate(
    holm_overall      = p.adjust(p_overall, method = "holm"),
    fdr_overall       = p.adjust(p_overall, method = "BH"),
    holm_by_condition = p.adjust(p_by_condition, method = "holm"),
    fdr_by_condition  = p.adjust(p_by_condition, method = "BH")
  )

fmt_pred_table <- function(d) {
  d |>
    transmute(
      predictor,
      n,
      completers,
      attriters,
      p        = fmt_p(p_overall),
      Holm     = fmt_p(holm_overall),
      FDR      = fmt_p(fdr_overall),
      p_x_cond = fmt_p(p_by_condition),
      Holm_x   = fmt_p(holm_by_condition),
      FDR_x    = fmt_p(fdr_by_condition)
    ) |>
    as.data.frame()
}

cat(sprintf("\nContinuous / binary predictors (k = %d tests per family):\n", nrow(all_rows)))
print(fmt_pred_table(all_rows[seq_len(nrow(pred_rows)), ]), row.names = FALSE)

sig_int <- which(pred_rows$p_by_condition < 0.05)
if (length(sig_int) > 0) {
  cat("\nBy-condition means for predictors with uncorrected p_by_condition < .05:\n")
  for (i in sig_int) {
    s <- pred_specs[[i]]
    scale <- if (s$as_pct) 100 else 1
    cat(sprintf("\n%s\n", s$label))
    digits <- if (s$as_pct) 1 else 2
    d_attr |>
      filter(!is.na(.data[[s$var]])) |>
      group_by(condition) |>
      summarise(
        n_comp     = sum(attrited == 0),
        n_attr     = sum(attrited == 1),
        completers = round(mean(.data[[s$var]][attrited == 0]) * scale, digits),
        attriters  = round(mean(.data[[s$var]][attrited == 1]) * scale, digits),
        .groups    = "drop"
      ) |>
      as.data.frame() |>
      print()
  }
}

cat("\nCategorical predictors:\n")
for (i in seq_along(factor_fits)) {
  fit <- factor_fits[[i]]
  adj <- all_rows |> filter(predictor == factor_specs[[i]]$label)
  cat(sprintf("\n%s (n = %d)\n", factor_specs[[i]]$label, fit$n))
  print(fit$tab)
  cat(sprintf(
    "  p overall = %s  Holm = %s  FDR = %s\n  p by condition = %s  Holm = %s  FDR = %s\n",
    fmt_p(adj$p_overall), fmt_p(adj$holm_overall), fmt_p(adj$fdr_overall),
    fmt_p(adj$p_by_condition), fmt_p(adj$holm_by_condition), fmt_p(adj$fdr_by_condition)
  ))
}
}

d_attr_t1 <- d1_itt |>
  mutate(attrited = as.integer(!ResponseId %in% d1$ResponseId))
run_attrition_predictors(d_attr_t1, "PRETREATMENT PREDICTORS OF T1 ATTRITION (ITT)")

d_attr_t2_itt <- d1_itt_fu |>
  mutate(attrited = as.integer(!participantId %in% d_fu$participantId))
run_attrition_predictors(
  d_attr_t2_itt,
  "PRETREATMENT PREDICTORS OF T2 ATTRITION (ITT, RECONTACTED ARMS)"
)

d_attr_t2_invited <- d1_fu |>
  mutate(attrited = as.integer(!participantId %in% d_fu$participantId))
run_attrition_predictors(
  d_attr_t2_invited,
  "PRETREATMENT PREDICTORS OF T2 ATTRITION (T1 COMPLETERS, INVITED ARMS)"
)

# =============================================================================
# SECTION 2 · Supplement: attrition robustness (zero-effect ITT)
# =============================================================================

options(width = max(getOption("width"), 220L))

cond_t1 <- c("control", "meat", "charity", "polviol", "lying")
cond_t2 <- c("control", "meat", "charity")

d1 <- readRDS(here("data", "study_2", "study2_d1.rds"))
d_itt <- readRDS(here("data", "study_2", "study2_d1_itt.rds"))
d_t2 <- readRDS(here("data", "study_2", "study2_d.rds"))
d_t2_matched <- readRDS(here("data", "study_2", "study2_d2_matched.rds"))

# Own-arm estimands. scale converts the model outcome to the reported unit
# (percentage points for the two binary behaviours).
# y_t2_level is the follow-up score used to build the ITT imputation.
# y_t2 is the completer outcome (belief rows are already change scores).
specs <- tibble::tribble(
  ~panel,      ~key,      ~label,                       ~pre,                  ~y_t1,                   ~y_t2,                      ~y_t2_level,           ~treat,    ~scale, ~at_t2,
  "Belief change", "meat",    "Meat",                       "belief_meat_pre",     "belief_meat_change",    "belief_meat_change_t2",    "belief_meat_t2",      "meat",    1,   TRUE,
  "Belief change", "charity", "Charity",                    "belief_charity_pre",  "belief_charity_change", "belief_charity_change_t2", "belief_charity_t2",   "charity", 1,   TRUE,
  "Belief change", "lying",   "Lying",                      "belief_lying_pre",    "belief_lying_change",   NA_character_,              NA_character_,         "lying",   1,   FALSE,
  "Belief change", "polviol", "Political violence",         "belief_polviol_pre",  "belief_polviol_change", NA_character_,              NA_character_,         "polviol", 1,   FALSE,
  "Behaviour",     "meat",    "Dietary choice (pp)",        "belief_meat_pre",     "voucher_is_veg",        "voucher_is_veg_t2",        "voucher_is_veg_t2",   "meat",    100, TRUE,
  "Behaviour",     "charity", "Charitable donation (cents)", "belief_charity_pre", "donation_cents",        "donation_cents_t2",        "donation_cents_t2",   "charity", 1,   TRUE,
  "Behaviour",     "lying",   "Coin flip (pp)",             "belief_lying_pre",    "coin_heads",            NA_character_,              NA_character_,         "lying",   100, FALSE,
  "Behaviour",     "polviol", "PV participation (points)",  "belief_polviol_pre",  "pv_participation",      NA_character_,              NA_character_,         "polviol", 1,   FALSE
)

fmt_stars <- function(p, tex = FALSE) {
  star <- dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ "\u2020",
    TRUE ~ ""
  )
  if (!tex) return(star)
  dplyr::case_when(
    star == "***" ~ "$^{***}$",
    star == "**" ~ "$^{**}$",
    star == "*" ~ "$^{*}$",
    star == "\u2020" ~ "$^{\\dagger}$",
    TRUE ~ ""
  )
}

fmt_cell <- function(est, lo, hi, p, tex = FALSE) {
  ifelse(
    is.na(est),
    if (tex) "---" else "\u2014",
    sprintf("%.2f [%.2f, %.2f]%s", est, lo, hi, fmt_stars(p, tex = tex))
  )
}

# Own-arm coefficient at the mean of the matching pretreatment belief.
fit_own <- function(data, outcome_var, pre_var, treat, scale, cond_levels) {
  dat <- data |>
    filter(
      !is.na(.data[[outcome_var]]),
      !is.na(.data[[pre_var]]),
      !is.na(condition),
      !is.na(order)
    ) |>
    mutate(
      condition = factor(as.character(condition), levels = cond_levels),
      order = droplevels(factor(as.character(order))),
      outcome = as.numeric(.data[[outcome_var]]),
      pre_belief_c = .data[[pre_var]] - mean(.data[[pre_var]])
    )
  m <- lm_robust(
    outcome ~ condition * pre_belief_c + order,
    data = dat,
    se_type = "HC2"
  )
  co <- summary(m)$coefficients
  term <- paste0("condition", treat)
  tibble(
    estimate = co[term, "Estimate"] * scale,
    ci_lo = co[term, "CI Lower"] * scale,
    ci_hi = co[term, "CI Upper"] * scale,
    p = co[term, "Pr(>|t|)"],
    n = nrow(dat)
  )
}

# Missing belief change becomes 0. y_level is the post score when the stored
# column is a level (T2); y_change is an already-computed change (T1).
impute_belief <- function(data, pre_var, y_change = NULL, y_level = NULL) {
  if (!is.null(y_change)) {
    change <- data[[y_change]]
  } else {
    change <- data[[y_level]] - data[[pre_var]]
  }
  data |>
    mutate(y_imp = if_else(is.na(change), 0, change))
}

# Missing behaviour becomes the control-arm mean of observed scores.
impute_behaviour <- function(data, outcome_var, ctrl_mean) {
  data |>
    mutate(
      y_imp = if_else(is.na(.data[[outcome_var]]), ctrl_mean, .data[[outcome_var]])
    )
}

ctrl_mean <- function(data, outcome_var) {
  x <- data[[outcome_var]][as.character(data$condition) == "control"]
  mean(x, na.rm = TRUE)
}

# T2 outcomes observed on any matched returner, including a partial session.
t2_obs <- d_t2_matched |>
  select(participantId, any_of(c(
    "belief_meat_t2", "belief_charity_t2",
    "voucher_is_veg_t2", "donation_cents_t2"
  )))

d_itt_t2 <- d_itt |>
  filter(as.character(condition) %in% cond_t2) |>
  left_join(t2_obs, by = "participantId")

# Control means from observed follow-up scores (returners), and from observed
# round-1 scores on the ITT file.
ctrl_t1 <- c(
  voucher_is_veg = ctrl_mean(d_itt, "voucher_is_veg"),
  donation_cents = ctrl_mean(d_itt, "donation_cents"),
  coin_heads = ctrl_mean(d_itt, "coin_heads"),
  pv_participation = ctrl_mean(d_itt, "pv_participation")
)
ctrl_t2 <- c(
  voucher_is_veg_t2 = ctrl_mean(d_t2, "voucher_is_veg_t2"),
  donation_cents_t2 = ctrl_mean(d_t2, "donation_cents_t2")
)

one_cell <- function(spec, sample_name) {
  blank <- tibble(
    estimate = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
    p = NA_real_, n = NA_integer_, n_imputed = NA_integer_
  )
  if (sample_name %in% c("t2_completers", "t2_itt") && !spec$at_t2) {
    return(blank)
  }

  if (sample_name == "t1_completers") {
    fit <- fit_own(d1, spec$y_t1, spec$pre, spec$treat, spec$scale, cond_t1)
    return(mutate(fit, n_imputed = 0L))
  }

  if (sample_name == "t2_completers") {
    fit <- fit_own(
      filter(d_t2, as.character(condition) %in% cond_t2),
      spec$y_t2, spec$pre, spec$treat, spec$scale, cond_t2
    )
    return(mutate(fit, n_imputed = 0L))
  }

  if (sample_name == "t1_itt") {
    base <- d_itt
    if (spec$panel == "Belief change") {
      dat <- impute_belief(base, spec$pre, y_change = spec$y_t1)
      y_src <- base[[spec$y_t1]]
    } else {
      dat <- impute_behaviour(base, spec$y_t1, ctrl_t1[[spec$y_t1]])
      y_src <- base[[spec$y_t1]]
    }
    n_imputed <- sum(is.na(y_src) & !is.na(base[[spec$pre]]) & !is.na(base$order))
    fit <- fit_own(dat, "y_imp", spec$pre, spec$treat, spec$scale, cond_t1)
    return(mutate(fit, n_imputed = n_imputed))
  }

  base <- d_itt_t2
  if (spec$panel == "Belief change") {
    dat <- impute_belief(base, spec$pre, y_level = spec$y_t2_level)
    y_src <- base[[spec$y_t2_level]]
  } else {
    dat <- impute_behaviour(base, spec$y_t2_level, ctrl_t2[[spec$y_t2_level]])
    y_src <- base[[spec$y_t2_level]]
  }
  n_imputed <- sum(is.na(y_src) & !is.na(base[[spec$pre]]) & !is.na(base$order))
  fit <- fit_own(dat, "y_imp", spec$pre, spec$treat, spec$scale, cond_t2)
  mutate(fit, n_imputed = n_imputed)
}

samples <- c("t1_completers", "t1_itt", "t2_completers", "t2_itt")

results <- map_dfr(seq_len(nrow(specs)), function(i) {
  spec <- specs[i, ]
  map_dfr(samples, function(sample_name) {
    one_cell(spec, sample_name) |>
      mutate(
        panel = spec$panel,
        outcome = spec$label,
        key = spec$key,
        sample = sample_name,
        cell = fmt_cell(estimate, ci_lo, ci_hi, p, tex = FALSE),
        cell_tex = fmt_cell(estimate, ci_lo, ci_hi, p, tex = TRUE)
      )
  })
})

# Column Ns. Within a column the estimated rows share one sample.
n_of <- function(sample_name) {
  results |>
    filter(sample == sample_name, !is.na(n)) |>
    pull(n) |>
    unique()
}
n_t1_comp <- n_of("t1_completers")
n_t1_itt <- n_of("t1_itt")
n_t2_comp <- n_of("t2_completers")
n_t2_itt <- n_of("t2_itt")
stopifnot(
  length(n_t1_comp) == 1, length(n_t1_itt) == 1,
  length(n_t2_comp) == 1, length(n_t2_itt) == 1
)

section("SUPPLEMENT: ATTRITION ROBUSTNESS")
cat("\nT1 control means: dietary choice ", sprintf("%.1f%%", 100 * ctrl_t1[["voucher_is_veg"]]),
    ", donation ", sprintf("%.2f cents", ctrl_t1[["donation_cents"]]),
    ", coin flip ", sprintf("%.1f%%", 100 * ctrl_t1[["coin_heads"]]),
    ", participation ", sprintf("%.2f", ctrl_t1[["pv_participation"]]), "\n", sep = "")
cat("T2 control means: dietary choice ", sprintf("%.1f%%", 100 * ctrl_t2[["voucher_is_veg_t2"]]),
    ", donation ", sprintf("%.2f cents", ctrl_t2[["donation_cents_t2"]]), "\n\n", sep = "")

show_wide <- results |>
  select(panel, outcome, sample, cell) |>
  pivot_wider(names_from = sample, values_from = cell)
print(as.data.frame(show_wide), row.names = FALSE, right = FALSE)

cat(sprintf(
  "\nNs: T1 completers %d, T1 ITT %d, T2 completers %d, T2 ITT %d\n",
  n_t1_comp, n_t1_itt, n_t2_comp, n_t2_itt
))
cat("\nImputed counts (ITT):\n")
results |>
  filter(sample %in% c("t1_itt", "t2_itt")) |>
  select(panel, outcome, sample, n, n_imputed) |>
  print(n = Inf)


# #############################################################################
# PAPER TABLES
# #############################################################################
# Long on purpose. Cells are hand-built LaTeX: completers vs attriters
# across two waves with Holm, a within-condition table, and the zero-effect
# ITT robustness table (SI Table S7). The tests and models are above.

# =============================================================================
# Supplement tables
# =============================================================================
# Two tables. The overall table reports the predictor mean among completers
# and among attriters, plus the linear-probability coefficient for completion.
# The differential table reports those means inside each condition, plus the
# joint test of predictor × condition. Time 2 is the intention-to-treat sample
# in the recontacted arms: follow-up completers versus everyone randomized to
# those arms who did not complete follow-up. Gender is woman versus man.
# Party identification is the 1–7 Democrat–Republican scale.

table_specs <- tribble(
  ~var,          ~label,                                      ~mean_scale, ~digits,
  "age",         "Age",                                       1,           1,
  "gender_w",    "Gender (\\% women)",                        100,         1,
  "education",   "Education",                                 1,           2,
  "income_10k",  "Income (\\$10,000s)",                       1,           1,
  "race_white",  "White (\\%)",                               100,         1,
  "AOT_mean",    "Actively open-minded thinking",             1,           2,
  "crt_num",     "Cognitive reflection (\\%)",                100,         1,
  "pol_orientation", "Political ideology",                    1,           2,
  "dem_rep_num", "Party identification",                      1,           2,
  "trust_ai_pre","AI trust",                                  1,           2,
  "belief_meat_pre",    "Pretreatment meat belief",           1,           1,
  "belief_charity_pre", "Pretreatment charity belief",        1,           1,
  "belief_lying_pre",   "Pretreatment lying belief",          1,           1,
  "belief_polviol_pre", "Pretreatment political-violence belief", 1,       1
)

cond_tex <- c(
  control = "Control",
  meat    = "Meat",
  charity = "Charity",
  polviol = "\\shortstack{Political\\\\violence}",
  lying   = "Lying"
)

prepare_table_data <- function(dat, conditions) {
  dat |>
    mutate(
      condition = factor(condition, levels = conditions),
      gender_w = case_when(
        gender_recoded == "Man" ~ 0,
        gender_recoded == "Woman" ~ 1,
        TRUE ~ NA_real_
      ),
      income_10k = income_numeric / 10000,
      crt_num = as.numeric(crt_batball_cor),
      race_white = as.numeric(race_white)
    )
}

mean_group <- function(x) {
  if (length(x) == 0L || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sd_group <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L) NA_real_ else stats::sd(x)
}

fit_table_row <- function(dat, spec, conditions) {
  d <- dat |>
    filter(!is.na(.data[[spec$var]]), !is.na(attrited), !is.na(condition)) |>
    mutate(
      x = as.numeric(.data[[spec$var]]),
      completed = 1 - attrited
    )

  co <- summary(lm(completed ~ x, data = d))$coefficients
  p_int <- anova(
    lm(completed ~ x + condition, data = d),
    lm(completed ~ x * condition, data = d)
  )$`Pr(>F)`[2]

  by_cond <- lapply(conditions, function(cond) {
    g <- d[d$condition == cond, , drop = FALSE]
    c(
      completers    = mean_group(g$x[g$attrited == 0]) * spec$mean_scale,
      completers_sd = sd_group(g$x[g$attrited == 0]) * spec$mean_scale,
      attriters     = mean_group(g$x[g$attrited == 1]) * spec$mean_scale,
      attriters_sd  = sd_group(g$x[g$attrited == 1]) * spec$mean_scale
    )
  })
  names(by_cond) <- conditions

  list(
    completers    = mean_group(d$x[d$attrited == 0]) * spec$mean_scale,
    completers_sd = sd_group(d$x[d$attrited == 0]) * spec$mean_scale,
    attriters     = mean_group(d$x[d$attrited == 1]) * spec$mean_scale,
    attriters_sd  = sd_group(d$x[d$attrited == 1]) * spec$mean_scale,
    b             = 100 * unname(co["x", "Estimate"]),
    p             = unname(co["x", "Pr(>|t|)"]),
    p_int         = unname(p_int),
    n             = nrow(d),
    by_cond       = by_cond
  )
}

fit_wave_table <- function(dat, conditions) {
  rows <- lapply(seq_len(nrow(table_specs)), function(i) {
    fit_table_row(dat, table_specs[i, ], conditions)
  })
  p <- vapply(rows, `[[`, numeric(1), "p")
  p_int <- vapply(rows, `[[`, numeric(1), "p_int")
  list(
    rows = rows,
    p_holm = p.adjust(p, method = "holm"),
    p_int_holm = p.adjust(p_int, method = "holm")
  )
}

fmt_mean <- function(x, digits) {
  vapply(x, function(v) {
    if (is.na(v)) return("---")
    r <- round(v, digits)
    if (abs(r) < 10^(-digits) / 2) r <- 0
    sprintf(paste0("%.", digits, "f"), r)
  }, character(1))
}

fmt_mean_sd <- function(m, s, digits) {
  paste0(fmt_mean(m, digits), " (", fmt_mean(s, digits), ")")
}

fmt_b_tex <- function(b) {
  r <- round(b, 2)
  r[abs(r) < 0.005] <- 0
  ifelse(is.na(r), "---", sprintf("%.2f", r))
}

fmt_p_txt <- function(p) {
  ifelse(
    is.na(p), "NA",
    ifelse(p < 0.001, "<.001", ifelse(p > 0.999, "1.00", sub("^0", "", sprintf("%.3f", p))))
  )
}

d_tab_t1 <- prepare_table_data(d_attr_t1, c("control", "meat", "charity", "polviol", "lying"))
d_tab_t2 <- prepare_table_data(d_attr_t2_itt, fu_arms)
wave_t1 <- fit_wave_table(d_tab_t1, levels(d_tab_t1$condition))
wave_t2 <- fit_wave_table(d_tab_t2, levels(d_tab_t2$condition))

overall_body <- vapply(seq_len(nrow(table_specs)), function(i) {
  s <- table_specs[i, ]
  a <- wave_t1$rows[[i]]
  b <- wave_t2$rows[[i]]
  sprintf(
    "  %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
    s$label,
    fmt_mean_sd(a$completers, a$completers_sd, s$digits),
    fmt_mean_sd(a$attriters, a$attriters_sd, s$digits),
    fmt_b_tex(a$b), fmt_p_tex(wave_t1$p_holm[i]),
    fmt_mean_sd(b$completers, b$completers_sd, s$digits),
    fmt_mean_sd(b$attriters, b$attriters_sd, s$digits),
    fmt_b_tex(b$b), fmt_p_tex(wave_t2$p_holm[i])
  )
}, character(1))

n_tex <- function(n) format(n, big.mark = ",")
n_t1_itt <- nrow(d_tab_t1)
n_t1_done <- sum(d_tab_t1$attrited == 0)
n_t2_itt <- nrow(d_tab_t2)
n_t2_done <- sum(d_tab_t2$attrited == 0)

overall_note <- paste(
  "\\footnotesize\\textit{Note.} `Completers' and `Attriters' entries report",
  "pre-treatment mean (SD) for completers and attriters.",
  "$b$ is the linear-probability coefficient in percentage points when",
  "regressing completion status on that variable.",
  sprintf(
    "Time 1 is attrition from the ITT sample (N = %s) to original study completion (N = %s).",
    n_tex(n_t1_itt), n_tex(n_t1_done)
  ),
  sprintf(
    "Time 2 is attrition from the intention-to-treat sample in the recontacted arms (N = %s) to follow-up completion (N = %s).",
    n_tex(n_t2_itt), n_tex(n_t2_done)
  ),
  "$p$ reports Holm-adjusted $p$-values, with each timepoint as a family.",
  "For gender, White, and cognitive reflection, the means are percentages and",
  "$b$ is the completion gap between the two groups.",
  "Income is in \\$10,000s. Party identification runs from strong Democrat (1)",
  "to strong Republican (7). Bold cells are Holm $p < .05$.",
  sep = " "
)

overall_tex <- c(
  "% Requires booktabs.",
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{Pretreatment predictors of attrition.}",
  "  \\label{tab:attrition-predictors}",
  "  \\footnotesize",
  "  \\setlength{\\tabcolsep}{4pt}",
  "  \\begin{tabular}{@{}l*{8}{c}@{}}",
  "  \\toprule",
  "  & \\multicolumn{4}{c}{Time 1} & \\multicolumn{4}{c}{Time 2} \\\\",
  "  \\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
  "  Variable & Completers & Attriters & $b$ & $p$ & Completers & Attriters & $b$ & $p$ \\\\",
  "  \\midrule",
  overall_body,
  "  \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{2pt}",
  "  \\begin{minipage}{\\linewidth}",
  paste0("  ", overall_note),
  "  \\end{minipage}",
  "\\end{table}",
  ""
)

diff_panel <- function(wave, conditions, title) {
  header <- paste(vapply(conditions, function(cond) {
    sprintf("\\multicolumn{2}{c}{%s}", cond_tex[[cond]])
  }, character(1)), collapse = " & ")
  sub <- paste(rep("Completers & Attriters", length(conditions)), collapse = " & ")
  rules <- paste(vapply(seq_along(conditions), function(k) {
    start <- 2 + (k - 1) * 2
    sprintf("\\cmidrule(lr){%d-%d}", start, start + 1)
  }, character(1)), collapse = " ")
  body <- vapply(seq_len(nrow(table_specs)), function(i) {
    s <- table_specs[i, ]
    cells <- vapply(conditions, function(cond) {
      m <- wave$rows[[i]]$by_cond[[cond]]
      paste(
        fmt_mean_sd(m[["completers"]], m[["completers_sd"]], s$digits),
        fmt_mean_sd(m[["attriters"]], m[["attriters_sd"]], s$digits),
        sep = " & "
      )
    }, character(1))
    sprintf(
      "  %s & %s & %s \\\\",
      s$label, paste(cells, collapse = " & "), fmt_p_tex(wave$p_int_holm[i])
    )
  }, character(1))
  c(
    sprintf("  \\textit{%s}", title),
    "  \\vspace{4pt}",
    "",
    sprintf("  \\begin{tabular}{@{}l*{%d}{c} c@{}}", length(conditions) * 2),
    "  \\toprule",
    sprintf("  & %s & \\\\", header),
    sprintf("  %s", rules),
    sprintf("  Variable & %s & $p$ \\\\", sub),
    "  \\midrule",
    body,
    "  \\bottomrule",
    "  \\end{tabular}",
    ""
  )
}

diff_note <- paste(
  "\\footnotesize\\textit{Note.} `Completers' and `Attriters' entries report",
  "pre-treatment mean (SD) for completers and attriters in each condition.",
  "$p$ is the Holm-adjusted nested likelihood-ratio test of predictor",
  "$\\times$ condition against predictor $+$ condition, applied across the",
  "14 predictors within each timepoint. The lying and political-violence arms",
  "had a separate follow-up, reported with the corrective conversations",
  "(SI~\\ref{sup:corrective}), and are not in this table.",
  "Bold cells are Holm $p < .05$.",
  sep = " "
)

diff_tex <- c(
  "% Requires booktabs and pdflscape.",
  "\\begin{landscape}",
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{Differential attrition by condition.}",
  "  \\label{tab:attrition-differential}",
  "  \\footnotesize",
  "  \\setlength{\\tabcolsep}{3pt}",
  diff_panel(wave_t1, c("control", "meat", "charity", "polviol", "lying"), "Time 1"),
  "  \\vspace{8pt}",
  "",
  diff_panel(wave_t2, fu_arms, "Time 2"),
  "  \\vspace{2pt}",
  "  \\begin{minipage}{\\linewidth}",
  paste0("  ", diff_note),
  "  \\end{minipage}",
  "\\end{table}",
  "\\end{landscape}",
  ""
)

out_dir <- here::here("output", "study_2", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
overall_path <- file.path(out_dir, "attrition-predictors.tex")
diff_path <- file.path(out_dir, "attrition-differential.tex")
write_paper_table(overall_tex, overall_path)
write_paper_table(diff_tex, diff_path)

overall_csv <- bind_rows(lapply(seq_len(nrow(table_specs)), function(i) {
  s <- table_specs[i, ]
  tibble(
    variable = s$label,
    comp_t1 = wave_t1$rows[[i]]$completers,
    comp_sd_t1 = wave_t1$rows[[i]]$completers_sd,
    attr_t1 = wave_t1$rows[[i]]$attriters,
    attr_sd_t1 = wave_t1$rows[[i]]$attriters_sd,
    b_t1 = wave_t1$rows[[i]]$b,
    p_t1 = wave_t1$rows[[i]]$p,
    p_holm_t1 = wave_t1$p_holm[i],
    comp_t2 = wave_t2$rows[[i]]$completers,
    comp_sd_t2 = wave_t2$rows[[i]]$completers_sd,
    attr_t2 = wave_t2$rows[[i]]$attriters,
    attr_sd_t2 = wave_t2$rows[[i]]$attriters_sd,
    b_t2 = wave_t2$rows[[i]]$b,
    p_t2 = wave_t2$rows[[i]]$p,
    p_holm_t2 = wave_t2$p_holm[i],
    p_diff_t1 = wave_t1$rows[[i]]$p_int,
    p_diff_holm_t1 = wave_t1$p_int_holm[i],
    p_diff_t2 = wave_t2$rows[[i]]$p_int,
    p_diff_holm_t2 = wave_t2$p_int_holm[i]
  )
}))
write.csv(overall_csv, file.path(out_dir, "attrition-predictors.csv"), row.names = FALSE)

diff_csv <- bind_rows(lapply(seq_len(nrow(table_specs)), function(i) {
  s <- table_specs[i, ]
  bind_rows(lapply(c("t1", "t2"), function(wave_name) {
    wave <- if (wave_name == "t1") wave_t1 else wave_t2
    conditions <- if (wave_name == "t1") {
      c("control", "meat", "charity", "polviol", "lying")
    } else {
      fu_arms
    }
    bind_rows(lapply(conditions, function(cond) {
      m <- wave$rows[[i]]$by_cond[[cond]]
      tibble(
        variable = s$label,
        wave = wave_name,
        condition = cond,
        completers = m[["completers"]],
        completers_sd = m[["completers_sd"]],
        attriters = m[["attriters"]],
        attriters_sd = m[["attriters_sd"]],
        p_holm = if (wave_name == "t1") wave_t1$p_int_holm[i] else wave_t2$p_int_holm[i]
      )
    }))
  }))
}))
write.csv(diff_csv, file.path(out_dir, "attrition-differential.csv"), row.names = FALSE)

section("ATTRITION TABLES")
cat(sprintf("Wrote %s\nWrote %s\n", overall_path, diff_path))
print(as.data.frame(transmute(
  overall_csv,
  variable,
  comp_t1 = round(comp_t1, 2),
  attr_t1 = round(attr_t1, 2),
  b_t1 = round(b_t1, 2),
  p_t1 = fmt_p_txt(p_holm_t1),
  comp_t2 = round(comp_t2, 2),
  attr_t2 = round(attr_t2, 2),
  b_t2 = round(b_t2, 2),
  p_t2 = fmt_p_txt(p_holm_t2),
  diff_t1 = fmt_p_txt(p_diff_holm_t1),
  diff_t2 = fmt_p_txt(p_diff_holm_t2)
)), row.names = FALSE)



wide_tex <- results |>
  select(panel, outcome, sample, cell_tex) |>
  pivot_wider(names_from = sample, values_from = cell_tex)

row_tex <- function(panel_name) {
  rows <- wide_tex |> filter(panel == panel_name)
  c(
    sprintf("  \\multicolumn{5}{@{}l}{\\textbf{%s}} \\\\", panel_name),
    purrr::map_chr(seq_len(nrow(rows)), function(i) {
      sprintf(
        "  %s & %s & %s & %s & %s \\\\",
        rows$outcome[[i]],
        rows$t1_completers[[i]],
        rows$t1_itt[[i]],
        rows$t2_completers[[i]],
        rows$t2_itt[[i]]
      )
    })
  )
}

col_heads <- sprintf(
  paste0(
    "  Outcome & \\shortstack{T1 completers\\\\(N = %s)} & ",
    "\\shortstack{T1 ITT\\\\(N = %s)} & ",
    "\\shortstack{T2 completers\\\\(N = %s)} & ",
    "\\shortstack{T2 ITT\\\\(N = %s)} \\\\"
  ),
  format(n_t1_comp, big.mark = ","),
  format(n_t1_itt, big.mark = ","),
  format(n_t2_comp, big.mark = ","),
  format(n_t2_itt, big.mark = ",")
)

note <- paste0(
  "    \\footnotesize\\textit{Note.} Cells are own-arm average treatment effects ",
  "(95\\% CIs), from outcome $\\sim$ condition $\\times$ centered pretreatment belief $+$ order, ",
  "with HC2 standard errors. Belief change is in scale points. ",
  "T1 completers and T2 completers restate the main analyses. ",
  "Intention-to-treat (ITT) columns assign a treatment effect of zero to participants missing that outcome. ",
  "For belief change, the missing score is replaced with the person's pretreatment score, so change $= 0$. ",
  "For behaviour, which has no pretreatment measure, the missing outcome is replaced with the control-arm mean ",
  sprintf(
    "(round 1: dietary choice %.1f\\%%, donation %.2f cents, coin flip %.1f\\%%, participation %.2f; ",
    100 * ctrl_t1[["voucher_is_veg"]],
    ctrl_t1[["donation_cents"]],
    100 * ctrl_t1[["coin_heads"]],
    ctrl_t1[["pv_participation"]]
  ),
  sprintf(
    "follow-up: dietary choice %.1f\\%%, donation %.2f cents). ",
    100 * ctrl_t2[["voucher_is_veg_t2"]],
    ctrl_t2[["donation_cents_t2"]]
  ),
  "T2 models include the recontact of control, meat, and charity only. ",
  "$^{\\dagger}p < .10$, ${}^{*}p < .05$, ${}^{**}p < .01$, ${}^{***}p < .001$."
)

tex <- c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{Robustness to attrition.}",
  "  \\label{tab:attrition-robustness}",
  "  \\footnotesize",
  "  \\setlength{\\tabcolsep}{4pt}",
  "  \\begin{tabular}{@{}lcccc@{}}",
  "  \\toprule",
  col_heads,
  "  \\midrule",
  row_tex("Belief change"),
  "  \\midrule",
  row_tex("Behaviour"),
  "  \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{2pt}",
  "  \\begin{minipage}{\\linewidth}",
  note,
  "  \\end{minipage}",
  "\\end{table}",
  ""
)

out_dir <- here("output", "study_2", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_tex <- file.path(out_dir, "ate-attrition-robustness.tex")
out_csv <- file.path(out_dir, "ate-attrition-robustness.csv")
write_paper_table(tex, out_tex)
results |>
  select(panel, outcome, sample, estimate, ci_lo, ci_hi, p, n, n_imputed, cell) |>
  write.csv(out_csv, row.names = FALSE)

section("SUPPLEMENT: ATTRITION ROBUSTNESS TABLE")
cat(sprintf("\nWrote %s\nWrote %s\n", out_tex, out_csv))
