# =============================================================================
# 04b · Follow-up effects (t2) and persistence (t1 → t2)
# =============================================================================
# One-month returners in control / meat / charity. Same preregistered model
# as immediate analyses. Persistence: t2 ~ t1 + condition + t0 + order.
#
# Sourced by 04_main_analyses.R.

library(dplyr)
library(purrr)
library(tidyr)
library(estimatr)
library(here)

source(here("scripts", "functions", "helpers.R"))
source(here("scripts", "functions", "study2_style.R"))
source(here("scripts", "functions", "study2_te_helpers.R"))
source(here("scripts", "functions", "study2_outcomes.R"))

options(width = max(getOption("width"), 220L))

# =============================================================================
# 6. FOLLOW-UP ANALYSES (one-month returners)
# =============================================================================
# One-month returners in the follow-up arms (control, meat, charity).
# Lying and political violence were not recontacted.
#
# Same preregistered model as immediate (t1) analyses:
#   outcome ~ condition * pre_belief_c + order
# HC2 robust SEs; control as reference; no multiplicity adjustment.
# `order` is the round-1 block order. `pre_belief_c` is the matching t0
# belief, centered in this follow-up sample.
#
# Belief outcomes are change from the round-1 pretreatment score.
# Behaviour and self-report outcomes are the follow-up levels.
#
# Persistence (preregistered), at the end of this section:
#   t1 ~ condition + t0 + order
#   t2 ~ t1 + condition + t0 + order

d <- readRDS(here("data", "study_2", "study2_d.rds")) |>
  mutate(
    condition = factor(condition, levels = c("control", "meat", "charity")),
    order = droplevels(factor(order))
  )

# Full baseline sample. Belief and donation standardizers are computed here
# and reused below, so follow-up d is not a function of who returned.
d1_base <- readRDS(here("data", "study_2", "study2_d1.rds")) |>
  mutate(
    condition = factor(
      condition,
      levels = c("control", "meat", "charity", "polviol", "lying")
    )
  )

cat("Follow-up sample (returners): N =", nrow(d), "\n")
print(table(d$condition))

condition_labels <- condition_labels_fu
# Follow-up arms only (lying / polviol were not recontacted).
primary_treat <- study2_primary_treat[c("meat", "charity")]
treat_levels <- c("meat", "charity")
treat_col_order <- c("Meat", "Charity")

# =============================================================================
# DESCRIPTIVES
# =============================================================================

summarise_outcomes <- function(data, defs) {
  purrr::map_dfr(seq_len(nrow(defs)), function(i) {
    v <- defs$outcome[i]
    key <- defs$key[i]
    outcome_type <- defs$type[i]
    data |>
      filter(!is.na(.data[[v]]), !is.na(condition)) |>
      group_by(condition) |>
      summarise(
        n = n(),
        M = mean(as.numeric(.data[[v]]), na.rm = TRUE),
        SD = sd(as.numeric(.data[[v]]), na.rm = TRUE),
        se = SD / sqrt(n()),
        .groups = "drop"
      ) |>
      mutate(
        measure = key,
        measure_lab = factor(defs$short[i], levels = defs$short),
        type = outcome_type,
        mean_plot = if (outcome_type == "binary") 100 * M else M,
        se_plot = if (outcome_type == "binary") 100 * se else se,
        ci_lower = mean_plot - 1.96 * se_plot,
        ci_upper = mean_plot + 1.96 * se_plot,
        cell = fmt_mean_cell(M, SD, outcome_type),
        condition_lab = factor(
          condition_labels[as.character(condition)],
          levels = unname(condition_labels)
        ),
        is_primary = as.character(condition) == primary_treat[key]
      )
  })
}

print_means <- function(summary_df, note) {
  wide <- summary_df |>
    select(measure_lab, condition_lab, cell) |>
    pivot_wider(names_from = condition_lab, values_from = cell) |>
    select(Measure = measure_lab, any_of(unname(condition_labels)))
  cat("\n", note, "\n", sep = "")
  print(as.data.frame(wide), row.names = FALSE)

  cat("\nCell Ns:\n")
  print(
    summary_df |>
      select(measure_lab, condition_lab, n) |>
      pivot_wider(names_from = condition_lab, values_from = n) |>
      select(Measure = measure_lab, any_of(unname(condition_labels))) |>
      as.data.frame(),
    row.names = FALSE
  )
}

# =============================================================================
# 1. BELIEF CHANGE
# =============================================================================

belief_defs <- study2_belief_defs_t2()
print(as.data.frame(belief_defs), row.names = FALSE)

section("FOLLOW-UP BELIEF CHANGE: MEANS AND SDs BY CONDITION")

belief_summary <- summarise_outcomes(d, belief_defs)
print_means(belief_summary, "Mean (SD) belief change (t2 − pretreatment) by condition:")

section("FOLLOW-UP BELIEF CHANGE: PREREGISTERED MODELS")
cat("outcome ~ condition * pre_belief_c + order  (HC2).\n")
cat("Outcome is follow-up belief minus the round-1 pretreatment score.\n")

# d = coefficient / SD of the pretreatment item on the full baseline sample,
# condition ignored. Not recomputed on returners.
sd_belief <- c(
  meat = study2_d_sd(d1_base, "belief_meat_pre", "pre"),
  charity = study2_d_sd(d1_base, "belief_charity_pre", "pre")
)
belief_models <- fit_prereg_models(d, belief_defs, outcome_col = "outcome")
belief_ates <- collect_ates(
  belief_models, belief_defs, treat_levels, primary_treat, condition_labels,
  d_from = "pre", sd_by_key = sd_belief
)
print_primary_belief_table(belief_ates)

# Main text: follow-up own-arm ATE as a percent of the immediate own-arm ATE.
# all_ates is the immediate belief table from 04a, still in this session.
share_of_immediate <- function(later, earlier, key) {
  b2 <- later$estimate[later$outcome == key & later$primary]
  b1 <- earlier$estimate[earlier$outcome == key & earlier$primary]
  100 * b2 / b1
}
cat(sprintf(
  "\nShare of the immediate effect still present at follow-up (follow-up ATE / immediate ATE): meat belief %.0f%%, charity belief %.0f%%.\n",
  share_of_immediate(belief_ates, all_ates, "meat"),
  share_of_immediate(belief_ates, all_ates, "charity")
))

# =============================================================================
# 2. BEHAVIOUR
# =============================================================================

behavior_defs <- study2_behavior_defs_t2()
print(as.data.frame(behavior_defs), row.names = FALSE)

section("FOLLOW-UP BEHAVIOUR: MEANS AND SDs BY CONDITION")

beh_summary <- summarise_outcomes(d, behavior_defs)
print_means(
  beh_summary,
  "Mean (SD) by condition. Dietary choice is the percent choosing the veggie voucher."
)

section("FOLLOW-UP BEHAVIOUR: PREREGISTERED MODELS")
cat("outcome ~ condition * pre_belief_c + order  (HC2); matching t0 belief.\n")
cat("Linear probability model for the voucher choice.\n")

# Neither behavior has a pretreatment measure. d reuses the control arm's
# time-1 SD, including the 0/1 dietary choice. Do not recompute it on returners.
sd_beh <- study2_behavior_d_sds(d1_base)[c("meat", "charity")]
beh_models <- fit_prereg_models(d, behavior_defs, outcome_col = "outcome")
# Immediate behaviour ATEs from 04a, before this script reuses the name.
t1_beh_ates <- beh_ates
beh_ates <- collect_ates(
  beh_models, behavior_defs, treat_levels, primary_treat, condition_labels,
  d_from = "out", sd_by_key = sd_beh
)
print_primary_behavior_table(
  beh_ates,
  paste(
    "Cohen's d = model coefficient / control-arm SD of the time-1 outcome.",
    "The dietary-choice coefficient and SD are both on the 0/1 scale."
  )
)
cat(sprintf(
  "\nShare of the immediate dietary-choice effect still present at follow-up (follow-up ATE / immediate ATE): %.0f%%.\n",
  share_of_immediate(beh_ates, t1_beh_ates, "meat")
))

# =============================================================================
# 3. SELF-REPORTED BEHAVIOUR
# =============================================================================
# Qualtrics names: prev_meat, prev_donate. No round-1 version of these items.

selfrep_defs <- tibble::tribble(
  ~key,      ~outcome,        ~pre,                 ~short,                    ~type,  ~label,
  "meat",    "prev_meat_t2",  "belief_meat_pre",    "Self-reported meat",      "count", "Self-report (t2): prev_meat  [pre = belief_meat_pre]",
  "charity", "prev_donate_t2","belief_charity_pre", "Self-reported donation",  "count", "Self-report (t2): prev_donate  [pre = belief_charity_pre]"
)

section("FOLLOW-UP SELF-REPORT: MEANS AND SDs BY CONDITION")

self_summary <- summarise_outcomes(d, selfrep_defs)
print_means(
  self_summary,
  "Mean (SD) of self-reported behaviour. Charity is prev_donate."
)

section("FOLLOW-UP SELF-REPORT: PREREGISTERED MODELS")
cat("outcome ~ condition * pre_belief_c + order  (HC2); matching t0 belief.\n")
cat("Outcomes are the follow-up self-reports (prev_meat, prev_donate).\n")

# These items exist only at follow-up, so the unaffected standardizer is the
# control arm's SD at this wave. The meat and charity arms are not pooled in.
sd_self <- c(
  meat = study2_d_sd(d, "prev_meat_t2", "control"),
  charity = study2_d_sd(d, "prev_donate_t2", "control")
)
self_models <- fit_prereg_models(d, selfrep_defs, outcome_col = "outcome")
self_ates <- collect_ates(
  self_models, selfrep_defs, treat_levels, primary_treat, condition_labels,
  d_from = "out", sd_by_key = sd_self
)
print_primary_behavior_table(
  self_ates,
  "Cohen's d = model coefficient / control-arm SD of the follow-up self-report."
)

# =============================================================================
# 4. PERSISTENCE (t1 -> t2)
# =============================================================================
# Preregistered mediation:
#   t1 ~ condition + t0 + order
#   t2 ~ t1 + condition + t0 + order
# Persistence = b on t1 in the t2 model (the b in the a*b indirect effect).
# Sleeper = average direct effect of the own-arm treatment vs control.
# Beliefs are levels (not change scores), as in Study 1. Behaviours are the
# t1 and t2 measures. t0 is the matching pretreatment belief. Self-reports
# have no t1 item, so they are not included.

section("PERSISTENCE (t1 -> t2)")
# Load the namespace without attaching it. library(mediation) would put MASS
# on the search path and mask dplyr::select for scripts sourced after this one.
invisible(loadNamespace("mediation"))
import_mediation_settings()
announce_mediation_settings()
cat("Model 1: t1 ~ condition + t0 + order\n")
cat("Model 2: t2 ~ t1 + condition + t0 + order\n")
cat("Persistence = b on t1 in Model 2. Sleeper = ADE (own-arm vs control).\n")
cat("Belief outcomes are levels. t0 is the matching pretreatment belief.\n")
cat(mediation_sims_line(), "\n", sep = "")

run_persistence <- function(data, y, m, t0, treat_group,
                            control_group = "control",
                            sims = mediation_n_sims(),
                            boot = mediation_bootstrap_enabled()) {
  d_med <- data |>
    filter(
      !is.na(.data[[y]]), !is.na(.data[[m]]), !is.na(.data[[t0]]),
      !is.na(condition), !is.na(order)
    ) |>
    mutate(
      across(all_of(c(y, m, t0)), as.numeric)
    )
  n <- nrow(d_med)
  assign(".d_med", d_med, envir = parent.frame())

  f1_str <- paste0(m, " ~ condition + ", t0, " + order")
  f2_str <- paste0(y, " ~ ", m, " + condition + ", t0, " + order")
  m1 <- eval.parent(call("lm", formula = as.formula(f1_str), data = as.name(".d_med")))
  m2 <- eval.parent(call("lm", formula = as.formula(f2_str), data = as.name(".d_med")))

  med <- mediation::mediate(
    model.m = m1, model.y = m2,
    treat = "condition", mediator = m,
    control.value = control_group, treat.value = treat_group,
    boot = boot, sims = sims
  )

  s_m2 <- summary(m2)$coefficients
  b_est <- if (m %in% rownames(s_m2)) s_m2[m, "Estimate"] else NA_real_
  b_se  <- if (m %in% rownames(s_m2)) s_m2[m, "Std. Error"] else NA_real_
  b_p   <- if (m %in% rownames(s_m2)) s_m2[m, "Pr(>|t|)"] else NA_real_

  list(m1 = m1, m2 = m2, med = med, n = n, b = b_est, b_se = b_se, b_p = b_p)
}

print_persistence <- function(label, res) {
  cat("\n------", label, "------\n")
  cat(sprintf("N (complete cases) = %d\n", res$n))
  cat("\nModel 1 (t1): t1 ~ condition + t0 + order\n")
  print(summary(res$m1))
  cat("\nModel 2 (t2): t2 ~ t1 + condition + t0 + order\n")
  print(summary(res$m2))
  cat(sprintf(
    "\nPersistence (b on t1) = %.3f (SE = %.3f, p = %s), %.0f%% of the t1 outcome\n",
    res$b, res$b_se, fmt_p(res$b_p), 100 * res$b
  ))
  cat(if (mediation_bootstrap_enabled()) {
    "\nMediation summary (bootstrap). ACME = a*b; ADE = sleeper / direct effect:\n"
  } else {
    paste0(
      "\nMediation summary (quasi-Bayesian smoke test; not the reported bootstrap). ",
      "ACME = a*b; ADE = sleeper / direct effect:\n"
    )
  })
  print(summary(res$med))
}

persist_defs <- tibble::tribble(
  ~key,          ~y,                    ~m,                   ~t0,                  ~treat,    ~short,
  "meat_bel",    "belief_meat_t2",      "belief_meat_post",   "belief_meat_pre",    "meat",    "Meat belief",
  "charity_bel", "belief_charity_t2",   "belief_charity_post","belief_charity_pre", "charity", "Charity belief",
  "meat_beh",    "voucher_is_veg_t2",   "voucher_is_veg",     "belief_meat_pre",    "meat",    "Dietary choice",
  "charity_beh", "donation_cents_t2",   "donation_cents",     "belief_charity_pre", "charity", "Charitable donation"
)

set.seed(MEDIATION_SEED)
persist_rows <- list()

for (i in seq_len(nrow(persist_defs))) {
  row <- persist_defs[i, ]
  treat_lab <- unname(condition_labels[row$treat])
  label <- paste0(row$short, "  [", treat_lab, " vs Control]")
  cat("\n=== Fitting:", label, "===\n")
  res <- run_persistence(
    d, y = row$y, m = row$m, t0 = row$t0,
    treat_group = row$treat
  )
  print_persistence(label, res)
  med <- res$med
  persist_rows[[row$key]] <- tibble::tibble(
    outcome    = row$short,
    contrast   = paste0(treat_lab, " vs Control"),
    n          = res$n,
    persist_b  = round(res$b, 3),
    SE         = round(res$b_se, 3),
    p          = fmt_p(res$b_p),
    sig        = fmt_sig(res$b_p),
    pct        = paste0(round(100 * res$b), "%"),
    ACME       = round(med$d.avg, 4),
    ACME_lo    = round(med$d.avg.ci[1], 4),
    ACME_hi    = round(med$d.avg.ci[2], 4),
    ACME_p     = fmt_p(med$d.avg.p),
    sleeper    = round(med$z.avg, 4),
    sleeper_lo = round(med$z.avg.ci[1], 4),
    sleeper_hi = round(med$z.avg.ci[2], 4),
    sleeper_p  = fmt_p(med$z.avg.p),
    total      = round(med$tau.coef, 4),
    total_p    = fmt_p(med$tau.p)
  )
}

section("PERSISTENCE SUMMARY")
cat("persist_b = coefficient on t1 in the t2 model (share of t1 that persists).\n")
cat("pct = 100 × persist_b. sleeper = ADE of own-arm vs control.\n")
cat("ACME = indirect effect (a × b). total = ACME + ADE.\n\n")
print(as.data.frame(bind_rows(persist_rows)), row.names = FALSE)

cat("\nFollow-up analyses complete.\n")
