# =============================================================================
# 04 · Main Analyses
# =============================================================================
# Runs all 6 preregistered analyses on the full sample and with preregistered
# exclusions (>75 on conversation-relevant belief). Also estimates persistence
# rates via mediation (condition -> t1 -> t2).
#
# Cohen's d specification:
#   Belief change measures (have t0): d = (M_treat - M_control) / pooled_SD(pre-treatment, full baseline)
#     Pre-treatment: diff_pre (1,6a), meat_wrong_pre (2a,6b-i), charity_wrong_pre (2b,6b-ii)
#   Outcomes without t0: d = (M_treat - M_control) / pooled_SD(outcome)
#     Donation (3,6c), voucher (4,6d), reflection meat (5a), reflection donation (5b)
#
# Preregistered analyses:
#   1.  Belief difference (charity - meat)
#   2a. Meat belief change
#   2b. Charity belief change
#   3.  Donation (Lin estimator)
#   4.  Voucher (logistic regression)
#   5.  Reflection DVs (winsorized, follow-up)
#   6a-d. All of the above at follow-up
#
# Persistence analysis:
#   Regresses each follow-up outcome on corresponding post-treatment outcome,
#   following Lin et al. (2025).

library(dplyr)
library(tidyr)
library(estimatr)
library(effsize)

source(here::here("scripts", "functions", "helpers.R"))

d1 <- readRDS(here::here("data", "d1.rds"))
d  <- readRDS(here::here("data", "d.rds"))

# Also get prev_meat_t2 / prev_donate_t2 from follow-up raw data
d2 <- readRDS(here::here("data", "d2.rds"))

d <- d |>
  left_join(
    d2 |> select(PROLIFIC_PID, prev_meat_t2 = prev_meat, prev_donate_t2 = prev_donate),
    by = "PROLIFIC_PID"
  )

cat("Initial sample:   N =", nrow(d1), "\n")
cat("Follow-up sample: N =", nrow(d), "\n")

# =============================================================================
# DATA PREPARATION
# =============================================================================

prepare_t1 <- function(data) {
  data |> mutate(
    treat_meat    = as.numeric(condition == "Meat"),
    treat_charity = as.numeric(condition == "Charity"),
    diff_pre      = charity_wrong_pre - meat_wrong_pre,
    diff_post     = charity_wrong_post - meat_wrong_post,
    change_diff   = diff_post - diff_pre
  )
}

prepare_t2 <- function(data) {
  data |> mutate(
    treat_meat              = as.numeric(condition == "Meat"),
    treat_charity           = as.numeric(condition == "Charity"),
    meat_wrong_change_t2    = meat_wrong_t2 - meat_wrong_pre,
    charity_wrong_change_t2 = charity_wrong_t2 - charity_wrong_pre,
    diff_pre                = charity_wrong_pre - meat_wrong_pre,
    diff_t2                 = charity_wrong_t2  - meat_wrong_t2,
    change_diff_t2          = diff_t2 - diff_pre
  )
}

make_samples <- function(data, regime) {
  if (regime == "treatment_only") {
    list(
      all = data |>
        filter(!(condition == "Meat"    & meat_wrong_pre    > 75),
               !(condition == "Charity" & charity_wrong_pre > 75)),
      meat = data |>
        filter(!(condition == "Meat" & meat_wrong_pre > 75)),
      charity = data |>
        filter(!(condition == "Charity" & charity_wrong_pre > 75))
    )
  } else {
    list(all = data, meat = data, charity = data)
  }
}

# =============================================================================
# ANALYSIS RUNNER
# =============================================================================

get_coef <- function(model, treat_var) {
  co <- summary(model)$coefficients
  p_col <- if ("Pr(>|t|)" %in% colnames(co)) "Pr(>|t|)" else "Pr(>|z|)"
  list(
    est = co[treat_var, "Estimate"],
    se  = co[treat_var, "Std. Error"],
    p   = co[treat_var, p_col]
  )
}

run_all <- function(d1_prep, d_prep, regime, verbose = TRUE) {

  t1 <- make_samples(d1_prep, regime)
  t2 <- make_samples(d_prep, regime)

  # Pooled SDs of pre-treatment variables from full baseline sample
  full_t1 <- make_samples(d1_prep, "none")
  full_t2 <- make_samples(d_prep, "none")
  sd_diff_pre          <- pooled_sd(full_t1$all$diff_pre[full_t1$all$condition == "Meat"],
                                    full_t1$all$diff_pre[full_t1$all$condition == "Charity"])
  sd_meat_wrong_pre    <- pooled_sd(full_t1$all$meat_wrong_pre[full_t1$all$condition == "Meat"],
                                    full_t1$all$meat_wrong_pre[full_t1$all$condition == "Charity"])
  sd_charity_wrong_pre <- pooled_sd(full_t1$all$charity_wrong_pre[full_t1$all$condition == "Charity"],
                                    full_t1$all$charity_wrong_pre[full_t1$all$condition == "Meat"])
  results <- list()
  add <- function(num, outcome, tp, n, est, se, p,
                  treat_vals, control_vals, treat_label, control_label,
                  sd_pooled = NULL) {
    n_treat   <- sum(!is.na(treat_vals))
    n_control <- sum(!is.na(control_vals))
    m_treat   <- mean(treat_vals, na.rm = TRUE)
    m_control <- mean(control_vals, na.rm = TRUE)
    sd_treat  <- sd(treat_vals, na.rm = TRUE)
    sd_control <- sd(control_vals, na.rm = TRUE)
    if (is.null(sd_pooled)) {
      sd_standardizer <- pooled_sd(treat_vals, control_vals)
      d_used <- safe_d(treat_vals, control_vals)
      d_note <- "d: outcome pooled SD"
    } else {
      sd_standardizer <- sd_pooled
      d_used <- safe_d_with_sd(treat_vals, control_vals, sd_pooled)
      d_note <- "d: pre-treatment pooled SD"
    }
    results[[length(results) + 1]] <<- tibble(
      analysis = num, outcome = outcome, timepoint = tp,
      n = n, d = round(d_used, 3),
      estimate = round(est, 3), se = round(se, 3), p = p,
      treat_label, control_label,
      n_treat, n_control,
      M_treat = round(m_treat, 2), SD_treat = round(sd_treat, 2),
      M_control = round(m_control, 2), SD_control = round(sd_control, 2),
      sd_standardizer = round(sd_standardizer, 2), d_note = d_note
    )
  }

  print_model <- function(label, model) {
    if (!verbose) return(invisible(NULL))
    cat("\n------", label, "------\n")
    print(summary(model))
  }

  # ---- t1 analyses ----

  # 1. Belief difference (change_diff = charity - meat; Meat condition expected to decrease)
  s <- t1$all
  m <- lm_lin(change_diff ~ treat_meat, covariates = ~diff_pre, data = s)
  print_model("Analysis 1: Belief difference (t1)", m)
  cf <- get_coef(m, "treat_meat")
  add("1", "Belief difference", "t1", nrow(s), cf$est, cf$se, cf$p,
      s$change_diff[s$condition == "Meat"], s$change_diff[s$condition == "Charity"],
      "Meat", "Charity", sd_pooled = sd_diff_pre)

  # 2a. Meat belief change (Meat = 1)
  s <- t1$meat
  m <- lm_lin(meat_wrong_change ~ treat_meat, covariates = ~meat_wrong_pre, data = s)
  print_model("Analysis 2a: Meat belief change (t1)", m)
  cf <- get_coef(m, "treat_meat")
  add("2a", "Meat belief change", "t1", nrow(s), cf$est, cf$se, cf$p,
      s$meat_wrong_change[s$condition == "Meat"], s$meat_wrong_change[s$condition == "Charity"],
      "Meat", "Charity", sd_pooled = sd_meat_wrong_pre)

  # 2b. Charity belief change (Charity = 1)
  s <- t1$charity
  m <- lm_lin(charity_wrong_change ~ treat_charity, covariates = ~charity_wrong_pre, data = s)
  print_model("Analysis 2b: Charity belief change (t1)", m)
  cf <- get_coef(m, "treat_charity")
  add("2b", "Charity belief change", "t1", nrow(s), cf$est, cf$se, cf$p,
      s$charity_wrong_change[s$condition == "Charity"], s$charity_wrong_change[s$condition == "Meat"],
      "Charity", "Meat", sd_pooled = sd_charity_wrong_pre)

  # 3. Donation (Charity = 1) — no t0 for outcome, use outcome pooled SD
  s <- t1$charity
  m <- lm_lin(charity_donation_cents ~ treat_charity, covariates = ~charity_wrong_pre, data = s)
  print_model("Analysis 3: Donation (t1)", m)
  cf <- get_coef(m, "treat_charity")
  add("3", "Donation (cents)", "t1", nrow(s), cf$est, cf$se, cf$p,
      s$charity_donation_cents[s$condition == "Charity"], s$charity_donation_cents[s$condition == "Meat"],
      "Charity", "Meat")

  # 4. Voucher (logistic, Meat = 1) — no t0 for outcome, use outcome pooled SD
  s <- t1$meat |> mutate(
    meat_wrong_pre_c = as.numeric(scale(meat_wrong_pre, scale = FALSE)),
    voucher_num      = as.numeric(voucher_is_veg)
  )
  m <- glm(voucher_num ~ treat_meat * meat_wrong_pre_c, data = s, family = binomial)
  print_model("Analysis 4: Voucher logistic (t1)", m)
  cf <- get_coef(m, "treat_meat")
  add("4", "Voucher (logistic)", "t1", nrow(s), cf$est, cf$se, cf$p,
      s$voucher_num[s$condition == "Meat"], s$voucher_num[s$condition == "Charity"],
      "Meat", "Charity")

  # ---- Analysis 5: Reflection DVs (winsorized per preregistration) ----
  s5 <- t2$all |> mutate(
    prev_meat_w      = winsorize_upper(prev_meat),
    prev_meat_t2_w   = winsorize_upper(prev_meat_t2),
    prev_donate_w    = winsorize_upper(prev_donate),
    prev_donate_t2_w = winsorize_upper(prev_donate_t2),
    change_atemeat   = prev_meat_t2_w   - prev_meat_w,
    change_donated   = prev_donate_t2_w - prev_donate_w
  )

  # 5a. Meat consumption reflection (Meat = 1) — no t0 for outcome, use outcome pooled SD
  m <- lm_lin(change_atemeat ~ treat_meat, covariates = ~prev_meat_w, data = s5)
  print_model("Analysis 5a: Reflection meat consumption (t2)", m)
  cf <- get_coef(m, "treat_meat")
  add("5a", "Reflection: meat consumption", "t2", nrow(s5), cf$est, cf$se, cf$p,
      s5$change_atemeat[s5$condition == "Meat"], s5$change_atemeat[s5$condition == "Charity"],
      "Meat", "Charity")

  # 5b. Donation reflection (Charity = 1) — no t0 for outcome, use outcome pooled SD
  m <- lm_lin(change_donated ~ treat_charity, covariates = ~prev_donate_w, data = s5)
  print_model("Analysis 5b: Reflection donation (t2)", m)
  cf <- get_coef(m, "treat_charity")
  add("5b", "Reflection: donation", "t2", nrow(s5), cf$est, cf$se, cf$p,
      s5$change_donated[s5$condition == "Charity"], s5$change_donated[s5$condition == "Meat"],
      "Charity", "Meat")

  # ---- t2 analyses (Analysis 6) — d = (M_treat - M_control) / pooled_SD(pre-treatment) ----

  # 6a. Belief difference — pre = diff_pre
  s <- t2$all
  m <- lm_lin(change_diff_t2 ~ treat_meat, covariates = ~diff_pre, data = s)
  print_model("Analysis 6a: Belief difference (t2)", m)
  cf <- get_coef(m, "treat_meat")
  add("6a", "Belief difference", "t2", nrow(s), cf$est, cf$se, cf$p,
      s$change_diff_t2[s$condition == "Meat"], s$change_diff_t2[s$condition == "Charity"],
      "Meat", "Charity", sd_pooled = sd_diff_pre)

  # 6b-i. Meat belief — pre = meat_wrong_pre
  s <- t2$meat
  m <- lm_lin(meat_wrong_change_t2 ~ treat_meat, covariates = ~meat_wrong_pre, data = s)
  print_model("Analysis 6b-i: Meat belief change (t2)", m)
  cf <- get_coef(m, "treat_meat")
  add("6b-i", "Meat belief change", "t2", nrow(s), cf$est, cf$se, cf$p,
      s$meat_wrong_change_t2[s$condition == "Meat"], s$meat_wrong_change_t2[s$condition == "Charity"],
      "Meat", "Charity", sd_pooled = sd_meat_wrong_pre)

  # 6b-ii. Charity belief — pre = charity_wrong_pre
  s <- t2$charity
  m <- lm_lin(charity_wrong_change_t2 ~ treat_charity, covariates = ~charity_wrong_pre, data = s)
  print_model("Analysis 6b-ii: Charity belief change (t2)", m)
  cf <- get_coef(m, "treat_charity")
  add("6b-ii", "Charity belief change", "t2", nrow(s), cf$est, cf$se, cf$p,
      s$charity_wrong_change_t2[s$condition == "Charity"], s$charity_wrong_change_t2[s$condition == "Meat"],
      "Charity", "Meat", sd_pooled = sd_charity_wrong_pre)

  # 6c. Donation — no t0 for outcome, use outcome pooled SD
  s <- t2$charity
  m <- lm_lin(charity_donation_cents_t2 ~ treat_charity, covariates = ~charity_wrong_pre, data = s)
  print_model("Analysis 6c: Donation (t2)", m)
  cf <- get_coef(m, "treat_charity")
  add("6c", "Donation (cents)", "t2", nrow(s), cf$est, cf$se, cf$p,
      s$charity_donation_cents_t2[s$condition == "Charity"], s$charity_donation_cents_t2[s$condition == "Meat"],
      "Charity", "Meat")

  # 6d. Voucher — no t0 for outcome, use outcome pooled SD
  s <- t2$meat |> mutate(
    meat_wrong_pre_c = as.numeric(scale(meat_wrong_pre, scale = FALSE)),
    voucher_t2_num   = as.numeric(voucher_is_veg_t2)
  )
  m <- glm(voucher_t2_num ~ treat_meat * meat_wrong_pre_c, data = s, family = binomial)
  print_model("Analysis 6d: Voucher logistic (t2)", m)
  cf <- get_coef(m, "treat_meat")
  add("6d", "Voucher (logistic)", "t2", nrow(s), cf$est, cf$se, cf$p,
      s$voucher_t2_num[s$condition == "Meat"], s$voucher_t2_num[s$condition == "Charity"],
      "Meat", "Charity")

  bind_rows(results) |> mutate(regime = regime)
}

# =============================================================================
# RUN PREREGISTERED ANALYSES
# =============================================================================

d1_prep <- prepare_t1(d1)
d_prep  <- prepare_t2(d)

res_full   <- run_all(d1_prep, d_prep, "none")
res_prereg <- run_all(d1_prep, d_prep, "treatment_only")

fmt <- function(df) {
  df |> mutate(
    p_fmt = case_when(p < 0.001 ~ "<.001", TRUE ~ sprintf("%.3f", p)),
    sig   = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**",
                      p < 0.05 ~ "*", p < 0.1 ~ ".", TRUE ~ "")
  )
}

print_table <- function(df, label) {
  section(label, char = "#")
  df |>
    select(analysis, outcome, timepoint, n, d, estimate, se, p_fmt, sig) |>
    as.data.frame() |>
    print(row.names = FALSE, right = FALSE)
}

print_descriptives <- function(df, label) {
  section(label, char = "#")
  desc <- df |>
    mutate(
      treat_M_SD   = paste0(M_treat, " (", SD_treat, ")"),
      control_M_SD = paste0(M_control, " (", SD_control, ")")
    ) |>
    select(analysis, outcome, timepoint, treat_label, control_label,
           n_treat, n_control, treat_M_SD, control_M_SD, sd_standardizer, d, d_note)
  print(as.data.frame(desc), row.names = FALSE, right = FALSE)
}

print_descriptives(res_full, "DESCRIPTIVES: Means (SD) by condition — FULL SAMPLE")
print_table(fmt(res_full),   "1. FULL SAMPLE (primary, reported in paper)")
print_descriptives(res_prereg, "DESCRIPTIVES: Means (SD) by condition — PREREGISTERED EXCLUSION")
print_table(fmt(res_prereg), "2. PREREGISTERED EXCLUSION (>75 from treatment group only)")

# =============================================================================
# SIDE-BY-SIDE COMPARISON
# =============================================================================

section("SIDE-BY-SIDE COMPARISON", char = "#")

side <- bind_rows(
  fmt(res_full)   |> mutate(regime = "full"),
  fmt(res_prereg) |> mutate(regime = "prereg")
) |>
  select(analysis, outcome, timepoint, regime, n, d, p_fmt, sig) |>
  pivot_wider(
    names_from  = regime,
    values_from = c(n, d, p_fmt, sig),
    names_glue  = "{regime}_{.value}"
  ) |>
  select(analysis, outcome, timepoint,
         full_n, full_d, full_p_fmt, full_sig,
         prereg_n, prereg_d, prereg_p_fmt, prereg_sig)

print(as.data.frame(side), row.names = FALSE, right = FALSE)

# =============================================================================
# PERSISTENCE RATE ANALYSIS
# =============================================================================
# Mediation approach: condition (t0) -> t1 outcome (mediator) -> t2 outcome,
# controlling for the pre-treatment baseline.
#   Model 1 (mediator): t1_outcome ~ condition + t0_baseline
#   Model 2 (outcome):  t2_outcome ~ condition + t1_outcome + t0_baseline
# The persistence rate is the b coefficient on the mediator (t1) in Model 2.

section("PERSISTENCE RATES (mediation: condition -> t1 -> t2)")

library(mediation)

run_mediation <- function(data, outcome, mediator, t0,
                          control_group, treat_group, sims = 1000) {
  data_name <- substitute(data)

  f1 <- paste0(mediator, " ~ condition + ", t0)
  f2 <- paste0(outcome, " ~ condition + ", mediator, " + ", t0)

  m1 <- eval.parent(call("lm", formula = as.formula(f1), data = data_name))
  m2 <- eval.parent(call("lm", formula = as.formula(f2), data = data_name))

  med <- mediation::mediate(
    model.m = m1, model.y = m2,
    treat = "condition", mediator = mediator,
    control.value = control_group, treat.value = treat_group,
    boot = TRUE, sims = sims
  )

  co <- summary(m2)$coefficients[mediator, ]
  list(
    m1 = m1, m2 = m2, med = med,
    b = co["Estimate"], se = co["Std. Error"], p = co["Pr(>|t|)"]
  )
}

set.seed(42)

d_med <- d |> mutate(
  voucher_is_veg    = as.numeric(voucher_is_veg),
  voucher_is_veg_t2 = as.numeric(voucher_is_veg_t2)
)

print_mediation <- function(label, res) {
  cat("\n------", label, "------\n")
  cat("\nModel 1 (mediator model):\n")
  print(summary(res$m1))
  cat("\nModel 2 (outcome model):\n")
  print(summary(res$m2))
  cat("\nMediation summary:\n")
  print(summary(res$med))
}

cat("\n--- Meat belief persistence ---\n")
med_meat <- run_mediation(
  d_med, outcome = "meat_wrong_t2", mediator = "meat_wrong_post",
  t0 = "meat_wrong_pre", control_group = "Charity", treat_group = "Meat"
)
print_mediation("Meat belief persistence", med_meat)

cat("\n--- Charity belief persistence ---\n")
med_charity <- run_mediation(
  d_med, outcome = "charity_wrong_t2", mediator = "charity_wrong_post",
  t0 = "charity_wrong_pre", control_group = "Meat", treat_group = "Charity"
)
print_mediation("Charity belief persistence", med_charity)

cat("\n--- Voucher persistence ---\n")
med_voucher <- run_mediation(
  d_med, outcome = "voucher_is_veg_t2", mediator = "voucher_is_veg",
  t0 = "meat_wrong_pre", control_group = "Charity", treat_group = "Meat"
)
print_mediation("Voucher persistence", med_voucher)

cat("\n--- Donation persistence ---\n")
med_donation <- run_mediation(
  d_med, outcome = "charity_donation_cents_t2", mediator = "charity_donation_cents",
  t0 = "charity_wrong_pre", control_group = "Meat", treat_group = "Charity"
)
print_mediation("Donation persistence", med_donation)

persist_df <- bind_rows(
  tibble(outcome = "Meat belief",      b = round(med_meat$b, 3),
         se = round(med_meat$se, 3),   p = med_meat$p),
  tibble(outcome = "Charity belief",   b = round(med_charity$b, 3),
         se = round(med_charity$se, 3), p = med_charity$p),
  tibble(outcome = "Voucher (veg)",    b = round(med_voucher$b, 3),
         se = round(med_voucher$se, 3), p = med_voucher$p),
  tibble(outcome = "Donation (cents)", b = round(med_donation$b, 3),
         se = round(med_donation$se, 3), p = med_donation$p)
) |>
  mutate(
    p_fmt = case_when(p < 0.001 ~ "<.001", TRUE ~ sprintf("%.3f", p)),
    pct_persist = paste0(round(b * 100), "%")
  )

cat("\n--- Persistence rates (b on mediator in outcome model) ---\n")
print(as.data.frame(persist_df), row.names = FALSE, right = FALSE)

section("DONE", char = "#")
