# =============================================================================
# 04 · Main Analyses
# =============================================================================
# Runs all 6 preregistered analyses on the full sample and with preregistered
# exclusions (>75 on conversation-relevant belief). Also estimates persistence
# rates via mediation (condition -> t1 -> t2).
#
# Cohen's d = model coefficient / a frozen standardizer. Not a raw mean
# difference, and not the SD of a change score.
#   Every contrast is the focal conversation minus the other one. Meat is the
#     treatment for the meat belief, the belief-difference score, the dietary
#     choice, and self-reported meat consumption. Charity is the treatment for
#     the charity belief, the donation, and self-reported giving.
#   Beliefs, the belief-difference score, and the self-reports: SD of that
#     pretreatment item on the full baseline sample, condition ignored. Same
#     number at follow-up and under the preregistered exclusion. The self-report
#     items are winsorized at their own full-baseline 95th percentile first.
#   Donation (3, 6c): no pretreatment behavior, so the standardizer is the
#     within-arm pooled SD of the time-1 donation in both arms. Frozen and
#     reused at follow-up. The percent-of-control figure is still
#     100 * coefficient / mean donation in the meat arm.
#   Voucher (4, 6d): the reported estimate is the logistic coefficient, in
#     log odds. Its d uses a linear-probability model with the same covariate,
#     because a log odds ratio is not an effect in outcome units, divided by
#     the within-arm pooled SD of the time-1 choice in both arms. Frozen and
#     reused at follow-up.
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

source(here::here("scripts", "functions", "helpers.R"))

d1 <- readRDS(here::here("data", "study_1", "d1.rds"))
d  <- readRDS(here::here("data", "study_1", "d.rds"))

# Also get prev_meat_t2 / prev_donate_t2 from follow-up raw data
d2 <- readRDS(here::here("data", "study_1", "d2.rds"))

d <- d |>
  left_join(
    d2 |> select(participant_id, prev_meat_t2 = prev_meat, prev_donate_t2 = prev_donate),
    by = "participant_id"
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

  # Cohen's d denominators, all frozen on the full baseline sample.
  # Pretreatment items: ordinary SD, condition ignored. Arm is not a grouping
  # because these scores were measured before the conversation. The same
  # numbers are used at follow-up and under the preregistered exclusion.
  full_t1 <- make_samples(d1_prep, "none")
  full_t2 <- make_samples(d_prep, "none")
  sd_diff_pre          <- sd(full_t1$all$diff_pre, na.rm = TRUE)
  sd_meat_wrong_pre    <- sd(full_t1$all$meat_wrong_pre, na.rm = TRUE)
  sd_charity_wrong_pre <- sd(full_t1$all$charity_wrong_pre, na.rm = TRUE)
  # Reflection items were also measured before the conversation. Winsorize at
  # the full-baseline 95th percentile, then take the SD, condition ignored.
  sd_prev_meat   <- sd(winsorize_upper(full_t1$all$prev_meat), na.rm = TRUE)
  sd_prev_donate <- sd(winsorize_upper(full_t1$all$prev_donate), na.rm = TRUE)
  # Donation has no pretreatment behavior. Each arm is the other's control, so
  # the standardizer is the within-arm pooled SD of the time-1 donation in
  # both arms. Within-arm pooling keeps the treatment's mean shift out of the
  # denominator. Reused at follow-up.
  fd <- full_t1$charity
  sd_charity_donation_pooled_full <- pooled_sd(
    fd$charity_donation_cents[fd$condition == "Meat"],
    fd$charity_donation_cents[fd$condition == "Charity"]
  )
  mean_donation_control_t1 <- mean(
    fd$charity_donation_cents[fd$condition == "Meat"],
    na.rm = TRUE
  )
  fd2 <- full_t2$charity
  mean_donation_control_t2 <- mean(
    fd2$charity_donation_cents_t2[fd2$condition == "Meat"],
    na.rm = TRUE
  )

  # Voucher: raw % veg voucher by arm & pp diff (Meat − Charity), full baseline only
  fv1 <- full_t1$meat |> mutate(voucher_num = as.numeric(voucher_is_veg))
  pct_voucher_meat_t1_full <- 100 * mean(fv1$voucher_num[fv1$condition == "Meat"], na.rm = TRUE)
  pct_voucher_charity_t1_full <- 100 * mean(fv1$voucher_num[fv1$condition == "Charity"], na.rm = TRUE)
  voucher_pp_diff_full_t1 <- pct_voucher_meat_t1_full - pct_voucher_charity_t1_full
  fv2 <- full_t2$meat |> mutate(voucher_t2_num = as.numeric(voucher_is_veg_t2))
  pct_voucher_meat_t2_full <- 100 * mean(fv2$voucher_t2_num[fv2$condition == "Meat"], na.rm = TRUE)
  pct_voucher_charity_t2_full <- 100 * mean(fv2$voucher_t2_num[fv2$condition == "Charity"], na.rm = TRUE)
  voucher_pp_diff_full_t2 <- pct_voucher_meat_t2_full - pct_voucher_charity_t2_full
  # No pretreatment voucher. Within-arm pooled SD of the time-1 choice,
  # frozen on the full baseline and reused at follow-up.
  sd_voucher_pooled <- pooled_sd(
    fv1$voucher_num[fv1$condition == "Meat"],
    fv1$voucher_num[fv1$condition == "Charity"]
  )

  results <- list()
  add <- function(num, outcome, tp, n, est, se, p,
                  treat_vals, control_vals, treat_label, control_label,
                  sd_pooled = NULL,
                  donation_ref_control_mean = NA_real_,
                  or_log_est = NA_real_, or_log_se = NA_real_,
                  voucher_pct_treat_full = NA_real_,
                  voucher_pct_control_full = NA_real_,
                  voucher_pp_diff_full = NA_real_,
                  d_numerator = NULL) {
    n_treat   <- sum(!is.na(treat_vals))
    n_control <- sum(!is.na(control_vals))
    m_treat   <- mean(treat_vals, na.rm = TRUE)
    m_control <- mean(control_vals, na.rm = TRUE)
    sd_treat  <- sd(treat_vals, na.rm = TRUE)
    sd_control <- sd(control_vals, na.rm = TRUE)
    # Numerator is the model coefficient on the outcome's scale. For the
    # voucher, est is the log odds and d_numerator is the linear-probability
    # coefficient. sd_pooled is the frozen standardizer.
    if (is.null(sd_pooled) || is.na(sd_pooled)) {
      stop("Pass sd_pooled: the frozen standardizer for this outcome.", call. = FALSE)
    }
    sd_standardizer <- sd_pooled
    num_for_d <- if (is.null(d_numerator)) est else d_numerator
    d_used <- num_for_d / sd_pooled
    d_note <- if (is.null(d_numerator)) {
      "d: model coefficient / frozen standardizer"
    } else {
      "d: linear-probability coefficient / pooled SD of the time-1 choice"
    }
    pct_vs_control_mean <- NA_real_
    if (!is.na(donation_ref_control_mean) && donation_ref_control_mean != 0) {
      pct_vs_control_mean <- round(100 * est / donation_ref_control_mean, 2)
    }
    or <- or_lo <- or_hi <- NA_real_
    if (!is.na(or_log_est) && !is.na(or_log_se)) {
      or    <- round(exp(or_log_est), 3)
      or_lo <- round(exp(or_log_est - 1.96 * or_log_se), 3)
      or_hi <- round(exp(or_log_est + 1.96 * or_log_se), 3)
    }
    results[[length(results) + 1]] <<- tibble(
      analysis = num, outcome = outcome, timepoint = tp,
      n = n, d = round(d_used, 3),
      estimate = round(est, 3), se = round(se, 3), p = p,
      treat_label, control_label,
      n_treat, n_control,
      M_treat = round(m_treat, 2), SD_treat = round(sd_treat, 2),
      M_control = round(m_control, 2), SD_control = round(sd_control, 2),
      sd_standardizer = round(sd_standardizer, 2), d_note = d_note,
      donation_ref_control_mean = ifelse(is.na(donation_ref_control_mean), NA_real_,
                                         round(donation_ref_control_mean, 2)),
      pct_vs_control_mean,
      voucher_pct_treat_full = ifelse(is.na(voucher_pct_treat_full), NA_real_,
                                      round(voucher_pct_treat_full, 2)),
      voucher_pct_control_full = ifelse(is.na(voucher_pct_control_full), NA_real_,
                                        round(voucher_pct_control_full, 2)),
      voucher_pp_diff_full = ifelse(is.na(voucher_pp_diff_full), NA_real_,
                                    round(voucher_pp_diff_full, 2)),
      or, or_lo, or_hi
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

  # 3. Donation (Charity = 1); d scaled by full-sample T1 donation pooled SD
  s <- t1$charity
  m <- lm_lin(charity_donation_cents ~ treat_charity, covariates = ~charity_wrong_pre, data = s)
  print_model("Analysis 3: Donation (t1)", m)
  cf <- get_coef(m, "treat_charity")
  add("3", "Donation (cents)", "t1", nrow(s), cf$est, cf$se, cf$p,
      s$charity_donation_cents[s$condition == "Charity"], s$charity_donation_cents[s$condition == "Meat"],
      "Charity", "Meat",
      sd_pooled = sd_charity_donation_pooled_full,
      donation_ref_control_mean = mean_donation_control_t1)

  # 4. Voucher. Reported estimate is the logistic coefficient (Meat - Charity).
  # d uses the linear-probability coefficient from the same covariate.
  s <- t1$meat |> mutate(
    meat_wrong_pre_c = as.numeric(scale(meat_wrong_pre, scale = FALSE)),
    voucher_num      = as.numeric(voucher_is_veg)
  )
  m <- glm(voucher_num ~ treat_meat * meat_wrong_pre_c, data = s, family = binomial)
  print_model("Analysis 4: Voucher logistic (t1)", m)
  cf <- get_coef(m, "treat_meat")
  m_lpm <- lm_lin(voucher_num ~ treat_meat, covariates = ~meat_wrong_pre, data = s)
  cf_lpm <- get_coef(m_lpm, "treat_meat")
  add("4", "Voucher (logistic)", "t1", nrow(s), cf$est, cf$se, cf$p,
      s$voucher_num[s$condition == "Meat"], s$voucher_num[s$condition == "Charity"],
      "Meat", "Charity",
      sd_pooled = sd_voucher_pooled,
      d_numerator = cf_lpm$est,
      or_log_est = cf$est, or_log_se = cf$se,
      voucher_pct_treat_full = pct_voucher_meat_t1_full,
      voucher_pct_control_full = pct_voucher_charity_t1_full,
      voucher_pp_diff_full = voucher_pp_diff_full_t1)

  # ---- Analysis 5: Reflection DVs (winsorized per preregistration) ----
  s5 <- t2$all |> mutate(
    prev_meat_w      = winsorize_upper(prev_meat),
    prev_meat_t2_w   = winsorize_upper(prev_meat_t2),
    prev_donate_w    = winsorize_upper(prev_donate),
    prev_donate_t2_w = winsorize_upper(prev_donate_t2),
    change_atemeat   = prev_meat_t2_w   - prev_meat_w,
    change_donated   = prev_donate_t2_w - prev_donate_w
  )

  # 5a. Meat consumption. d = coefficient / full-baseline SD of the
  # winsorized pretreatment item (sd_prev_meat).
  m <- lm_lin(change_atemeat ~ treat_meat, covariates = ~prev_meat_w, data = s5)
  print_model("Analysis 5a: Reflection meat consumption (t2)", m)
  cf <- get_coef(m, "treat_meat")
  add("5a", "Reflection: meat consumption", "t2", nrow(s5), cf$est, cf$se, cf$p,
      s5$change_atemeat[s5$condition == "Meat"], s5$change_atemeat[s5$condition == "Charity"],
      "Meat", "Charity", sd_pooled = sd_prev_meat)

  # 5b. Donation reflection. d = coefficient / full-baseline SD of the
  # winsorized pretreatment item (sd_prev_donate).
  m <- lm_lin(change_donated ~ treat_charity, covariates = ~prev_donate_w, data = s5)
  print_model("Analysis 5b: Reflection donation (t2)", m)
  cf <- get_coef(m, "treat_charity")
  add("5b", "Reflection: donation", "t2", nrow(s5), cf$est, cf$se, cf$p,
      s5$change_donated[s5$condition == "Charity"], s5$change_donated[s5$condition == "Meat"],
      "Charity", "Meat", sd_pooled = sd_prev_donate)

  # ---- t2 analyses (Analysis 6). Same frozen standardizers as time 1. ----

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

  # 6c. Donation (same d standardizer as Analysis 3)
  s <- t2$charity
  m <- lm_lin(charity_donation_cents_t2 ~ treat_charity, covariates = ~charity_wrong_pre, data = s)
  print_model("Analysis 6c: Donation (t2)", m)
  cf <- get_coef(m, "treat_charity")
  add("6c", "Donation (cents)", "t2", nrow(s), cf$est, cf$se, cf$p,
      s$charity_donation_cents_t2[s$condition == "Charity"], s$charity_donation_cents_t2[s$condition == "Meat"],
      "Charity", "Meat",
      sd_pooled = sd_charity_donation_pooled_full,
      donation_ref_control_mean = mean_donation_control_t2)

  # 6d. Same voucher standardizer as analysis 4. Estimate stays the log odds.
  s <- t2$meat |> mutate(
    meat_wrong_pre_c = as.numeric(scale(meat_wrong_pre, scale = FALSE)),
    voucher_t2_num   = as.numeric(voucher_is_veg_t2)
  )
  m <- glm(voucher_t2_num ~ treat_meat * meat_wrong_pre_c, data = s, family = binomial)
  print_model("Analysis 6d: Voucher logistic (t2)", m)
  cf <- get_coef(m, "treat_meat")
  m_lpm <- lm_lin(voucher_t2_num ~ treat_meat, covariates = ~meat_wrong_pre, data = s)
  cf_lpm <- get_coef(m_lpm, "treat_meat")
  add("6d", "Voucher (logistic)", "t2", nrow(s), cf$est, cf$se, cf$p,
      s$voucher_t2_num[s$condition == "Meat"], s$voucher_t2_num[s$condition == "Charity"],
      "Meat", "Charity",
      sd_pooled = sd_voucher_pooled,
      d_numerator = cf_lpm$est,
      or_log_est = cf$est, or_log_se = cf$se,
      voucher_pct_treat_full = pct_voucher_meat_t2_full,
      voucher_pct_control_full = pct_voucher_charity_t2_full,
      voucher_pp_diff_full = voucher_pp_diff_full_t2)

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
  df |> mutate(p_fmt = fmt_p(p), sig = fmt_sig(p))
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

# -----------------------------------------------------------------------------
# Post-hoc Benjamini–Hochberg FDR correction over eight preregistered p-values:
# Immediate: 2a, 2b, 3, 4. Follow-up: 6b-i through 6d.
# Excludes belief difference (1, 6a), reflection DVs (5a–b).
# Voucher (4, 6d): logistic Wald p-values; others Lin estimator.
# -----------------------------------------------------------------------------

bh_family_order <- tibble(
  analysis = c("2a", "2b", "3", "4", "6b-i", "6b-ii", "6c", "6d"),
  bh_ord = seq_len(8)
)

print_bh_fdr <- function(df, label) {
  section(paste0("POST-HOC BENJAMINI-HOCHBERG FDR (8 preregistered p-values): ", label), char = "#")
  out <- df |>
    filter(analysis %in% bh_family_order$analysis) |>
    inner_join(bh_family_order, by = "analysis") |>
    arrange(bh_ord) |>
    mutate(
      p_bh = stats::p.adjust(p, method = "BH"),
      sig_nominal = p < 0.05,
      sig_bh = p_bh < 0.05
    )
  cat(
    "Family = eight two-sided treatment-effect p-values: analyses 2a, 2b, 3, 4 ",
    "(immediate) and 6b-i through 6d (follow-up): meat belief change, charity belief ",
    "change, donation, and voucher (Lin for beliefs/donation; logistic Wald for ",
    "voucher). Excludes belief-difference composite (1, 6a) and reflection (5a–b).\n\n",
    sep = ""
  )
  disp <- out |>
    mutate(
      p_fmt = ifelse(p < 0.001, "<.001", sprintf("%.3f", p)),
      p_bh_fmt = ifelse(p_bh < 0.001, "<.001", sprintf("%.3f", p_bh)),
      sig_nominal_chr = ifelse(sig_nominal, "*", ""),
      sig_bh_chr = ifelse(sig_bh, "*", "")
    ) |>
    select(analysis, outcome, timepoint,
           p_fmt, p_bh_fmt, sig_nominal_chr, sig_bh_chr)
  names(disp) <- c(
    "analysis", "outcome", "timepoint",
    "p", "p_BH", "sig_p<.05", "sig_BH"
  )
  print(as.data.frame(disp), row.names = FALSE, right = FALSE)
  inference_changed <- any(out$sig_nominal != out$sig_bh)
  cat("\n")
  if (inference_changed) {
    cat(
      "NOTE: At least one conclusion at alpha = .05 differs after BH correction.\n"
    )
  } else {
    cat(
      "Nominal two-sided significance at alpha = .05 matches BH-adjusted ",
      "significance for every test in this family.\n"
    )
  }
}

print_bh_fdr(res_full, "full sample (primary)")
print_bh_fdr(res_prereg, "preregistered exclusion sample")

print_interpretable <- function(df, label) {
  section(label, char = "#")
  cat(
    "Donation: estimate & SE are Lin adjusted effects (Charity vs Meat), in cents.\n",
    "pct_vs_control_mean = 100 * estimate / mean donation in control (Meat), ",
    "full baseline sample at that wave.\n",
    "Voucher: raw display = full-sample % choosing veg voucher (Meat arm, Charity arm) ",
    "and pp diff (Meat − Charity); model = logistic with OR = exp(coef), 95% Wald CI.\n\n",
    sep = ""
  )
  don <- df |>
    filter(outcome == "Donation (cents)") |>
    select(analysis, outcome, timepoint, estimate, se,
           donation_ref_control_mean, pct_vs_control_mean, d)
  cat("--- Donation ---\n")
  print(as.data.frame(don), row.names = FALSE, right = FALSE)
  cat("\n--- Voucher ---\n")
  vch <- df |>
    filter(outcome == "Voucher (logistic)") |>
    mutate(
      or_ci = paste0(or, " [", or_lo, ", ", or_hi, "]")
    ) |>
    select(analysis, outcome, timepoint, estimate, se, p,
           voucher_pct_treat_full, voucher_pct_control_full, voucher_pp_diff_full,
           or_ci)
  print(as.data.frame(vch), row.names = FALSE, right = FALSE)
}

print_interpretable(res_full, "INTERPRETABLE SCALES: Donation (cents, % vs control mean), Voucher (full-sample pp, OR)")
print_interpretable(res_prereg, "INTERPRETABLE SCALES — preregistered exclusion sample")

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

# Do not attach mediation: it pulls MASS onto the search path and masks
# dplyr::select in every script sourced after this one.
invisible(loadNamespace("mediation"))
import_mediation_settings()
announce_mediation_settings()

# Study 1 has no order factor, and each arm is the other's control.
# This is not the Study 2 helper in mediation_helpers.R. The bootstrap
# count, bootstrap switch, and seed come from that file.
run_mediation <- function(data, outcome, mediator, t0,
                          control_group, treat_group,
                          sims = mediation_n_sims(),
                          boot = mediation_bootstrap_enabled()) {
  data_name <- substitute(data)

  f1 <- paste0(mediator, " ~ condition + ", t0)
  f2 <- paste0(outcome, " ~ condition + ", mediator, " + ", t0)

  m1 <- eval.parent(call("lm", formula = as.formula(f1), data = data_name))
  m2 <- eval.parent(call("lm", formula = as.formula(f2), data = data_name))

  med <- mediation::mediate(
    model.m = m1, model.y = m2,
    treat = "condition", mediator = mediator,
    control.value = control_group, treat.value = treat_group,
    boot = boot, sims = sims
  )

  co <- summary(m2)$coefficients[mediator, ]
  list(
    m1 = m1, m2 = m2, med = med,
    b = co["Estimate"], se = co["Std. Error"], p = co["Pr(>|t|)"]
  )
}

set.seed(MEDIATION_SEED)

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
  cat(if (mediation_bootstrap_enabled()) {
    "\nMediation summary (bootstrap):\n"
  } else {
    "\nMediation summary (quasi-Bayesian smoke test; not the reported bootstrap):\n"
  })
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

# =============================================================================
# EXPLORATORY HETEROGENEITY (AOT / CRT moderation)
# =============================================================================
# Sourced module: outcome ~ condition * t_0 + condition * moderator;
# BH-FDR on condition × moderator interactions (AOT and CRT families).

source(here::here("scripts", "study_1", "moderation_linear_crt_aot.R"))
run_crt_aot_heterogeneity(d1, d, verbose = TRUE)

section("DONE", char = "#")
