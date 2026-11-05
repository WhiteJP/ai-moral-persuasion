# =============================================================================
# 06 · Preregistered exclusion table (Study 1 pilot)
# =============================================================================
# Runs the six preregistered analyses twice:
#   1. Full sample (primary, reported in the paper)
#   2. Preregistered exclusion, by which conversation is the treatment.
#      Meat belief, the voucher, and the meat self-report share one sample:
#        drop Meat-arm participants with meat_wrong_pre > 75.
#      Charity belief, the donation, and the donation self-report share one:
#        drop Charity-arm participants with charity_wrong_pre > 75.
#      The belief-difference score uses both cuts, because both statements
#      enter that outcome. A score of 75 is kept.
#
# Analysis 5 winsorizes the reflection outcomes at the 95th percentile.
# Writes the full-sample and exclusion estimates side by side to
# output/study_1/tables/pilot-full-vs-exclusion.tex, and, when
# save_to_paper_repo is TRUE, copies that table into the manuscript repo.

library(dplyr)
library(tidyr)
library(estimatr)

source(here::here("scripts", "functions", "helpers.R"))

d1 <- readRDS(here::here("data", "study_1", "d1.rds"))
d  <- readRDS(here::here("data", "study_1", "d.rds"))
d2 <- readRDS(here::here("data", "study_1", "d2.rds"))

d <- d |>
  left_join(
    d2 |> select(PROLIFIC_PID, prev_meat_t2 = prev_meat, prev_donate_t2 = prev_donate),
    by = "PROLIFIC_PID"
  )

cat("Initial sample:   N =", nrow(d1), "\n")
cat("Follow-up sample: N =", nrow(d), "\n")

# =============================================================================
# HELPERS
# =============================================================================

winsorize_upper <- function(x, p = 0.95) {
  pmin(x, quantile(x, p, na.rm = TRUE))
}

# Cohen's d = model coefficient / a frozen standardizer. Computed once on
# the full baseline sample and reused at follow-up and in the exclusion run.
# Not a raw mean difference, and not the SD of a change score.
# Pretreatment items: ordinary SD, condition ignored.
sd_meat_pre    <- sd(d1$meat_wrong_pre, na.rm = TRUE)
sd_charity_pre <- sd(d1$charity_wrong_pre, na.rm = TRUE)
sd_diff_pre    <- sd(d1$charity_wrong_pre - d1$meat_wrong_pre, na.rm = TRUE)
# Reflection items were measured before the conversation. Winsorize at the
# full-baseline 95th percentile, then take the SD, condition ignored.
sd_prev_meat   <- sd(winsorize_upper(d1$prev_meat), na.rm = TRUE)
sd_prev_donate <- sd(winsorize_upper(d1$prev_donate), na.rm = TRUE)
# Donation and the voucher have no pretreatment behavior. Each arm is the
# other's control, so these are the within-arm pooled SDs of the time-1
# outcomes in both arms.
sd_donation <- pooled_sd(
  d1$charity_donation_cents[d1$condition == "Meat"],
  d1$charity_donation_cents[d1$condition == "Charity"]
)
sd_voucher <- pooled_sd(
  as.numeric(d1$voucher_is_veg[d1$condition == "Meat"]),
  as.numeric(d1$voucher_is_veg[d1$condition == "Charity"])
)

# Numerator is the model coefficient on the outcome's scale.
d_from_coef <- function(estimate, sd_standardizer) {
  estimate / sd_standardizer
}

# condition_cc codes Charity as +0.5 and Meat as -0.5, so its coefficient is
# Charity minus Meat. Meat-focal analyses report Meat minus Charity.
flip_to_meat <- function(cf) {
  cf$est <- -cf$est
  cf
}

# =============================================================================
# DATA PREPARATION
# =============================================================================

prepare_t1 <- function(data) {
  data |> mutate(
    condition_cc = ifelse(condition == "Meat", -0.5, 0.5),
    diff_pre     = charity_wrong_pre - meat_wrong_pre,
    diff_post    = charity_wrong_post - meat_wrong_post,
    change_diff  = diff_post - diff_pre
  )
}

prepare_t2 <- function(data) {
  data |> mutate(
    condition_cc            = ifelse(condition == "Meat", -0.5, 0.5),
    meat_wrong_change_t2    = meat_wrong_t2 - meat_wrong_pre,
    charity_wrong_change_t2 = charity_wrong_t2 - charity_wrong_pre,
    diff_pre                = charity_wrong_pre - meat_wrong_pre,
    diff_t2                 = charity_wrong_t2  - meat_wrong_t2,
    change_diff_t2          = diff_t2 - diff_pre
  )
}

# Exclusion is the treatment arm for that outcome. `all` applies both cuts
# and is used only for the belief-difference score.
make_samples <- function(data, regime) {
  if (regime == "preregistered") {
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

get_coef <- function(model, row_pattern = "condition_cc") {
  co <- summary(model)$coefficients
  idx <- grep(paste0("^", row_pattern, "$"), rownames(co))
  if (length(idx) == 0) idx <- grep(row_pattern, rownames(co))[1]
  p_col <- if ("Pr(>|t|)" %in% colnames(co)) "Pr(>|t|)" else "Pr(>|z|)"
  list(est = co[idx, "Estimate"], se = co[idx, "Std. Error"], p = co[idx, p_col])
}

run_all <- function(d1_prep, d_prep, regime) {

  t1 <- make_samples(d1_prep, regime)
  t2 <- make_samples(d_prep, regime)

  results <- list()
  add <- function(num, outcome, tp, n, d_val, est, se, p) {
    results[[length(results) + 1]] <<- tibble::tibble(
      analysis = num, outcome = outcome, timepoint = tp,
      n = n, d = d_val, estimate = est, se = se, p = p
    )
  }

  # ---- t1 analyses ----

  # 1. Difference. Meat is the focal conversation.
  s <- t1$all
  m <- lm_lin(change_diff ~ condition_cc, covariates = ~diff_pre, data = s)
  cf <- flip_to_meat(get_coef(m, "condition_cc0.5"))
  dv <- d_from_coef(cf$est, sd_diff_pre)
  add("1", "Belief difference", "t1", nrow(s), dv, cf$est, cf$se, cf$p)

  # 2a. Meat belief. Meat minus Charity.
  s <- t1$meat
  m <- lm_lin(meat_wrong_change ~ condition_cc, covariates = ~meat_wrong_pre, data = s)
  cf <- flip_to_meat(get_coef(m, "condition_cc0.5"))
  dv <- d_from_coef(cf$est, sd_meat_pre)
  add("2a", "Meat belief change", "t1", nrow(s), dv, cf$est, cf$se, cf$p)

  # 2b. Charity belief
  s <- t1$charity
  m <- lm_lin(charity_wrong_change ~ condition_cc, covariates = ~charity_wrong_pre, data = s)
  cf <- get_coef(m, "condition_cc0.5")
  dv <- d_from_coef(cf$est, sd_charity_pre)
  add("2b", "Charity belief change", "t1", nrow(s), dv, cf$est, cf$se, cf$p)

  # 3. Donation
  s <- t1$charity
  m <- lm_lin(charity_donation_cents ~ condition_cc, covariates = ~charity_wrong_pre, data = s)
  cf <- get_coef(m, "condition_cc0.5")
  dv <- d_from_coef(cf$est, sd_donation)
  add("3", "Donation (cents)", "t1", nrow(s), dv, cf$est, cf$se, cf$p)

  # 4. Voucher. Estimate is the log odds, Meat minus Charity. d uses the
  # linear-probability coefficient from the same covariate.
  s <- t1$meat |> mutate(
    meat_wrong_pre_c = as.numeric(scale(meat_wrong_pre, scale = FALSE)),
    voucher_num = as.numeric(voucher_is_veg)
  )
  m <- glm(voucher_num ~ condition_cc * meat_wrong_pre_c, data = s, family = binomial)
  cf <- flip_to_meat(get_coef(m, "^condition_cc$"))
  m_lpm <- lm_lin(voucher_num ~ condition_cc, covariates = ~meat_wrong_pre, data = s)
  cf_lpm <- flip_to_meat(get_coef(m_lpm, "condition_cc0.5"))
  dv <- d_from_coef(cf_lpm$est, sd_voucher)
  add("4", "Voucher (logistic)", "t1", nrow(s), dv, cf$est, cf$se, cf$p)

  # ---- Analysis 5: Reflection DVs (winsorized per preregistration) ----
  # Same samples as the matching belief and behaviour: meat, then charity.
  s5a <- t2$meat |> mutate(
    prev_meat_w    = winsorize_upper(prev_meat),
    prev_meat_t2_w = winsorize_upper(prev_meat_t2),
    change_atemeat = prev_meat_t2_w - prev_meat_w
  )
  s5b <- t2$charity |> mutate(
    prev_donate_w    = winsorize_upper(prev_donate),
    prev_donate_t2_w = winsorize_upper(prev_donate_t2),
    change_donated   = prev_donate_t2_w - prev_donate_w
  )

  m <- lm_lin(change_atemeat ~ condition_cc, covariates = ~prev_meat_w, data = s5a)
  cf <- flip_to_meat(get_coef(m, "condition_cc0.5"))
  dv <- d_from_coef(cf$est, sd_prev_meat)
  add("5a", "Reflection: meat consumption", "t2", nrow(s5a), dv, cf$est, cf$se, cf$p)

  m <- lm_lin(change_donated ~ condition_cc, covariates = ~prev_donate_w, data = s5b)
  cf <- get_coef(m, "condition_cc0.5")
  dv <- d_from_coef(cf$est, sd_prev_donate)
  add("5b", "Reflection: donation", "t2", nrow(s5b), dv, cf$est, cf$se, cf$p)

  # ---- t2 analyses (Analysis 6) ----

  # 6a. Difference. Meat is the focal conversation.
  s <- t2$all
  m <- lm_lin(change_diff_t2 ~ condition_cc, covariates = ~diff_pre, data = s)
  cf <- flip_to_meat(get_coef(m, "condition_cc0.5"))
  dv <- d_from_coef(cf$est, sd_diff_pre)
  add("6a", "Belief difference", "t2", nrow(s), dv, cf$est, cf$se, cf$p)

  # 6b-i. Meat belief. Meat minus Charity.
  s <- t2$meat
  m <- lm_lin(meat_wrong_change_t2 ~ condition_cc, covariates = ~meat_wrong_pre, data = s)
  cf <- flip_to_meat(get_coef(m, "condition_cc0.5"))
  dv <- d_from_coef(cf$est, sd_meat_pre)
  add("6b-i", "Meat belief change", "t2", nrow(s), dv, cf$est, cf$se, cf$p)

  # 6b-ii. Charity belief
  s <- t2$charity
  m <- lm_lin(charity_wrong_change_t2 ~ condition_cc, covariates = ~charity_wrong_pre, data = s)
  cf <- get_coef(m, "condition_cc0.5")
  dv <- d_from_coef(cf$est, sd_charity_pre)
  add("6b-ii", "Charity belief change", "t2", nrow(s), dv, cf$est, cf$se, cf$p)

  # 6c. Donation
  s <- t2$charity
  m <- lm_lin(charity_donation_cents_t2 ~ condition_cc, covariates = ~charity_wrong_pre, data = s)
  cf <- get_coef(m, "condition_cc0.5")
  dv <- d_from_coef(cf$est, sd_donation)
  add("6c", "Donation (cents)", "t2", nrow(s), dv, cf$est, cf$se, cf$p)

  # 6d. Voucher. Same time-1 standardizer as analysis 4. Estimate stays the log odds.
  s <- t2$meat |> mutate(
    meat_wrong_pre_c = as.numeric(scale(meat_wrong_pre, scale = FALSE)),
    voucher_t2_num = as.numeric(voucher_is_veg_t2)
  )
  m <- glm(voucher_t2_num ~ condition_cc * meat_wrong_pre_c, data = s, family = binomial)
  cf <- flip_to_meat(get_coef(m, "^condition_cc$"))
  m_lpm <- lm_lin(voucher_t2_num ~ condition_cc, covariates = ~meat_wrong_pre, data = s)
  cf_lpm <- flip_to_meat(get_coef(m_lpm, "condition_cc0.5"))
  dv <- d_from_coef(cf_lpm$est, sd_voucher)
  add("6d", "Voucher (logistic)", "t2", nrow(s), dv, cf$est, cf$se, cf$p)

  bind_rows(results) |> mutate(regime = regime)
}

# =============================================================================
# RUN
# =============================================================================

d1_prep <- prepare_t1(d1)
d_prep  <- prepare_t2(d)

res_full   <- run_all(d1_prep, d_prep, "none")
res_prereg <- run_all(d1_prep, d_prep, "preregistered")

fmt <- function(df) {
  df |> mutate(p_fmt = fmt_p(p), sig = fmt_sig(p))
}

# =============================================================================
# PRINT RESULTS
# =============================================================================

print_table <- function(df, label) {
  section(label, char = "#")
  df |>
    mutate(d = round(d, 3), estimate = round(estimate, 3), se = round(se, 3)) |>
    select(analysis, outcome, timepoint, n, d, estimate, se, p_fmt, sig) |>
    as.data.frame() |>
    print(row.names = FALSE, right = FALSE)
}

print_table(fmt(res_full),   "1. FULL SAMPLE (primary, reported in paper)")
print_table(fmt(res_prereg), "2. PREREGISTERED EXCLUSION (>75 on the treated statement for that outcome)")

# =============================================================================
# SIDE-BY-SIDE COMPARISON
# =============================================================================

section("SIDE-BY-SIDE COMPARISON", char = "#")

side <- bind_rows(
  fmt(res_full)   |> mutate(regime = "full"),
  fmt(res_prereg) |> mutate(regime = "prereg")
) |>
  mutate(d = round(d, 3)) |>
  select(analysis, outcome, timepoint, regime, n, d, p_fmt, sig) |>
  pivot_wider(
    names_from = regime,
    values_from = c(n, d, p_fmt, sig),
    names_glue = "{regime}_{.value}"
  ) |>
  select(analysis, outcome, timepoint,
         full_n, full_d, full_p_fmt, full_sig,
         prereg_n, prereg_d, prereg_p_fmt, prereg_sig)

print(as.data.frame(side), row.names = FALSE, right = FALSE)

# =============================================================================
# SI TABLE: FULL SAMPLE VS PREREGISTERED EXCLUSION
# =============================================================================
#
# Each estimate is the focal conversation minus the other one. Meat is the
# treatment for the meat belief, the burger choice, and self-reported meat
# consumption. Charity is the treatment for the charity belief, the donation,
# and self-reported giving. Cohen's d divides that coefficient by the frozen
# standardizer. The burger estimate is a log odds ratio; its d uses the
# linear-probability coefficient from the same covariate.

write_pilot_exclusion_table <- function(full, prereg) {
  keep <- c("2a", "2b", "3", "4", "6b-i", "6b-ii", "6c", "6d", "5a", "5b")
  labels <- c(
    "2a"    = "Agreement that eating meat is morally wrong",
    "2b"    = "Agreement that not giving to charity is morally wrong",
    "3"     = "Donation to the Against Malaria Foundation (cents)",
    "4"     = "Choice of a plant-based rather than beef burger (log odds)",
    "6b-i"  = "Agreement that eating meat is morally wrong",
    "6b-ii" = "Agreement that not giving to charity is morally wrong",
    "6c"    = "Donation to the Against Malaria Foundation (cents)",
    "6d"    = "Choice of a plant-based rather than beef burger (log odds)",
    "5a"    = "Times eaten meat in the past 30 days",
    "5b"    = "Dollars given to charity in the past 30 days"
  )

  tex_num <- function(x, digits = 2) {
    rounded <- round(x, digits)
    s <- sprintf(paste0("%.", digits, "f"), abs(rounded))
    ifelse(rounded < 0, paste0("$-$", s), s)
  }
  tex_p <- function(p) {
    ifelse(p < 0.001, "<.001", paste0(".", sprintf("%03d", as.integer(round(p * 1000)))))
  }
  tex_est <- function(est, se, p) {
    e <- tex_num(est, 2)
    e <- ifelse(p < 0.05, paste0("\\textbf{", e, "}"), e)
    paste0(e, " (", tex_num(se, 2), ")")
  }
  tex_p_cell <- function(p) {
    body <- tex_p(p)
    ifelse(p < 0.05, paste0("\\textbf{", body, "}"), body)
  }

  tab <- full |>
    transmute(
      analysis, timepoint,
      n_full = n, est_full = estimate, se_full = se, d_full = d, p_full = p
    ) |>
    left_join(
      prereg |> transmute(
        analysis,
        n_ex = n, est_ex = estimate, se_ex = se, d_ex = d, p_ex = p
      ),
      by = "analysis"
    ) |>
    filter(analysis %in% keep) |>
    mutate(analysis = factor(analysis, levels = keep)) |>
    arrange(analysis)

  tex_d <- function(x) ifelse(is.na(x), "---", tex_num(x, 2))

  row_line <- function(i) {
    paste(
      labels[[as.character(tab$analysis[i])]],
      c(t1 = "Immediate", t2 = "Follow-up")[[tab$timepoint[i]]],
      tab$n_full[i],
      tex_est(tab$est_full[i], tab$se_full[i], tab$p_full[i]),
      tex_d(tab$d_full[i]),
      tex_p_cell(tab$p_full[i]),
      tab$n_ex[i],
      tex_est(tab$est_ex[i], tab$se_ex[i], tab$p_ex[i]),
      tex_d(tab$d_ex[i]),
      tex_p_cell(tab$p_ex[i]),
      sep = " & "
    )
  }

  body <- character()
  prev_tp <- NA_character_
  for (i in seq_len(nrow(tab))) {
    if (!identical(tab$timepoint[i], prev_tp)) {
      header <- if (tab$timepoint[i] == "t1") "Immediate" else "One-month follow-up"
      body <- c(body, sprintf("\\multicolumn{10}{@{}l}{\\textbf{%s}} \\\\", header), "\\addlinespace[2pt]")
      prev_tp <- tab$timepoint[i]
    }
    body <- c(body, paste0(row_line(i), " \\\\"))
  }

  note <- paste(
    "\\textit{Note.} `Estimate' reports the treatment--control average treatment",
    "effect estimate, and $d$ is Cohen's $d$ for that same contrast.",
    "Belief rows are the change in agreement on the 0--100 scale.",
    "Bold estimates and $p$-values have $p < .05$.",
    "$p$-values are two-sided and unadjusted."
  )

  c(
    "% Requires booktabs and pdflscape.",
    "\\begin{landscape}",
    "\\SILandscapeStyle",
    "\\nolinenumbers",
    "\\vspace*{0.5em}",
    "\\noindent\\begin{minipage}{\\linewidth}",
    "  \\centering",
    "  \\captionof{table}{Pilot belief, behavior, and self-report estimates in the full sample and under the preregistered exclusion.}",
    "  \\label{tab:pilot-exclusion}",
    "  \\footnotesize",
    "  \\setlength{\\tabcolsep}{4pt}",
    "  \\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{2.8in}lrrrrrrrr@{}}",
    "  \\toprule",
    "  & & \\multicolumn{4}{c}{Full sample} & \\multicolumn{4}{c}{Preregistered exclusion} \\\\",
    "  \\cmidrule(lr){3-6} \\cmidrule(lr){7-10}",
    "  Outcome & Time & $N$ & Estimate (SE) & $d$ & $p$ & $N$ & Estimate (SE) & $d$ & $p$ \\\\",
    "  \\midrule",
    paste0("  ", body),
    "  \\bottomrule",
    "  \\end{tabular}",
    "  \\vspace{2pt}",
    "  \\begin{minipage}{\\linewidth}",
    paste0("  \\footnotesize", note),
    "  \\end{minipage}",
    "\\end{minipage}",
    "\\SILandscapeFooter",
    "\\end{landscape}"
  )
}

out_dir <- here::here("output", "study_1", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
table_path <- file.path(out_dir, "pilot-full-vs-exclusion.tex")
write_paper_table(write_pilot_exclusion_table(res_full, res_prereg), table_path)
cat("\nWrote ", table_path, "\n", sep = "")

section("DONE", char = "#")
