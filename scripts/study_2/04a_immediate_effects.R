# =============================================================================
# 04a · Immediate effects (t1 beliefs, behaviours, secondary PV)
# =============================================================================
# Pretreatment characterization, belief-change descriptives/figures, and
# preregistered treatment–control models for the four moral-belief outcomes
# and four behavioural outcomes (plus secondary PV items).
#
# Sourced by 04_main_analyses.R. Follow-up lives in 04b_followup_effects.R.
# Spillover matrices: 12_spillover_effects.R.
#
# Preregistered model (each outcome):
#   outcome ~ condition * pre_belief_c + order
# with HC2 robust SEs; control as reference; no multiplicity adjustment.
#
# Cohen's d = model coefficient / a frozen standardizer.
#   Beliefs:     SD of the pretreatment item on this full baseline sample,
#                condition ignored.
#   Behaviours:  control-arm SD of the time-1 outcome, including binary 0/1
#                outcomes. The coefficient and the SD stay on that scale.
#                Follow-up reuses the time-1 SD.
#
# Binary behaviours (LPM-adjusted predicted rates):
#   Odds ratio, risk ratio, NNT (see study2_te_helpers.R).
#
# Estimation / ATE tables: scripts/functions/study2_te_helpers.R
#   options(amp.verbose.models = TRUE) prints full lm_robust summaries.
#
# Figure: pretreatment belief histogram, output/study_2/figures/fig-pre-beliefs.pdf

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(estimatr)
library(here)

source(here("scripts", "functions", "helpers.R"))
source(here("scripts", "functions", "study2_style.R"))
source(here("scripts", "functions", "study2_te_helpers.R"))
source(here("scripts", "functions", "study2_outcomes.R"))

# Wide console so printed tables don't wrap mid-column
options(width = max(getOption("width"), 220L))

d1 <- readRDS(here("data", "study_2", "study2_d1.rds"))

cat("Analysis sample (d1 completers): N =", nrow(d1), "\n")
print(table(d1$condition))

# =============================================================================
# SHARED LABELS / STYLE
# =============================================================================

output_dir <- here("output", "study_2", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Outcomes and confirmatory arms: scripts/functions/study2_outcomes.R
belief_defs <- study2_belief_defs_t1()
primary_treat <- study2_primary_treat
treat_levels <- study2_treat_levels
theme_fig <- theme_study2_fig()

# Printed so a reader sees which columns this script uses.
print(as.data.frame(belief_defs), row.names = FALSE)

# =============================================================================
# 1. PRETREATMENT BELIEFS
# =============================================================================

section("PRETREATMENT BELIEFS: OVERALL QUANTILES")

pre_quantiles <- purrr::map_dfr(seq_len(nrow(belief_defs)), function(i) {
  v <- d1[[belief_defs$pre[i]]]
  qs <- quantile(v, probs = c(0, 0.25, 0.5, 0.75), na.rm = TRUE)
  tibble(
    measure = belief_defs$short[i],
    statement = belief_defs$statement[i],
    n = sum(!is.na(v)),
    min = qs[["0%"]],
    Q1 = qs[["25%"]],
    median = qs[["50%"]],
    mean = mean(v, na.rm = TRUE),
    Q3 = qs[["75%"]]
  )
})

print(
  pre_quantiles |>
    mutate(across(c(min, Q1, median, mean, Q3), ~ round(.x, 2))) |>
    as.data.frame(),
  row.names = FALSE
)

# --- Means by condition + omnibus F ------------------------------------------

section("PRETREATMENT BELIEFS: MEANS BY CONDITION")

pre_by_cond <- purrr::map_dfr(seq_len(nrow(belief_defs)), function(i) {
  pre_var <- belief_defs$pre[i]
  d1 |>
    filter(!is.na(.data[[pre_var]]), !is.na(condition)) |>
    group_by(condition) |>
    summarise(
      n = n(),
      M = mean(.data[[pre_var]], na.rm = TRUE),
      SD = sd(.data[[pre_var]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      measure = belief_defs$short[i],
      cell = sprintf("%.1f (%.1f)", M, SD)
    )
})

pre_means_wide <- pre_by_cond |>
  mutate(condition_lab = condition_labels[as.character(condition)]) |>
  select(measure, condition_lab, cell) |>
  pivot_wider(names_from = condition_lab, values_from = cell)

cat("\nMean (SD) by condition:\n")
print(as.data.frame(pre_means_wide), row.names = FALSE)

omnibus <- purrr::map_dfr(seq_len(nrow(belief_defs)), function(i) {
  pre_var <- belief_defs$pre[i]
  d <- d1 |> filter(!is.na(.data[[pre_var]]), !is.na(condition))
  fit <- aov(as.formula(paste(pre_var, "~ condition")), data = d)
  s <- summary(fit)[[1]]
  tibble(
    measure = belief_defs$short[i],
    F = s[["F value"]][1],
    df1 = s[["Df"]][1],
    df2 = s[["Df"]][2],
    p = s[["Pr(>F)"]][1]
  )
})

section("PRETREATMENT BELIEFS: OMNIBUS TEST")
cat("One-way ANOVA (belief_pre ~ condition):\n")
print(
  omnibus |>
    mutate(
      F = round(F, 3),
      p_fmt = fmt_p(p),
      sig = fmt_sig(p)
    ) |>
    select(measure, F, df1, df2, p_fmt, sig) |>
    as.data.frame(),
  row.names = FALSE
)


# --- 4-panel histogram -------------------------------------------------------

section("PRETREATMENT BELIEFS: HISTOGRAM FIGURE")

pre_titles <- paste0(
  "\u201c", stringr::str_wrap(belief_defs$statement, width = 42), "\u201d"
)
pre_long <- purrr::map_dfr(seq_len(nrow(belief_defs)), function(i) {
  tibble(
    measure = belief_defs$key[i],
    panel_title = pre_titles[i],
    value = d1[[belief_defs$pre[i]]]
  )
}) |>
  mutate(panel_title = factor(panel_title, levels = pre_titles))

# Density is computed within each panel; times the 5-point bin width it is
# the share of that panel's participants, so all panels share one y-axis.
fig_pre_hist <- ggplot(pre_long, aes(x = value)) +
  geom_histogram(aes(y = after_stat(100 * 5 * density)),
                 breaks = seq(0, 100, 5), closed = "left",
                 fill = "#3D405B", color = "white", linewidth = 0.2) +
  facet_wrap(~ panel_title, ncol = 2) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     expand = expansion(add = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Agreement (0–100)", y = "Percent",
       title = "Pretreatment moral beliefs") +
  theme_fig +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 7, lineheight = 1.05),
    panel.spacing.y = unit(28, "pt")
  )

save_plot(
  file.path(output_dir, "fig-pre-belief-hist.png"),
  fig_pre_hist,
  base_width = 7.5, base_height = 5.5,
  dpi = FIGURE_DPI, bg = "white"
)
cat("Saved: output/study_2/figures/fig-pre-belief-hist.png\n")

save_paper_pdf(
  fig_pre_hist + labs(title = NULL), "fig-pre-beliefs.pdf",
  width = FIGURE_WIDTH_DOUBLE, height = 4.7, output_dir = output_dir,
  png = FALSE
)

# =============================================================================
# 2. BELIEF CHANGE
# =============================================================================

section("BELIEF CHANGE: MEANS AND SDs BY CONDITION")

change_by_cond <- purrr::map_dfr(seq_len(nrow(belief_defs)), function(i) {
  ch_var <- belief_defs$change[i]
  d1 |>
    filter(!is.na(.data[[ch_var]]), !is.na(condition)) |>
    group_by(condition) |>
    summarise(
      n = n(),
      M = mean(.data[[ch_var]], na.rm = TRUE),
      SD = sd(.data[[ch_var]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      measure = belief_defs$short[i],
      cell = sprintf("%.2f (%.2f)", M, SD)
    )
})

change_means_wide <- change_by_cond |>
  mutate(condition_lab = condition_labels[as.character(condition)]) |>
  select(measure, condition_lab, cell) |>
  pivot_wider(names_from = condition_lab, values_from = cell)

cat("\nMean (SD) belief change by condition:\n")
print(as.data.frame(change_means_wide), row.names = FALSE)

# =============================================================================
# 3. PREREGISTERED MODELS
# =============================================================================

section("BELIEF CHANGE: PREREGISTERED MODELS")

# Ensure control is reference; order is factor for dummy coding
d1 <- d1 |>
  mutate(
    condition = factor(condition, levels = c("control", "meat", "charity", "polviol", "lying")),
    order = factor(order)
  )

# Main condition coefficients are ATEs at mean pre_belief (centered).
# d = coefficient / SD(pretreatment) on this full baseline sample, condition
# ignored. Follow-up belief models reuse these same standardizers.
sd_belief <- stats::setNames(
  vapply(belief_defs$pre, function(v) study2_d_sd(d1, v, "pre"), numeric(1)),
  belief_defs$key
)
models <- fit_prereg_models(d1, belief_defs, outcome_col = "change")
all_ates <- collect_ates(
  models, belief_defs, treat_levels, primary_treat, condition_labels,
  d_from = "pre", sd_by_key = sd_belief
)

section("BELIEF CHANGE: PRIMARY TREATMENT–CONTROL CONTRASTS")
print_primary_belief_table(all_ates)

cat("\nMoral belief analyses complete.\n")

# =============================================================================
# 4. BEHAVIOURAL OUTCOMES (t1)
# =============================================================================
# Recode from raw Qualtrics items so this script does not depend on re-running
# wrangling (voucher 0/1 is beef/veggie in both display orders; coin 1 = heads).
# Punishment is reversed in the PV composite so higher = more pro-violence.

d1 <- d1 |>
  mutate(
    voucher_is_veg = as.integer(coalesce(voucher_DV_beeffirst, voucher_DV_vegfirst) == 1),
    coin_heads = as.integer(lying_DV == 1),
    pv_punishment_rev = 1 - pv_punishment,
    pv_composite = as.numeric(
      (scale(pv_justification) + scale(pv_support) + scale(pv_punishment_rev)) / 3
    )
  )

behavior_defs <- study2_behavior_defs_t1()
print(as.data.frame(behavior_defs), row.names = FALSE)

# --- Means / SDs -------------------------------------------------------------

section("BEHAVIOUR: MEANS AND SDs BY CONDITION")

beh_by_cond <- purrr::map_dfr(seq_len(nrow(behavior_defs)), function(i) {
  summarise_by_cond(d1, behavior_defs$outcome[i], behavior_defs$type[i]) |>
    mutate(measure = behavior_defs$short[i])
})

beh_means_wide <- beh_by_cond |>
  mutate(condition_lab = condition_labels[as.character(condition)]) |>
  select(measure, condition_lab, cell) |>
  pivot_wider(names_from = condition_lab, values_from = cell)

cat("\nMean (SD) by condition. Binary outcomes in percentage points.\n")
print(as.data.frame(beh_means_wide), row.names = FALSE)

cat("\nCell Ns:\n")
print(
  beh_by_cond |>
    mutate(condition_lab = condition_labels[as.character(condition)]) |>
    select(measure, condition_lab, n) |>
    pivot_wider(names_from = condition_lab, values_from = n) |>
    as.data.frame(),
  row.names = FALSE
)

# --- Preregistered models ----------------------------------------------------

section("BEHAVIOUR: PREREGISTERED MODELS")
cat("outcome ~ condition * pre_belief_c + order  (HC2); matching t0 belief.\n")
cat("Linear probability model for binary outcomes (voucher, coin flip).\n")
cat("OR / RR / NNT use the same LPM-adjusted predicted probabilities.\n")

# No behavior was measured before the conversation, so d uses the control
# arm's time-1 SD of that outcome, including the 0/1 voucher and coin flip.
# Follow-up reuses these time-1 standardizers.
sd_beh <- study2_behavior_d_sds(d1)
beh_models <- fit_prereg_models(d1, behavior_defs, outcome_col = "outcome")
beh_ates <- collect_ates(
  beh_models, behavior_defs, treat_levels, primary_treat, condition_labels,
  d_from = "out", sd_by_key = sd_beh
)

section("BEHAVIOUR: PRIMARY TREATMENT–CONTROL CONTRASTS")
print_primary_behavior_table(
  beh_ates,
  paste(
    "Cohen's d = model coefficient / control-arm SD of the time-1 outcome.",
    "Binary coefficients and SDs are both on the 0/1 scale."
  )
)

# Main text: the donation ATE as a percent of the control-arm mean.
don_ate <- beh_ates$estimate[beh_ates$outcome == "charity" & beh_ates$primary]
don_ctrl <- beh_by_cond$M[
  beh_by_cond$measure == "Charitable donation" &
    as.character(beh_by_cond$condition) == "control"
]
cat(sprintf(
  "\nImmediate donation increase relative to the control mean: %.1f%% (%.2f cents / %.2f cents).\n",
  100 * don_ate / don_ctrl, don_ate, don_ctrl
))

# =============================================================================
# 5. SECONDARY OUTCOMES — POLITICAL VIOLENCE COMPOSITE + ITEMS
# =============================================================================
# Composite: z(justification) + z(support) + z(reversed punishment), averaged.
# Punishment reverse-coded so all items run pro-violence (alpha ≈ .82 vs
# negative if unreversed). Item-level models use the original scales.

# --- 5a. Composite -----------------------------------------------------------

section("SECONDARY: PV COMPOSITE — MEANS AND SDs BY CONDITION")

pv_comp_def <- tibble::tribble(
  ~key,         ~outcome,        ~pre,                 ~short,          ~type,  ~x_lab,
  "polviol",    "pv_composite",  "belief_polviol_pre", "PV composite",  "z",    "Pro-violence composite (z)"
)
# collect_ates() uses primary_treat[key]; keep key = polviol for the composite

comp_by_cond <- summarise_by_cond(d1, "pv_composite", "z") |>
  mutate(
    measure = "PV composite",
    condition_lab = condition_labels[as.character(condition)]
  )

cat("\nMean (SD) PV composite by condition (higher = more pro-violence):\n")
print(
  comp_by_cond |>
    select(measure, condition_lab, cell) |>
    pivot_wider(names_from = condition_lab, values_from = cell) |>
    as.data.frame(),
  row.names = FALSE
)
cat("\nCell Ns:\n")
print(
  comp_by_cond |>
    select(measure, condition_lab, n) |>
    pivot_wider(names_from = condition_lab, values_from = n) |>
    as.data.frame(),
  row.names = FALSE
)

section("SECONDARY: PV COMPOSITE — PREREGISTERED MODEL")
cat("outcome ~ condition * pre_belief_c + order  (HC2); pre = belief_polviol_pre.\n")
cat("Punishment reverse-coded before z-scoring.\n")

# The composite has no pretreatment measure. d uses the control arm's SD
# of the composite. The z-scores themselves are still computed on the full sample.
sd_comp <- c(polviol = study2_d_sd(d1, "pv_composite", "control"))
comp_models <- fit_prereg_models(d1, pv_comp_def, outcome_col = "outcome")
comp_ates <- collect_ates(
  comp_models, pv_comp_def, treat_levels, primary_treat, condition_labels,
  d_from = "out", sd_by_key = sd_comp
)

section("SECONDARY: PV COMPOSITE — PRIMARY CONTRAST")
print_primary_behavior_table(
  comp_ates,
  "Cohen's d = model coefficient / control-arm SD of the composite."
)

# --- 5b. Three constituent items ---------------------------------------------

section("SECONDARY: PV ITEMS — MEANS AND SDs BY CONDITION")

pv_item_defs <- tibble::tribble(
  ~key,            ~outcome,           ~pre,                 ~short,              ~type,     ~x_lab,
  "justification", "pv_justification", "belief_polviol_pre", "PV justification",  "points",  "Actions justified (0–100)",
  "support",       "pv_support",       "belief_polviol_pre", "PV support",        "points",  "Support crowd (0–100)",
  "punishment",    "pv_punishment",    "belief_polviol_pre", "PV punishment",     "binary",  "% endorsing criminal charges"
)
# Own-arm primary contrast is still the PV condition for every item
primary_treat <- c(primary_treat,
                   justification = "polviol",
                   support      = "polviol",
                   punishment   = "polviol")

item_by_cond <- purrr::map_dfr(seq_len(nrow(pv_item_defs)), function(i) {
  summarise_by_cond(d1, pv_item_defs$outcome[i], pv_item_defs$type[i]) |>
    mutate(measure = pv_item_defs$short[i])
})

item_means_wide <- item_by_cond |>
  mutate(condition_lab = condition_labels[as.character(condition)]) |>
  select(measure, condition_lab, cell) |>
  pivot_wider(names_from = condition_lab, values_from = cell)

cat("\nMean (SD) by condition. Punishment in percentage points (Yes = criminal charges).\n")
print(as.data.frame(item_means_wide), row.names = FALSE)

section("SECONDARY: PV ITEMS — PREREGISTERED MODELS")
cat("Same specification as primary behaviours; pre = belief_polviol_pre.\n")
cat("Punishment is the original Yes/No item (not reversed); PV arm expected to decrease it.\n")

# None of these items has a pretreatment measure, so d uses the control
# arm's time-1 SD. Punishment is 0/1; its coefficient is on that scale.
sd_items <- c(
  justification = study2_d_sd(d1, "pv_justification", "control"),
  support = study2_d_sd(d1, "pv_support", "control"),
  punishment = study2_d_sd(d1, "pv_punishment", "control")
)
item_models <- fit_prereg_models(d1, pv_item_defs, outcome_col = "outcome")
item_ates <- collect_ates(
  item_models, pv_item_defs, treat_levels, primary_treat, condition_labels,
  d_from = "out", sd_by_key = sd_items
)

section("SECONDARY: PV ITEMS — PRIMARY TREATMENT–CONTROL CONTRASTS")
print_primary_behavior_table(
  item_ates,
  paste(
    "Cohen's d = model coefficient / control-arm SD of the time-1 item.",
    "The criminal-charges coefficient and SD are both on the 0/1 scale."
  )
)

cat("\nBehavioural analyses complete.\n")
