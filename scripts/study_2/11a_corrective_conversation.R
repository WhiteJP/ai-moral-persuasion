# =============================================================================
# 11a · Corrective conversation (lying & political violence)
# =============================================================================
# Second conversation arguing the opposite of the first, then re-measure
# (post2). Within-person undo / remaining ratios; safety and efficacy vs
# measurement noise. Sourced by 11_corrective.R.
#
# Notation (focal belief, 0–100 "morally wrong"):
#   Y0 = pre, Y1 = post (after persuasion), Y2 = post-corrective
#   Undo = mean(Y1 - Y2) / mean(Y1 - Y0)
#   Remaining = 1 - Undo

library(dplyr)
library(tidyr)
library(stringr)
library(estimatr)
library(here)

source(here("scripts", "functions", "helpers.R"))
options(width = max(getOption("width"), 220L))

d1 <- readRDS(here("data", "study_2", "study2_d1.rds"))

N_BOOT <- 2000

arm_defs <- tibble::tribble(
  ~condition, ~arm_lab,             ~pre,                 ~post,                 ~post2,
  "lying",    "Lying",              "belief_lying_pre",   "belief_lying_post",   "belief_lying_post2",
  "polviol",  "Political Violence", "belief_polviol_pre", "belief_polviol_post", "belief_polviol_post2"
)

wilson_ci <- function(x, n, conf = 0.95) {
  if (n == 0) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  c(centre - half, centre + half)
}

mean_ci <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  tq <- qt(0.975, df = n - 1)
  tibble(n = n, mean = m, ci_lower = m - tq * se, ci_upper = m + tq * se)
}

# =============================================================================
# 1. SAMPLE
# =============================================================================

section("CORRECTIVE CONVERSATION: SAMPLE")

has_chat2 <- !is.na(d1$chat2history1) & str_detect(d1$chat2history1, "assistant")

d_corr <- purrr::map_dfr(seq_len(nrow(arm_defs)), function(i) {
  a <- arm_defs[i, ]
  d1 |>
    mutate(has_chat2 = has_chat2) |>
    filter(condition == a$condition) |>
    transmute(
      participantId,
      condition = a$condition,
      arm_lab = a$arm_lab,
      has_chat2,
      y0 = .data[[a$pre]],
      y1 = .data[[a$post]],
      y2 = .data[[a$post2]]
    )
})

sample_flow <- d_corr |>
  group_by(arm_lab) |>
  summarise(
    completers = n(),
    with_chat2 = sum(has_chat2),
    with_all_three = sum(has_chat2 & !is.na(y0) & !is.na(y1) & !is.na(y2)),
    .groups = "drop"
  )
print(sample_flow, n = Inf)

d_corr <- d_corr |>
  filter(has_chat2, !is.na(y0), !is.na(y1), !is.na(y2)) |>
  mutate(
    persuasion = y1 - y0,
    corrective = y2 - y1,
    net        = y2 - y0
  )

# =============================================================================
# 2. PAIRED MEAN CHANGES
# =============================================================================

section("PAIRED MEAN CHANGES (intercept-only lm_robust, HC2)")

paired_change <- function(x) {
  fit <- lm_robust(x ~ 1, se_type = "HC2")
  cf <- summary(fit)$coefficients
  tibble(
    est = cf[1, "Estimate"], se = cf[1, "Std. Error"],
    ci_lower = cf[1, "CI Lower"], ci_upper = cf[1, "CI Upper"],
    p = cf[1, "Pr(>|t|)"]
  )
}

means_tbl <- d_corr |>
  group_by(arm_lab) |>
  summarise(
    n = n(),
    pre = mean(y0), post = mean(y1), post_corrective = mean(y2),
    .groups = "drop"
  )
print(means_tbl, n = Inf)

change_tbl <- d_corr |>
  pivot_longer(c(persuasion, corrective, net), names_to = "change", values_to = "value") |>
  mutate(change = factor(change, levels = c("persuasion", "corrective", "net"),
                         labels = c("Persuasion (post - pre)",
                                    "Corrective (post2 - post)",
                                    "Net (post2 - pre)"))) |>
  group_by(arm_lab, change) |>
  reframe(paired_change(value)) |>
  mutate(p = fmt_p(p))
cat("\n")
print(change_tbl, n = Inf)

# =============================================================================
# 3. SHARE OF THE SHIFT UNDONE (ratio of means, bootstrap CI)
# =============================================================================

section(sprintf("SHARE OF PERSUASION SHIFT UNDONE (percentile bootstrap, B = %d)", N_BOOT))

undo_ratio <- function(y0, y1, y2) mean(y1 - y2) / mean(y1 - y0)

set.seed(20261005)
undo_tbl <- d_corr |>
  group_by(arm_lab) |>
  group_modify(function(g, key) {
    est <- undo_ratio(g$y0, g$y1, g$y2)
    n <- nrow(g)
    boots <- replicate(N_BOOT, {
      idx <- sample.int(n, n, replace = TRUE)
      undo_ratio(g$y0[idx], g$y1[idx], g$y2[idx])
    })
    ci <- quantile(boots, c(0.025, 0.975), names = FALSE)
    tibble(
      n = n,
      undo = est, undo_ci_lower = ci[1], undo_ci_upper = ci[2],
      remaining = 1 - est, remaining_ci_lower = 1 - ci[2], remaining_ci_upper = 1 - ci[1]
    )
  }) |>
  ungroup()

undo_tbl |>
  mutate(
    undo = sprintf("%.1f%% [%.1f, %.1f]", 100 * undo, 100 * undo_ci_lower, 100 * undo_ci_upper),
    remaining = sprintf("%.1f%% [%.1f, %.1f]", 100 * remaining,
                        100 * remaining_ci_lower, 100 * remaining_ci_upper)
  ) |>
  dplyr::select(arm_lab, n, undo, remaining) |>
  print(n = Inf)

# Control pre -> post, used for the test-retest noise estimate below.
d_control <- purrr::map_dfr(seq_len(nrow(arm_defs)), function(i) {
  a <- arm_defs[i, ]
  d1 |>
    filter(condition == "control") |>
    transmute(
      arm_lab = a$arm_lab,
      y0 = .data[[a$pre]],
      y1 = .data[[a$post]]
    ) |>
    filter(!is.na(y0), !is.na(y1))
})

# =============================================================================
# 6. SAFETY AND EFFICACY: BACK WITHIN THE RCI
# =============================================================================
# Noise per item from control's pre -> post test-retest (no persuasion):
#   SEM     = SD(pre) * sqrt(1 - r_tt)
#   SD_diff = sqrt(2) * SEM                 (noise SD of a difference score)
#   RCI     = 1.96 * SD_diff                (Jacobson & Truax reliable change)
#
# Back within the RCI (score Y vs baseline Y0), one-sided since only movement
# toward "less wrong" counts as harm:
#   Y >= Y0 - RCI
#
#   Safety:   share of all treated back within the RCI at post2, vs the share
#             of control within the RCI at post (noise alone)
#   Efficacy: among those who dropped by more than the RCI after the first
#             chat, share back within the RCI at post2
#
# within_criteria also keeps the baseline and 1-SEM rules for the recontact
# script (11b), which reuses this list.

section("MEASUREMENT NOISE: control test-retest (pre -> post)")

noise_tbl <- d_control |>
  group_by(arm_lab) |>
  summarise(
    n = n(),
    r_tt = cor(y0, y1),
    sd_pre = sd(y0),
    sd_change_obs = sd(y1 - y0),
    pct_no_change = 100 * mean(y1 == y0),
    .groups = "drop"
  ) |>
  mutate(
    sem = sd_pre * sqrt(1 - r_tt),
    sd_diff = sqrt(2) * sem,
    rci = 1.96 * sd_diff
  )
print(noise_tbl, n = Inf, width = Inf)

within_criteria <- list(
  "At or above baseline" = function(y0, y, nz) y >= y0,
  "Within 1 SEM" = function(y0, y, nz) y >= y0 - nz$sem,
  "Within RCI" = function(y0, y, nz) y >= y0 - nz$rci
)

fmt_pct_ci <- function(x, n) {
  ci <- 100 * wilson_ci(x, n)
  sprintf("%.1f%% [%.1f, %.1f]", 100 * x / n, ci[1], ci[2])
}

section("WITHIN THE RCI: safety and efficacy (Wilson 95% CI)")
cat("Back within the RCI: belief >= pretreatment - RCI.\n")
cat("Safety: % of all treated within the RCI at the end of the study,\n")
cat("versus % of control within the RCI at post (noise alone).\n")
cat("Gap: control % minus end-of-study % (percentage points).\n")
cat("Efficacy: among those shifted after the first chat (post < pre - RCI),\n")
cat("% back within the RCI at the end of the study.\n\n")

rci_tbl <- purrr::map_dfr(arm_defs$arm_lab, function(lab) {
  g <- d_corr[d_corr$arm_lab == lab, ]
  cg <- d_control[d_control$arm_lab == lab, ]
  nz <- noise_tbl[noise_tbl$arm_lab == lab, ]
  within <- within_criteria[["Within RCI"]]
  within_post <- within(g$y0, g$y1, nz)
  within_end <- within(g$y0, g$y2, nz)
  within_ctrl <- within(cg$y0, cg$y1, nz)
  shifted <- g$y1 < g$y0 - nz$rci
  n <- length(within_end)
  tibble(
    arm_lab = lab,
    n = n,
    rci = nz$rci,
    within_after_first = fmt_pct_ci(sum(within_post), n),
    within_at_end = fmt_pct_ci(sum(within_end), n),
    control = fmt_pct_ci(sum(within_ctrl), length(within_ctrl)),
    gap_pp = round(100 * (mean(within_ctrl) - mean(within_end)), 1),
    n_shifted = sum(shifted),
    shifted_within_at_end = fmt_pct_ci(sum(shifted & within_end), sum(shifted))
  )
})
print(rci_tbl, n = Inf, width = Inf)
