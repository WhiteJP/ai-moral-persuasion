# =============================================================================
# 11b · Antisocial recontact (lying & political violence)
# =============================================================================
# Separate recontact of the two antisocial arms (study2_d_antisocial.rds).
# Timepoints for the arm's own belief:
#   t0 round-1 pre, t1 round-1 post, t2 post-corrective,
#   t3 recontact before the new conversation, t4 recontact after it.
#
# Sourced by 11_corrective.R. Relies on arm_defs / helpers defined in 11a
# (run 11a first).

# =============================================================================
# 8. ANTISOCIAL RECONTACT: OWN-ARM BELIEF BY TIMEPOINT
# =============================================================================
# Recontact respondents only. t4 exists only for people who were not at the
# top of the slider at t3, so its mean is over a different, lower-starting
# subset. "Last" is each person's latest measure (t4 if they have it, else t3).

d_as <- readRDS(here("data", "study_2", "study2_d_antisocial.rds")) |>
  mutate(
    arm_lab = arm_defs$arm_lab[match(as.character(condition), arm_defs$condition)],
    arm_lab = factor(arm_lab, levels = arm_defs$arm_lab),
    belief_last = if_else(condition == "lying", belief_lying_last, belief_polviol_last)
  )

subsection("Recontact sample")
print(count(d_as, condition, ceiling_skip, last_measure, name = "n"), n = Inf)
n_as_chats <- sum(stringr::str_detect(coalesce(d_as$chathistory1, ""), "assistant"))
n_as_t4_no_chat <- sum(
  !is.na(d_as$cond_belief_t4) &
    !stringr::str_detect(coalesce(d_as$chathistory1, ""), "assistant")
)
cat(sprintf(
  "Saved conversations: %d. t4 belief with no saved conversation: %d.\n",
  n_as_chats, n_as_t4_no_chat
))

section("ANTISOCIAL RECONTACT: OWN-ARM BELIEF, MEAN (SD)")

fmt_mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("---")
  sprintf("%.1f (%.1f)", mean(x), sd(x))
}

as_tp <- c(
  t0 = "cond_belief_t0", t1 = "cond_belief_t1", t2 = "cond_belief_t2",
  t3 = "cond_belief_t3", t4 = "cond_belief_t4", last = "belief_last"
)

as_desc_tbl <- d_as |>
  dplyr::select(arm_lab, all_of(as_tp)) |>
  pivot_longer(-arm_lab, names_to = "timepoint", values_to = "belief") |>
  mutate(timepoint = factor(timepoint, levels = names(as_tp))) |>
  group_by(arm_lab, timepoint) |>
  summarise(
    n = sum(!is.na(belief)),
    mean_sd = fmt_mean_sd(belief),
    .groups = "drop"
  )
print(as_desc_tbl, n = Inf)

cat("\nRecontact respondents by arm and last measure:\n")
print(count(d_as, arm_lab, last_measure), n = Inf)

cat("\nFor comparison, t0 in all round-1 completers of each arm:\n")
d_corr_t0 <- purrr::map_dfr(seq_len(nrow(arm_defs)), function(i) {
  a <- arm_defs[i, ]
  d1 |>
    filter(condition == a$condition) |>
    transmute(arm_lab = a$arm_lab, y0 = .data[[a$pre]])
})
d_corr_t0 |>
  group_by(arm_lab) |>
  summarise(n = sum(!is.na(y0)), t0 = fmt_mean_sd(y0), .groups = "drop") |>
  print(n = Inf)

# -----------------------------------------------------------------------------
# Demographics and recontact lag
# -----------------------------------------------------------------------------
# The recontact surveys did not ask age or gender, so both come from round 1.
# Lag is recontact StartDate minus round-1 StartDate, both in UTC.

d_as_demo <- d_as |>
  dplyr::select(participant_id, arm_lab, StartDate_fu = StartDate) |>
  left_join(
    d1 |> dplyr::select(participant_id, gender, age, StartDate_r1 = StartDate),
    by = "participant_id"
  ) |>
  mutate(lag_days = as.numeric(difftime(StartDate_fu, StartDate_r1, units = "days")))

d_as_demo_both <- bind_rows(
  d_as_demo,
  mutate(d_as_demo, arm_lab = "Both arms")
) |>
  mutate(arm_lab = factor(arm_lab, levels = c(arm_defs$arm_lab, "Both arms")))

section("ANTISOCIAL RECONTACT: DEMOGRAPHICS (from round 1)")

cat("\nGender:\n")
d_as_demo_both |>
  count(arm_lab, gender) |>
  group_by(arm_lab) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  ungroup() |>
  print(n = Inf)

cat("\nAge:\n")
d_as_demo_both |>
  group_by(arm_lab) |>
  summarise(
    n   = sum(!is.na(age)),
    M   = round(mean(age, na.rm = TRUE), 2),
    SD  = round(sd(age, na.rm = TRUE), 2),
    Min = min(age, na.rm = TRUE),
    Max = max(age, na.rm = TRUE),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

section("ANTISOCIAL RECONTACT LAG (START RECONTACT - START ROUND 1)")
cat("\nDecimal days. Recontact respondents matched to a round-1 completer.\n")
n_as_lag_na <- sum(is.na(d_as_demo$lag_days))
if (n_as_lag_na > 0) {
  cat(sprintf("Respondents with no lag (missing a start date): %d\n", n_as_lag_na))
}
d_as_demo_both |>
  group_by(arm_lab) |>
  summarise(
    n      = sum(!is.na(lag_days)),
    min    = round(min(lag_days, na.rm = TRUE), 2),
    Q1     = round(quantile(lag_days, 0.25, na.rm = TRUE), 2),
    median = round(median(lag_days, na.rm = TRUE), 2),
    mean   = round(mean(lag_days, na.rm = TRUE), 2),
    Q3     = round(quantile(lag_days, 0.75, na.rm = TRUE), 2),
    max    = round(max(lag_days, na.rm = TRUE), 2),
    sd     = round(sd(lag_days, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

# -----------------------------------------------------------------------------
# Recontact return by arm: chi-squared vs ITT and vs round-1 completers
# -----------------------------------------------------------------------------
# Same form as 03_attrition.R: condition × (dropped, stayed), among
# lying and political-violence only. Invites went to round-1 completers; the
# ITT test is the broader base for differential return by arm.

as_arms <- arm_defs$condition
d1_itt_as <- readRDS(here("data", "study_2", "study2_d1_itt.rds")) |>
  filter(condition %in% as_arms)
d1_as <- d1 |> filter(condition %in% as_arms)
as_returned_ids <- d_as$participant_id

test_as_return <- function(base, label) {
  counts <- base |>
    mutate(returned = participant_id %in% as_returned_ids) |>
    count(condition, returned) |>
    tidyr::pivot_wider(
      names_from = returned, values_from = n, values_fill = 0L,
      names_glue = "{.value}_{returned}"
    ) |>
    transmute(
      condition,
      n_base = n_FALSE + n_TRUE,
      dropped = n_FALSE,
      stayed = n_TRUE,
      pct_returned = 100 * stayed / n_base
    )
  mat <- counts |>
    dplyr::select(condition, dropped, stayed) |>
    tibble::column_to_rownames("condition") |>
    as.matrix()

  section(label)
  cat("\nReturn matrix (rows = arm, cols = did not return / returned):\n")
  print(mat)
  cat("\nPercent returned:\n")
  print(
    counts |>
      mutate(pct_returned = sprintf("%.1f%%", pct_returned)) |>
      dplyr::select(condition, n_base, dropped, stayed, pct_returned) |>
      as.data.frame(),
    row.names = FALSE
  )
  cat(sprintf(
    "  Overall: %.1f%%  (%d / %d)\n\n",
    100 * sum(counts$stayed) / sum(counts$n_base),
    sum(counts$stayed), sum(counts$n_base)
  ))
  print(chisq.test(mat))
}

test_as_return(
  d1_itt_as,
  "ANTISOCIAL RECONTACT RETURN: ITT (lying / polviol) BY ARM"
)
test_as_return(
  d1_as,
  "ANTISOCIAL RECONTACT RETURN: ROUND-1 COMPLETERS BY ARM"
)

# =============================================================================
# 9. ANTISOCIAL RECONTACT: MEANS AND PRE VS LAST
# =============================================================================
# Own-arm belief at every timepoint, for all round-1 completers of the two
# arms. t0-t2 are round 1; t3-t4 come from the recontact.
#   Full sample:  everyone with a value at that timepoint.
#   Recontact:    only people who took the recontact survey.
#   Pre vs last:  all round-1 completers. Last is the latest measure each
#                 person gave (t4, else t3, else t2, else t1).

section("ANTISOCIAL RECONTACT: MEANS BY TIMEPOINT")

tp_fu_levels <- c("t0", "t1", "t2", "t3", "t4")

d_fu_all <- purrr::map_dfr(seq_len(nrow(arm_defs)), function(i) {
  a <- arm_defs[i, ]
  d1 |>
    filter(condition == a$condition) |>
    transmute(
      participant_id,
      arm_lab = a$arm_lab,
      t0 = .data[[a$pre]],
      t1 = .data[[a$post]],
      t2 = .data[[a$post2]]
    )
}) |>
  left_join(
    d_as |> dplyr::select(participant_id, t3 = cond_belief_t3, t4 = cond_belief_t4),
    by = "participant_id"
  ) |>
  mutate(
    in_recontact = participant_id %in% d_as$participant_id,
    last = coalesce(t4, t3, t2, t1),
    arm_lab = factor(arm_lab, levels = arm_defs$arm_lab)
  )

fu_means <- function(dat) {
  dat |>
    pivot_longer(all_of(tp_fu_levels), names_to = "timepoint", values_to = "belief") |>
    filter(!is.na(belief)) |>
    group_by(arm_lab, timepoint) |>
    reframe(mean_ci(belief)) |>
    mutate(timepoint = factor(timepoint, levels = tp_fu_levels))
}

fu_means_full <- fu_means(d_fu_all)
print(fu_means_full, n = Inf)

fu_means_recontact <- fu_means(d_fu_all |> filter(in_recontact))
print(fu_means_recontact, n = Inf)

# --- Pre (t0) vs last ---------------------------------------------------------

pre_last_analysis <- function(dat, sample_title,
                              last_col = "last", last_label = "Last measure") {
  section(paste0(
    "PRE (t0) VS ", toupper(gsub("\n", " ", last_label)), ": PAIRED t-TEST, ", sample_title
  ))

  d_pl <- dat |>
    mutate(last = .data[[last_col]]) |>
    filter(!is.na(t0), !is.na(last))

  tests <- d_pl |>
    group_by(arm_lab) |>
    group_modify(function(g, key) {
      tt <- t.test(g$last, g$t0, paired = TRUE)
      tibble(
        n = nrow(g),
        pre = mean(g$t0), pre_sd = sd(g$t0),
        last = mean(g$last), last_sd = sd(g$last),
        diff = unname(tt$estimate),
        ci_lower = tt$conf.int[1], ci_upper = tt$conf.int[2],
        t = unname(tt$statistic), df = unname(tt$parameter),
        p = tt$p.value,
        d_z = mean(g$last - g$t0) / sd(g$last - g$t0)
      )
    }) |>
    ungroup()
  print(tests |> mutate(p = fmt_p(p)), n = Inf, width = Inf)

  if (last_col == "last") {
    cat("\nWhere each person's last measure came from:\n")
    d_pl |>
      mutate(last_from = case_when(
        !is.na(t4) ~ "t4", !is.na(t3) ~ "t3", !is.na(t2) ~ "t2", TRUE ~ "t1"
      )) |>
      count(arm_lab, last_from) |>
      print(n = Inf)
  }

  invisible(tests)
}

pre_last_analysis(d_fu_all, "ALL ROUND-1 COMPLETERS")
pre_last_analysis(
  d_fu_all |> filter(in_recontact), "RECONTACT RESPONDENTS"
)
pre_last_analysis(
  d_fu_all |> filter(in_recontact), "RECONTACT RESPONDENTS",
  last_col = "t3", last_label = "Recontact pre (t3)"
)

# =============================================================================
# 10. ANTISOCIAL RECONTACT: RELIABLE CHANGE VS BASELINE
# =============================================================================
# Baseline, 1-SEM, and RCI rules from within_criteria (defined in 11a), using
# the same control-based SEM / RCI. Recontact respondents only. t4 exists only
# for people below the top of the slider at t3, so its n is smaller and not
# comparable to t3.
#   Safety:   share of all recontact respondents back within at that timepoint
#   Efficacy: among those shifted at t1 (same definition as section 6), share
#             back within at that timepoint

d_as_rci <- d_fu_all |>
  filter(in_recontact)

as_rci_tp <- c(t3 = "t3", t4 = "t4")

as_rci_grid <- tidyr::expand_grid(
  arm_lab = arm_defs$arm_lab,
  timepoint = names(as_rci_tp),
  criterion = names(within_criteria)
)

as_rci_tbl <- as_rci_grid |>
  rowwise() |>
  mutate(res = list({
    nz <- noise_tbl[noise_tbl$arm_lab == arm_lab, ]
    within <- within_criteria[[criterion]]
    g <- d_as_rci[d_as_rci$arm_lab == arm_lab, ]
    g <- g[!is.na(g[[as_rci_tp[[timepoint]]]]) & !is.na(g$t1), ]
    y <- g[[as_rci_tp[[timepoint]]]]
    within_post <- within(g$t0, g$t1, nz)
    within_tp <- within(g$t0, y, nz)
    shifted <- g$t1 < g$t0 - nz$rci
    cg <- d_control[d_control$arm_lab == arm_lab, ]
    within_ctrl <- within(cg$y0, cg$y1, nz)
    tibble(
      n = nrow(g),
      within_at_t1 = fmt_pct_ci(sum(within_post), nrow(g)),
      within_at_tp = fmt_pct_ci(sum(within_tp), nrow(g)),
      control = fmt_pct_ci(sum(within_ctrl), length(within_ctrl)),
      n_shifted = sum(shifted),
      shifted_within_at_tp = fmt_pct_ci(sum(shifted & within_tp), sum(shifted))
    )
  })) |>
  tidyr::unnest(res) |>
  ungroup() |>
  mutate(
    timepoint = factor(timepoint, levels = names(as_rci_tp)),
    criterion = factor(criterion, levels = names(within_criteria))
  ) |>
  arrange(arm_lab, timepoint, criterion)

section("ANTISOCIAL RECONTACT: RELIABLE CHANGE VS BASELINE (Wilson 95% CI)")
cat("SEM and RCI from control test-retest (section 6):\n")
print(noise_tbl |> dplyr::select(arm_lab, sem, rci), n = Inf)
cat("\nwithin_at_tp: % of recontact respondents back within at that timepoint (safety).\n")
cat("control: % of control within at post (noise alone).\n")
cat("shifted_within_at_tp: % of those shifted at t1 back within at that timepoint (efficacy).\n\n")
print(as_rci_tbl, n = Inf, width = Inf)

# =============================================================================
# 11. ANTISOCIAL RECONTACT: POLITICAL-VIOLENCE VIGNETTE
# =============================================================================
# Round 1 has no pre-treatment vignette, and the control arm was not
# recontacted. The follow-up contrast is therefore each returner's last
# vignette score against the control arm's time-1 score. Last is t4 when
# they had the further conversation, otherwise t3.
# Higher scores = more endorsement of violence (except punishment, where 1 =
# punish). The within-person panel below z-scores the composite on round-1
# political-violence completers so timepoints share a metric. The contrast
# with control uses the full round-1 completer means and SDs, the same
# standardization as the round-1 composite.

section("ANTISOCIAL RECONTACT: PV VIGNETTE")

pv_panel_levels <- c(
  "Support crowd", "Justification", "Willingness to join",
  "Mean of three", "Preregistered composite"
)

pv_items <- tibble::tribble(
  ~stem,           ~item_lab,
  "support",       "Support crowd",
  "justification", "Justification",
  "participation", "Willingness to join"
)

d_pv_vig <- d_as |>
  filter(condition == "polviol") |>
  tidyr::pivot_longer(
    cols = c(
      pv_support_t1, pv_support_t3, pv_support_t4,
      pv_justification_t1, pv_justification_t3, pv_justification_t4,
      pv_participation_t1, pv_participation_t3, pv_participation_t4
    ),
    names_to = c("stem", "timepoint"),
    names_pattern = "pv_(.+)_(t[134])",
    values_to = "score"
  ) |>
  dplyr::select(participant_id, stem, timepoint, score) |>
  left_join(pv_items, by = "stem") |>
  mutate(item_lab = factor(item_lab, levels = pv_panel_levels))

d_pv_wide <- d_pv_vig |>
  tidyr::pivot_wider(names_from = timepoint, values_from = score) |>
  mutate(last = coalesce(t4, t3))

# Mean of the three continuous items (0-100)
d_pv_mean3 <- d_as |>
  filter(condition == "polviol") |>
  transmute(
    participant_id,
    item_lab = factor("Mean of three", levels = pv_panel_levels),
    t1 = (pv_support_t1 + pv_justification_t1 + pv_participation_t1) / 3,
    t3 = (pv_support_t3 + pv_justification_t3 + pv_participation_t3) / 3,
    t4 = (pv_support_t4 + pv_justification_t4 + pv_participation_t4) / 3
  ) |>
  mutate(last = coalesce(t4, t3))

# Preregistered composite: fixed scaling from round-1 polviol completers
pv_ref <- d1 |> filter(condition == "polviol")
pv_z <- function(x, ref) (x - mean(ref, na.rm = TRUE)) / sd(ref, na.rm = TRUE)
pv_comp_at <- function(support, justification, punishment) {
  (
    pv_z(support, pv_ref$pv_support) +
      pv_z(justification, pv_ref$pv_justification) +
      pv_z(1 - punishment, pv_ref$pv_punishment_rev)
  ) / 3
}

d_pv_zcomp <- d_as |>
  filter(condition == "polviol") |>
  transmute(
    participant_id,
    item_lab = factor("Preregistered composite", levels = pv_panel_levels),
    t1 = pv_comp_at(pv_support_t1, pv_justification_t1, pv_punishment_t1),
    t3 = pv_comp_at(pv_support_t3, pv_justification_t3, pv_punishment_t3),
    t4 = pv_comp_at(pv_support_t4, pv_justification_t4, pv_punishment_t4)
  ) |>
  mutate(last = coalesce(t4, t3))

d_pv_plot <- bind_rows(
  d_pv_wide |> dplyr::select(participant_id, item_lab, t1, t3, t4, last),
  d_pv_mean3,
  d_pv_zcomp
)

tp_pv_levels <- c("t1", "t3", "t4")

pv_means <- d_pv_plot |>
  pivot_longer(all_of(tp_pv_levels), names_to = "timepoint", values_to = "score") |>
  filter(!is.na(score)) |>
  group_by(item_lab, timepoint) |>
  reframe(mean_ci(score)) |>
  mutate(timepoint = factor(timepoint, levels = tp_pv_levels))

print(pv_means, n = Inf)

# --- Last follow-up vs control time 1 ----------------------------------------
# No pretreatment vignette and no recontact control arm.

section("PV VIGNETTE: LAST FOLLOW-UP VS CONTROL TIME 1")
cat("Welch t-test of the last recontact score minus the control arm's time-1 score.\n")
cat("Composite z-scores use full round-1 completer means and SDs (same scale as round 1).\n\n")

d1_pv <- d1 |>
  mutate(pv_punishment_rev = 1 - pv_punishment)
pv_full_center <- list(
  support = mean(d1_pv$pv_support, na.rm = TRUE),
  justification = mean(d1_pv$pv_justification, na.rm = TRUE),
  punishment_rev = mean(d1_pv$pv_punishment_rev, na.rm = TRUE)
)
pv_full_scale <- list(
  support = sd(d1_pv$pv_support, na.rm = TRUE),
  justification = sd(d1_pv$pv_justification, na.rm = TRUE),
  punishment_rev = sd(d1_pv$pv_punishment_rev, na.rm = TRUE)
)
pv_comp_full <- function(support, justification, punishment) {
  (
    (support - pv_full_center$support) / pv_full_scale$support +
      (justification - pv_full_center$justification) / pv_full_scale$justification +
      ((1 - punishment) - pv_full_center$punishment_rev) / pv_full_scale$punishment_rev
  ) / 3
}

d_pv_last <- d_as |>
  filter(condition == "polviol") |>
  transmute(
    support = coalesce(pv_support_t4, pv_support_t3),
    justification = coalesce(pv_justification_t4, pv_justification_t3),
    participation = coalesce(pv_participation_t4, pv_participation_t3),
    punishment = coalesce(pv_punishment_t4, pv_punishment_t3),
    mean3 = (support + justification + participation) / 3,
    composite = pv_comp_full(support, justification, punishment)
  )
d_ctrl_t1 <- d1_pv |>
  filter(condition == "control") |>
  transmute(
    support = pv_support,
    justification = pv_justification,
    participation = pv_participation,
    punishment = pv_punishment,
    mean3 = (pv_support + pv_justification + pv_participation) / 3,
    composite = pv_comp_full(pv_support, pv_justification, pv_punishment)
  )

welch_vs_ctrl <- function(follow, control, lab) {
  follow <- follow[!is.na(follow)]
  control <- control[!is.na(control)]
  tt <- t.test(follow, control)
  tibble(
    item = lab,
    n_follow = length(follow),
    n_control = length(control),
    follow = mean(follow),
    control = mean(control),
    diff = mean(follow) - mean(control),
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2],
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p = tt$p.value
  )
}

pv_ctrl_items <- c(
  participation = "Willingness to join",
  support = "Support crowd",
  justification = "Justification",
  mean3 = "Mean of three",
  composite = "Preregistered composite"
)
pv_ctrl_tbl <- purrr::map_dfr(names(pv_ctrl_items), function(nm) {
  welch_vs_ctrl(d_pv_last[[nm]], d_ctrl_t1[[nm]], pv_ctrl_items[[nm]])
})
print(pv_ctrl_tbl |> mutate(p = fmt_p(p)), n = Inf, width = Inf)

# Punishment is 1 = should be punished. Two-proportion test with continuity correction.
pun_follow <- d_pv_last$punishment[!is.na(d_pv_last$punishment)]
pun_control <- d_ctrl_t1$punishment[!is.na(d_ctrl_t1$punishment)]
pun_prop <- prop.test(
  c(sum(pun_follow), sum(pun_control)),
  c(length(pun_follow), length(pun_control)),
  correct = TRUE
)
pun_ctrl_tbl <- tibble(
  item = "Punishment (should be punished)",
  n_follow = length(pun_follow),
  n_control = length(pun_control),
  pct_follow = 100 * mean(pun_follow),
  pct_control = 100 * mean(pun_control),
  diff_pp = 100 * (mean(pun_follow) - mean(pun_control)),
  ci_lower_pp = 100 * pun_prop$conf.int[1],
  ci_upper_pp = 100 * pun_prop$conf.int[2],
  p = fmt_p(pun_prop$p.value)
)
cat("\nPunishment, two-proportion test (continuity correction), follow-up minus control:\n")
print(pun_ctrl_tbl, n = Inf, width = Inf)

# --- t1 vs recontact pre and vs last -----------------------------------------

pv_t1_contrast <- function(dat, follow_col, follow_label) {
  section(paste0(
    "PV VIGNETTE: ROUND-1 POST (t1) VS ",
    toupper(gsub("\n", " ", follow_label)),
    " (RECONTACT)"
  ))

  d_pl <- dat |>
    mutate(follow = .data[[follow_col]]) |>
    filter(!is.na(t1), !is.na(follow))

  tests <- d_pl |>
    group_by(item_lab) |>
    group_modify(function(g, key) {
      tt <- t.test(g$follow, g$t1, paired = TRUE)
      tibble(
        n = nrow(g),
        t1 = mean(g$t1), follow = mean(g$follow),
        diff = unname(tt$estimate),
        ci_lower = tt$conf.int[1], ci_upper = tt$conf.int[2],
        t = unname(tt$statistic), df = unname(tt$parameter),
        p = tt$p.value,
        d_z = mean(g$follow - g$t1) / sd(g$follow - g$t1)
      )
    }) |>
    ungroup()
  print(tests |> mutate(p = fmt_p(p)), n = Inf, width = Inf)
  invisible(tests)
}

pv_t1_contrast(d_pv_plot, "t3", "recontact pre (t3)")
pv_t1_contrast(d_pv_plot, "last", "last measure")

# Punishment is 1 = should be punished.
section("PV VIGNETTE: PUNISHMENT (1 = punish) VS ROUND-1 POST")
d_pv_pun <- d_as |>
  filter(condition == "polviol") |>
  transmute(
    t1 = pv_punishment_t1,
    t3 = pv_punishment_t3,
    t4 = pv_punishment_t4,
    last = coalesce(pv_punishment_t4, pv_punishment_t3)
  )
pun_row <- function(follow, lab) {
  ok <- !is.na(d_pv_pun$t1) & !is.na(follow)
  tab <- table(
    factor(d_pv_pun$t1[ok], levels = 0:1),
    factor(follow[ok], levels = 0:1)
  )
  mt <- mcnemar.test(tab, correct = TRUE)
  tibble(
    contrast = lab,
    n = sum(ok),
    pct_t1 = 100 * mean(d_pv_pun$t1[ok]),
    pct_follow = 100 * mean(follow[ok]),
    p = mt$p.value
  )
}
pun_tbl <- bind_rows(
  pun_row(d_pv_pun$t3, "t1 vs t3"),
  pun_row(d_pv_pun$last, "t1 vs last")
) |>
  mutate(p = fmt_p(p), across(c(pct_t1, pct_follow), ~ round(., 1)))
print(pun_tbl, n = Inf)
