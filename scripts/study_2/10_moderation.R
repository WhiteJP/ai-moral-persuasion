# =============================================================================
# 10 · Treatment-effect moderation
# =============================================================================
# Section 1 — Exploratory linear moderation (preregistered Q8): models, tables,
# and BH-FDR within moderator family (t1 and t2).
#
# Section 2 — Supplement table: Holm-adjusted own-arm interaction p-values
# (within outcome, across 14 moderators), written to output/study_2/tables/.

library(dplyr)
library(tidyr)
library(purrr)
library(estimatr)
library(here)

source(here("scripts", "functions", "helpers.R"))
options(width = max(getOption("width"), 220L))

# =============================================================================
# SECTION 1 · EXPLORATORY MODERATION ANALYSES
# =============================================================================

# Exploratory linear moderation, as preregistered (Q8):
#   outcome ~ condition * moderator + condition * pre_belief_c + order
#
# Moderators: demographics, AOT, CRT, political measures, AI trust, and
# pretreatment moral beliefs. Outcomes: the 8 confirmatory t1 DVs (4 belief
# change, 4 behaviours). HC2 robust SEs; control as reference; order dummies.
#
# Matching pretreatment belief is already the covariate in the main model.
# Those fits use outcome ~ condition * pre_belief_c + order (no duplicate
# interaction). Continuous / binary moderators are mean-centered so condition
# main effects remain ATEs at the mean of the moderator; the interaction
# coefficient itself is invariant to centering.
#
# Tests are exploratory. Raw two-sided p-values are reported, plus BH-FDR
# within each moderator family on the own-arm interactions (outcomes ×
# moderators in that family). The same models are fit at t2 for meat and
# charity beliefs (change from pretreatment) and behaviours (voucher, donation).
# t2 FDR is computed within the t2 family only.
d1 <- readRDS(here("data", "study_2", "study2_d1.rds"))

d1 <- d1 |>
  mutate(
    condition = factor(
      condition,
      levels = c("control", "meat", "charity", "polviol", "lying")
    ),
    order = factor(order),
    voucher_is_veg = as.integer(
      dplyr::coalesce(voucher_DV_beeffirst, voucher_DV_vegfirst) == 1
    ),
    coin_heads = as.integer(lying_DV == 1),
    # Other n = 6 is too sparse for condition × gender; drop from that model
    gender_mw = factor(gender_recoded, levels = c("Man", "Woman")),
    crt_num = as.numeric(crt_batball_cor)
  )

cat("Analysis sample (d1 completers): N =", nrow(d1), "\n")
print(table(d1$condition))

# =============================================================================
# SHARED LABELS / HELPERS
# =============================================================================

fmt_cell <- function(est, p) {
  stars <- fmt_sig(p)
  base <- sprintf("%.3f", est)
  ifelse(
    !is.na(p) & p < 0.05,
    paste0("**", base, "** ", stars),
    paste0(base, stars)
  )
}

condition_labels <- c(
  control = "Control",
  meat    = "Meat",
  charity = "Charity",
  polviol = "Political Violence",
  lying   = "Lying"
)
treat_levels <- c("meat", "charity", "polviol", "lying")

# 8 confirmatory t1 outcomes (own-arm treatment is `treat`)
outcome_defs <- tibble::tribble(
  ~key,          ~outcome,               ~pre,                  ~short,               ~kind,      ~treat,    ~y_lab,                      ~as_pct,
  "meat_bel",    "belief_meat_change",   "belief_meat_pre",     "Meat belief",        "belief",   "meat",    "Belief change",              FALSE,
  "charity_bel", "belief_charity_change","belief_charity_pre",  "Charity belief",     "belief",   "charity", "Belief change",              FALSE,
  "lying_bel",   "belief_lying_change",  "belief_lying_pre",    "Lying belief",       "belief",   "lying",   "Belief change",              FALSE,
  "polviol_bel", "belief_polviol_change","belief_polviol_pre",  "PV belief",          "belief",   "polviol", "Belief change",              FALSE,
  "meat_beh",    "voucher_is_veg",       "belief_meat_pre",     "Dietary choice",     "behavior", "meat",    "% choosing veg voucher",     TRUE,
  "charity_beh", "donation_cents",       "belief_charity_pre",  "Charitable donation","behavior", "charity", "Donation to AMF (cents)",    FALSE,
  "lying_beh",   "coin_heads",           "belief_lying_pre",    "Coin flip (heads)",  "behavior", "lying",   "% reporting heads",          TRUE,
  "polviol_beh", "pv_participation",     "belief_polviol_pre",  "PV participation",   "behavior", "polviol", "Would join crowd (0–100)",   FALSE
)

# Prereg moderator list. Education and income are the survey numeric codes
# (1–6 and 1–17). Political: continuous ideology + 7-point Dem–Rep party ID.
mod_defs <- tibble::tribble(
  ~var,               ~label,                      ~family,          ~type,
  "age",              "Age",                       "Demographics",   "numeric",
  "gender_mw",        "Gender (Man/Woman)",        "Demographics",   "factor",
  "education",        "Education (1–6)",           "Demographics",   "numeric",
  "income",           "Income (1–17)",             "Demographics",   "numeric",
  "race_white",       "White vs non-White",        "Demographics",   "binary",
  "AOT_mean",         "AOT (mean)",                "AOT",            "numeric",
  "crt_num",          "CRT correct",               "CRT",            "binary",
  "pol_orientation",  "Political ideology (1–7)",  "Political",      "numeric",
  "dem_rep_num",      "Party ID (Dem–Rep 1–7)",    "Political",      "numeric",
  "trust_ai_pre",     "AI trust (pre)",            "AI trust",       "numeric",
  "belief_meat_pre",  "Belief: meat (pre)",        "Moral beliefs",  "numeric",
  "belief_charity_pre","Belief: charity (pre)",    "Moral beliefs",  "numeric",
  "belief_lying_pre", "Belief: lying (pre)",       "Moral beliefs",  "numeric",
  "belief_polviol_pre","Belief: pol. violence (pre)","Moral beliefs","numeric"
)

# Wald chi-square for a set of coefficients (robust vcov)
wald_terms <- function(model, terms) {
  co <- coef(model)
  V  <- vcov(model)
  terms <- terms[terms %in% names(co) & !is.na(co[terms])]
  if (length(terms) == 0L) {
    return(list(stat = NA_real_, df = 0L, p = NA_real_, terms = character(0)))
  }
  b <- co[terms]
  Vsub <- V[terms, terms, drop = FALSE]
  stat <- tryCatch(
    as.numeric(t(b) %*% solve(Vsub) %*% b),
    error = function(e) NA_real_
  )
  df <- length(b)
  p <- if (is.na(stat)) NA_real_ else pchisq(stat, df = df, lower.tail = FALSE)
  list(stat = stat, df = df, p = p, terms = terms)
}

parse_int_term <- function(term) {
  m <- regexec("^condition([^:]+):moderator(.*)$", term)
  g <- regmatches(term, m)[[1]]
  if (length(g) < 3L) {
    return(list(treat = NA_character_, level = NA_character_))
  }
  list(
    treat = g[2],
    level = if (nzchar(g[3])) g[3] else NA_character_
  )
}

coef_row <- function(model, term) {
  sm <- summary(model)$coefficients
  if (!term %in% rownames(sm)) {
    return(tibble(
      term = term, estimate = NA_real_, se = NA_real_,
      t = NA_real_, p = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_
    ))
  }
  tibble(
    term     = term,
    estimate = sm[term, "Estimate"],
    se       = sm[term, "Std. Error"],
    t        = sm[term, "t value"],
    p        = sm[term, "Pr(>|t|)"],
    ci_lo    = sm[term, "CI Lower"],
    ci_hi    = sm[term, "CI Upper"]
  )
}

# Fit one prereg moderation model. Matching pre-belief uses the main
# specification (condition * pre_belief_c + order).
fit_moderation <- function(data, outcome_var, pre_var, mod_var, mod_type) {
  d <- data |>
    filter(
      !is.na(.data[[outcome_var]]), !is.na(.data[[pre_var]]),
      !is.na(.data[[mod_var]]), !is.na(condition), !is.na(order)
    ) |>
    mutate(
      outcome = as.numeric(.data[[outcome_var]]),
      pre_belief_c = .data[[pre_var]] - mean(.data[[pre_var]], na.rm = TRUE)
    )

  matching <- identical(mod_var, pre_var)
  is_factor <- identical(mod_type, "factor")

  if (is_factor) {
    d <- d |> mutate(moderator = droplevels(factor(.data[[mod_var]])))
    mod_mean_raw <- NA_real_
    mod_sd_raw <- NA_real_
  } else {
    d <- d |> mutate(moderator_raw = as.numeric(.data[[mod_var]]))
    mod_mean_raw <- mean(d$moderator_raw, na.rm = TRUE)
    mod_sd_raw <- sd(d$moderator_raw, na.rm = TRUE)
    d <- d |> mutate(moderator = moderator_raw - mod_mean_raw)
  }

  if (matching) {
    # moderator is the centered matching pre-belief; do not duplicate
    m <- lm_robust(
      outcome ~ condition * moderator + order,
      data = d, se_type = "HC2"
    )
  } else {
    m <- lm_robust(
      outcome ~ condition * moderator + condition * pre_belief_c + order,
      data = d, se_type = "HC2"
    )
  }

  int_terms <- grep("^condition[^:]+:moderator", names(coef(m)), value = TRUE)
  joint <- wald_terms(m, int_terms)

  int_tbl <- purrr::map_dfr(int_terms, function(tm) {
    parsed <- parse_int_term(tm)
    coef_row(m, tm) |>
      mutate(
        treat = parsed$treat,
        level = parsed$level
      )
  })

  list(
    model = m, data = d, n = nrow(d), matching = matching,
    mod_mean_raw = mod_mean_raw, mod_sd_raw = mod_sd_raw,
    int_terms = int_terms, joint = joint, int_tbl = int_tbl
  )
}

ate_at_mod <- function(model, treat, m_c) {
  b_term <- paste0("condition", treat)
  i_term <- paste0("condition", treat, ":moderator")
  co <- coef(model)
  V  <- vcov(model)
  if (!all(c(b_term, i_term) %in% names(co)) ||
      any(is.na(co[c(b_term, i_term)]))) {
    return(tibble(m_c = m_c, ate = NA_real_, se = NA_real_, p = NA_real_))
  }
  b1 <- unname(co[b_term])
  b2 <- unname(co[i_term])
  ate <- b1 + b2 * m_c
  var <- V[b_term, b_term] + m_c^2 * V[i_term, i_term] +
    2 * m_c * V[b_term, i_term]
  se <- sqrt(max(var, 0))
  z <- ate / se
  tibble(
    m_c = m_c, ate = ate, se = se,
    p = 2 * pnorm(-abs(z))
  )
}

# =============================================================================
# FIT ALL MODELS
# =============================================================================

run_moderation_wave <- function(data, outcome_defs, wave_label) {
section(paste0("PREREG MODERATION: SETUP — ", wave_label))
cat("Model: outcome ~ condition * moderator + condition * pre_belief_c + order\n")
cat("HC2 robust SEs; control as reference. Matching pre-belief uses the main model.\n")
cat(sprintf(
  "Gender: Man vs Woman (Other / prefer-not-to-say dropped; n_Other = %d).\n",
  sum(data$gender_recoded == "Other", na.rm = TRUE)
))
cat("Education 1–6 and income 1–17 as numeric. Ideology = 1–7 continuous;\n")
cat("party ID = 7-point Dem–Rep. CRT and race treated as 0/1 then centered.\n")
n_models <- nrow(outcome_defs) * nrow(mod_defs)
cat(sprintf("%d outcomes × %d moderators = %d models.\n",
            nrow(outcome_defs), nrow(mod_defs), n_models))
cat("Fitting...\n")

fits <- list()
int_rows <- list()
joint_rows <- list()
idx <- 0L

for (i in seq_len(nrow(mod_defs))) {
  mdef <- mod_defs[i, ]
  for (j in seq_len(nrow(outcome_defs))) {
    odef <- outcome_defs[j, ]
    idx <- idx + 1L
    key <- paste(mdef$var, odef$key, sep = "__")

    fit <- fit_moderation(
      data,
      outcome_var = odef$outcome,
      pre_var     = odef$pre,
      mod_var     = mdef$var,
      mod_type    = mdef$type
    )
    fits[[key]] <- fit

    own_terms <- grep(
      paste0("^condition", odef$treat, ":moderator"),
      fit$int_terms, value = TRUE
    )
    own_joint <- wald_terms(fit$model, own_terms)

    joint_rows[[key]] <- tibble(
      mod_var    = mdef$var,
      mod_label  = mdef$label,
      family     = mdef$family,
      mod_type   = mdef$type,
      outcome    = odef$key,
      outcome_lab = odef$short,
      kind       = odef$kind,
      treat_own  = odef$treat,
      n          = fit$n,
      matching   = fit$matching,
      joint_stat = fit$joint$stat,
      joint_df   = fit$joint$df,
      joint_p    = fit$joint$p,
      own_stat   = own_joint$stat,
      own_df     = own_joint$df,
      own_p      = own_joint$p
    )

    if (nrow(fit$int_tbl) > 0) {
      int_rows[[key]] <- fit$int_tbl |>
        mutate(
          mod_var     = mdef$var,
          mod_label   = mdef$label,
          family      = mdef$family,
          mod_type    = mdef$type,
          outcome     = odef$key,
          outcome_lab = odef$short,
          kind        = odef$kind,
          treat_own   = odef$treat,
          primary     = treat == odef$treat,
          n           = fit$n,
          matching    = fit$matching,
          treat_lab   = unname(condition_labels[treat])
        )
    }
  }
}
cat(sprintf("Fitted %d models.\n", idx))

joints <- bind_rows(joint_rows)
ints   <- bind_rows(int_rows)

# BH-FDR within family on own-arm tests (one p per outcome × moderator)
joints <- joints |>
  group_by(family) |>
  mutate(own_p_bh = p.adjust(own_p, method = "BH")) |>
  ungroup()

# =============================================================================
# OWN-ARM INTERACTIONS (PRIMARY)
# =============================================================================

section(paste0(wave_label, " — OWN-ARM CONDITION × MODERATOR"))
cat("Primary estimand: interaction of own-arm treatment vs control with the moderator.\n")
cat("Numeric/binary: coefficient is the change in ATE per 1-unit (centered) increase.\n")
cat("Factors: Wald test of all own-arm × moderator-level terms; level-specific coeffs below.\n")
cat(sprintf(
  "BH-FDR is within moderator family (across the %d outcomes × moderators in that family), for this wave only.\n",
  nrow(outcome_defs)
))

print_own_numeric <- function(dat) {
  dat |>
    transmute(
      moderator = mod_label,
      outcome   = outcome_lab,
      n,
      matching  = ifelse(matching, "yes", ""),
      est       = round(estimate, 3),
      SE        = round(se, 3),
      t         = round(t, 3),
      p         = fmt_p(p),
      sig       = fmt_sig(p),
      FDR       = fmt_p(own_p_bh),
      CI        = sprintf("[%.3f, %.3f]", ci_lo, ci_hi)
    ) |>
    as.data.frame()
}

print_own_factor <- function(jdat) {
  jdat |>
    transmute(
      moderator = mod_label,
      outcome   = outcome_lab,
      n,
      W         = round(own_stat, 2),
      df        = own_df,
      p         = fmt_p(own_p),
      sig       = fmt_sig(own_p),
      FDR       = fmt_p(own_p_bh)
    ) |>
    as.data.frame()
}

own_num <- ints |>
  filter(primary, mod_type != "factor", is.na(level)) |>
  left_join(
    joints |> select(mod_var, outcome, own_p_bh),
    by = c("mod_var", "outcome")
  )

own_fac <- joints |> filter(mod_type == "factor")

for (fam in unique(mod_defs$family)) {
  cat("\n---", fam, "---\n")
  num <- own_num |> filter(family == fam)
  fac <- own_fac |> filter(family == fam)
  if (nrow(num) > 0) {
    print(print_own_numeric(num), row.names = FALSE)
  }
  if (nrow(fac) > 0) {
    cat("\nFactor moderators (own-arm joint Wald):\n")
    print(print_own_factor(fac), row.names = FALSE)
  }
}

# Wide own-arm snapshot (numeric/binary estimates; factor joint p)
section(paste0(wave_label, " — OWN-ARM SNAPSHOT (WIDE)"))
cat("Cells: interaction estimate (numeric/binary) or joint Wald p (factors).\n")
cat("Bold + * = uncorrected p < .05. Matching pre-belief marked in the moderator label.\n\n")

snap_num <- own_num |>
  mutate(cell = fmt_cell(estimate, p)) |>
  select(mod_label, outcome_lab, cell)

snap_fac <- own_fac |>
  mutate(cell = paste0("p=", fmt_p(own_p), fmt_sig(own_p))) |>
  transmute(mod_label, outcome_lab, cell)

snap <- bind_rows(snap_num, snap_fac) |>
  mutate(
    outcome_lab = factor(outcome_lab, levels = outcome_defs$short),
    mod_label = factor(mod_label, levels = mod_defs$label)
  ) |>
  arrange(mod_label, outcome_lab) |>
  pivot_wider(names_from = outcome_lab, values_from = cell) |>
  rename(Moderator = mod_label)

print(as.data.frame(snap), row.names = FALSE)

# =============================================================================
# JOINT TESTS (ANY TREATMENT × MODERATOR)
# =============================================================================

section(paste0(wave_label, " — JOINT WALD: ALL CONDITION × MODERATOR TERMS"))
cat("Tests whether treatment effects (any arm vs control) vary with the moderator.\n")

print_joint <- function(dat) {
  dat |>
    transmute(
      moderator = mod_label,
      outcome   = outcome_lab,
      n,
      matching  = ifelse(matching, "yes", ""),
      W         = round(joint_stat, 2),
      df        = joint_df,
      p         = fmt_p(joint_p),
      sig       = fmt_sig(joint_p)
    ) |>
    as.data.frame()
}

for (fam in unique(mod_defs$family)) {
  cat("\n---", fam, "---\n")
  print(print_joint(joints |> filter(family == fam)), row.names = FALSE)
}

# =============================================================================
# ALL TREATMENT × MODERATOR COEFFICIENTS
# =============================================================================

section(paste0(wave_label, " — ALL TREATMENT × MODERATOR COEFFICIENTS"))
cat("Each cell is the condition × moderator interaction vs control.\n")
cat("For factors, one row per treatment × dummy level (vs reference).\n")

print_int_wide <- function(dat) {
  dat |>
    mutate(cell = fmt_cell(estimate, p)) |>
    select(outcome_lab, treat_lab, cell) |>
    mutate(outcome_lab = factor(outcome_lab, levels = outcome_defs$short)) |>
    arrange(outcome_lab) |>
    pivot_wider(names_from = treat_lab, values_from = cell) |>
    rename(Outcome = outcome_lab) |>
    select(Outcome, any_of(unname(condition_labels))) |>
    as.data.frame()
}

print_int_long <- function(dat, mod_var) {
  lev <- levels(data[[mod_var]])
  dat |>
    transmute(
      outcome = factor(outcome_lab, levels = outcome_defs$short),
      treat   = factor(treat_lab, levels = unname(condition_labels)),
      level   = factor(level, levels = lev),
      est = round(estimate, 3),
      SE  = round(se, 3),
      t   = round(t, 3),
      p   = fmt_p(p),
      sig = fmt_sig(p)
    ) |>
    arrange(outcome, treat, level) |>
    as.data.frame()
}

for (i in seq_len(nrow(mod_defs))) {
  mdef <- mod_defs[i, ]
  cat(sprintf("\n------ %s (%s) ------\n", mdef$label, mdef$family))
  dat <- ints |> filter(mod_var == mdef$var)
  if (mdef$type == "factor") {
    cat("Reference level:", levels(d1[[mdef$var]])[1], "\n")
    print(print_int_long(dat, mdef$var), row.names = FALSE)
  } else {
    print(print_int_wide(dat), row.names = FALSE)
  }
}

# Factor own-arm level detail
section(paste0(wave_label, " — FACTOR MODERATORS: OWN-ARM LEVEL COEFFICIENTS"))

fac_own <- ints |>
  filter(primary, mod_type == "factor") |>
  left_join(
    joints |> select(mod_var, outcome, own_p, own_p_bh, own_stat, own_df),
    by = c("mod_var", "outcome")
  )

print(
  fac_own |>
    transmute(
      moderator = mod_label,
      outcome   = outcome_lab,
      level,
      n,
      est = round(estimate, 3),
      SE  = round(se, 3),
      t   = round(t, 3),
      p   = fmt_p(p),
      sig = fmt_sig(p),
      joint_p = fmt_p(own_p),
      FDR = fmt_p(own_p_bh)
    ) |>
    as.data.frame(),
  row.names = FALSE
)

# =============================================================================
# SIGNIFICANT OWN-ARM HITS + SIMPLE SLOPES
# =============================================================================

section(paste0(wave_label, " — OWN-ARM HITS (uncorrected p < .05)"))

hits_num <- own_num |> filter(p < 0.05)
hits_fac <- own_fac |> filter(own_p < 0.05)

if (nrow(hits_num) == 0 && nrow(hits_fac) == 0) {
  cat("No own-arm condition × moderator terms with uncorrected p < .05.\n")
} else {
  if (nrow(hits_num) > 0) {
    cat("\nNumeric / binary:\n")
    print(print_own_numeric(hits_num), row.names = FALSE)
  }
  if (nrow(hits_fac) > 0) {
    cat("\nFactor (joint):\n")
    print(print_own_factor(hits_fac), row.names = FALSE)
  }
}

bh_hits <- joints |> filter(own_p_bh < 0.05)
section(paste0(wave_label, " — OWN-ARM HITS SURVIVING BH-FDR < .05 (within family)"))
if (nrow(bh_hits) == 0) {
  cat("None.\n")
} else {
  print(
    bh_hits |>
      transmute(
        family, moderator = mod_label, outcome = outcome_lab,
        p = fmt_p(own_p), FDR = fmt_p(own_p_bh)
      ) |>
      as.data.frame(),
    row.names = FALSE
  )
}

section(paste0(wave_label, " — SIMPLE SLOPES (significant numeric/binary own-arm interactions)"))
cat("Own-arm ATE at selected moderator values. Moderator is centered in the model;\n")
cat("reported values are on the original scale. Binary outcomes in the outcome's native units\n")
cat("(0–1 for voucher / coin; not converted to pp here).\n")

if (nrow(hits_num) == 0) {
  cat("\nNo numeric/binary own-arm hits; skipping simple slopes.\n")
} else {
  slope_rows <- purrr::map_dfr(seq_len(nrow(hits_num)), function(i) {
    row <- hits_num[i, ]
    key <- paste(row$mod_var, row$outcome, sep = "__")
    fit <- fits[[key]]
    treat <- row$treat_own
    mean_x <- fit$mod_mean_raw
    sd_x   <- fit$mod_sd_raw

    if (identical(row$mod_type, "binary")) {
      grid <- tibble(label = c("0 (no)", "1 (yes)"), x_raw = c(0, 1))
    } else {
      grid <- tibble(
        label = c("-1 SD", "Mean", "+1 SD"),
        x_raw = c(mean_x - sd_x, mean_x, mean_x + sd_x)
      )
    }
    grid <- grid |> mutate(m_c = x_raw - mean_x)
    purrr::pmap_dfr(grid, function(label, x_raw, m_c) {
      sl <- ate_at_mod(fit$model, treat, m_c)
      tibble(
        moderator = row$mod_label,
        outcome   = row$outcome_lab,
        at        = label,
        x         = round(x_raw, 2),
        ATE       = round(sl$ate, 3),
        SE        = round(sl$se, 3),
        p         = fmt_p(sl$p),
        sig       = fmt_sig(sl$p)
      )
    })
  })
  print(as.data.frame(slope_rows), row.names = FALSE)
}

invisible(list(fits = fits, joints = joints, ints = ints))
}

# =============================================================================
# RUN t1 AND t2
# =============================================================================

run_moderation_wave(
  d1, outcome_defs,
  wave_label = "t1"
)

d_t2 <- readRDS(here("data", "study_2", "study2_d.rds")) |>
  mutate(
    condition = droplevels(factor(
      condition, levels = c("control", "meat", "charity")
    )),
    order = droplevels(factor(order)),
    gender_mw = factor(gender_recoded, levels = c("Man", "Woman")),
    crt_num = as.numeric(crt_batball_cor)
  )

cat("\nFollow-up sample (t2 returners): N =", nrow(d_t2), "\n")
print(table(d_t2$condition))

outcome_defs_t2 <- tibble::tribble(
  ~key,          ~outcome,                   ~pre,                 ~short,                ~kind,      ~treat,    ~y_lab,                    ~as_pct,
  "meat_bel",    "belief_meat_change_t2",    "belief_meat_pre",    "Meat belief (t2)",    "belief",   "meat",    "Belief change",            FALSE,
  "charity_bel", "belief_charity_change_t2", "belief_charity_pre", "Charity belief (t2)", "belief",   "charity", "Belief change",            FALSE,
  "meat_beh",    "voucher_is_veg_t2",        "belief_meat_pre",    "Dietary choice (t2)", "behavior", "meat",    "% choosing veg voucher",   TRUE,
  "charity_beh", "donation_cents_t2",        "belief_charity_pre", "Charitable donation (t2)", "behavior", "charity", "Donation to AMF (cents)", FALSE
)

run_moderation_wave(
  d_t2, outcome_defs_t2,
  wave_label = "t2"
)

# =============================================================================
# SECTION 2 · HOLM-ADJUSTED SUPPLEMENT TABLE
# =============================================================================

# Own-arm interaction from
#   outcome ~ condition * moderator + condition * pre_belief_c + order
# with HC2 standard errors and control as the reference. The moderator is
# mean-centered, so the condition coefficient is the treatment effect at the
# average moderator. When the moderator is the matching pretreatment belief,
# that interaction is the main-model term and is not entered twice.
#
# The reported coefficient is the own-arm interaction versus control: how the
# own-arm treatment effect changes per one-unit increase in the moderator.
# Other arms are in the model; their interactions are not tested here.
#
# Holm is applied within each outcome, across all 14 moderators of that
# outcome, separately for the immediate and follow-up waves. Follow-up models
# use the recontact of control, meat, and charity only; lying and political
# violence had a separate follow-up.
#
# Writes:
#   output/study_2/tables/moderation-holm.tex
#   output/study_2/tables/moderation-holm.csv
prepare_wave <- function(data, conditions) {
  data |>
    mutate(
      condition = droplevels(factor(condition, levels = conditions)),
      order = droplevels(factor(order)),
      gender_mw = factor(gender_recoded, levels = c("Man", "Woman")),
      crt_num = as.numeric(crt_batball_cor)
    )
}

d1 <- readRDS(here("data", "study_2", "study2_d1.rds")) |>
  prepare_wave(c("control", "meat", "charity", "polviol", "lying")) |>
  mutate(
    voucher_is_veg = as.integer(
      dplyr::coalesce(voucher_DV_beeffirst, voucher_DV_vegfirst) == 1
    ),
    coin_heads = as.integer(lying_DV == 1)
  )

d_t2 <- readRDS(here("data", "study_2", "study2_d.rds")) |>
  prepare_wave(c("control", "meat", "charity"))

cat("Immediate completers: N =", nrow(d1), "\n")
print(table(d1$condition))
cat("Follow-up returners: N =", nrow(d_t2), "\n")
print(table(d_t2$condition))

# Columns of the supplement table, left to right.
outcome_defs <- tibble::tribble(
  ~wave, ~key,          ~outcome,                    ~pre,                  ~treat,    ~header,
  "t1",  "meat_bel",    "belief_meat_change",        "belief_meat_pre",     "meat",    "Meat belief",
  "t1",  "meat_beh",    "voucher_is_veg",            "belief_meat_pre",     "meat",    "Meat behaviour",
  "t1",  "charity_bel", "belief_charity_change",     "belief_charity_pre",  "charity", "Charity belief",
  "t1",  "charity_beh", "donation_cents",            "belief_charity_pre",  "charity", "Charity behaviour",
  "t1",  "lying_bel",   "belief_lying_change",       "belief_lying_pre",    "lying",   "Lying belief",
  "t1",  "lying_beh",   "coin_heads",                "belief_lying_pre",    "lying",   "Lying behaviour",
  "t1",  "polviol_bel", "belief_polviol_change",     "belief_polviol_pre",  "polviol", "PV belief",
  "t1",  "polviol_beh", "pv_participation",          "belief_polviol_pre",  "polviol", "PV behaviour",
  "t2",  "meat_bel",    "belief_meat_change_t2",     "belief_meat_pre",     "meat",    "Meat belief",
  "t2",  "meat_beh",    "voucher_is_veg_t2",         "belief_meat_pre",     "meat",    "Meat behaviour",
  "t2",  "charity_bel", "belief_charity_change_t2",  "belief_charity_pre",  "charity", "Charity belief",
  "t2",  "charity_beh", "donation_cents_t2",         "belief_charity_pre",  "charity", "Charity behaviour"
)

mod_defs <- tibble::tribble(
  ~var,                 ~label,                                      ~type,
  "age",                "Age",                                       "numeric",
  "gender_mw",          "Gender (woman vs.\\ man)",                  "factor",
  "education",          "Education",                                 "numeric",
  "income",             "Income",                                    "numeric",
  "race_white",         "White",                                     "binary",
  "AOT_mean",           "Actively open-minded thinking",             "numeric",
  "crt_num",            "Cognitive reflection",                      "binary",
  "pol_orientation",    "Political ideology",                        "numeric",
  "dem_rep_num",        "Party identification",                      "numeric",
  "trust_ai_pre",       "AI trust",                                  "numeric",
  "belief_meat_pre",    "Pretreatment meat belief",                  "numeric",
  "belief_charity_pre", "Pretreatment charity belief",               "numeric",
  "belief_lying_pre",   "Pretreatment lying belief",                 "numeric",
  "belief_polviol_pre", "Pretreatment political-violence belief",    "numeric"
)

wald_p <- function(model, terms) {
  co <- coef(model)
  V <- vcov(model)
  terms <- terms[terms %in% names(co) & !is.na(co[terms])]
  if (length(terms) == 0L) return(NA_real_)
  b <- co[terms]
  Vsub <- V[terms, terms, drop = FALSE]
  stat <- tryCatch(as.numeric(t(b) %*% solve(Vsub) %*% b), error = function(e) NA_real_)
  if (is.na(stat)) NA_real_ else pchisq(stat, df = length(b), lower.tail = FALSE)
}

# Own-arm condition x moderator term. Factor moderators use a joint Wald
# test when the contrast has more than one degree of freedom.
own_interaction <- function(model, treat) {
  terms <- grep(
    paste0("^condition", treat, ":moderator"),
    names(coef(model)),
    value = TRUE
  )
  if (length(terms) == 0L) {
    return(tibble(estimate = NA_real_, se = NA_real_, p = NA_real_, n_terms = 0L))
  }
  sm <- summary(model)$coefficients
  if (length(terms) == 1L) {
    return(tibble(
      estimate = unname(sm[terms, "Estimate"]),
      se = unname(sm[terms, "Std. Error"]),
      p = unname(sm[terms, "Pr(>|t|)"]),
      n_terms = 1L
    ))
  }
  tibble(
    estimate = NA_real_,
    se = NA_real_,
    p = wald_p(model, terms),
    n_terms = length(terms)
  )
}

fit_own <- function(data, outcome_var, pre_var, mod_var, mod_type, treat) {
  d <- data |>
    filter(
      !is.na(.data[[outcome_var]]), !is.na(.data[[pre_var]]),
      !is.na(.data[[mod_var]]), !is.na(condition), !is.na(order)
    ) |>
    mutate(
      outcome = as.numeric(.data[[outcome_var]]),
      pre_belief_c = .data[[pre_var]] - mean(.data[[pre_var]], na.rm = TRUE)
    )

  matching <- identical(mod_var, pre_var)
  if (identical(mod_type, "factor")) {
    d <- d |> mutate(moderator = droplevels(factor(.data[[mod_var]])))
  } else {
    d <- d |> mutate(
      moderator = as.numeric(.data[[mod_var]]) - mean(as.numeric(.data[[mod_var]]), na.rm = TRUE)
    )
  }

  if (matching) {
    m <- lm_robust(outcome ~ condition * moderator + order, data = d, se_type = "HC2")
  } else {
    m <- lm_robust(
      outcome ~ condition * moderator + condition * pre_belief_c + order,
      data = d, se_type = "HC2"
    )
  }

  own_interaction(m, treat) |>
    mutate(n = nrow(d), matching = matching)
}

section("FITTING OWN-ARM MODERATION MODELS")
cat(sprintf(
  "%d outcomes x %d moderators = %d models.\n",
  nrow(outcome_defs), nrow(mod_defs), nrow(outcome_defs) * nrow(mod_defs)
))

results <- purrr::map_dfr(seq_len(nrow(outcome_defs)), function(i) {
  odef <- outcome_defs[i, ]
  wave_data <- if (odef$wave == "t1") d1 else d_t2
  cat(sprintf("  %s %s\n", odef$wave, odef$header))
  purrr::map_dfr(seq_len(nrow(mod_defs)), function(j) {
    mdef <- mod_defs[j, ]
    fit_own(
      wave_data,
      outcome_var = odef$outcome,
      pre_var = odef$pre,
      mod_var = mdef$var,
      mod_type = mdef$type,
      treat = odef$treat
    ) |>
      mutate(
        wave = odef$wave,
        outcome = odef$key,
        header = odef$header,
        moderator = mdef$var,
        label = mdef$label
      )
  })
})

results <- results |>
  group_by(wave, outcome) |>
  mutate(p_holm = p.adjust(p, method = "holm")) |>
  ungroup() |>
  mutate(
    col_id = paste(wave, outcome, sep = "_"),
    label = factor(label, levels = mod_defs$label)
  )

fmt_holm_tex <- function(p) {
  cell <- dplyr::case_when(
    is.na(p) ~ "---",
    p < 0.001 ~ "<.001",
    p > 0.999 ~ "1.00",
    TRUE ~ sub("^0", "", sprintf("%.3f", p))
  )
  ifelse(!is.na(p) & p < 0.05, paste0("\\textbf{", cell, "}"), cell)
}

fmt_holm_txt <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "NA",
    p < 0.001 ~ "<.001",
    p > 0.999 ~ "1.00",
    TRUE ~ sub("^0", "", sprintf("%.3f", p))
  )
}

wide_tex <- results |>
  mutate(cell = fmt_holm_tex(p_holm)) |>
  select(label, col_id, cell) |>
  pivot_wider(names_from = col_id, values_from = cell)

col_order <- paste(outcome_defs$wave, outcome_defs$key, sep = "_")
stopifnot(all(col_order %in% names(wide_tex)))

header_stack <- function(text) {
  parts <- strsplit(text, " ", fixed = TRUE)[[1]]
  paste0("\\shortstack{", paste(parts, collapse = "\\\\"), "}")
}
col_heads <- paste(vapply(outcome_defs$header, header_stack, character(1)), collapse = " & ")

body <- purrr::map_chr(seq_len(nrow(wide_tex)), function(i) {
  cells <- vapply(col_order, function(col) wide_tex[[col]][[i]], character(1))
  sprintf("  %s & %s \\\\", wide_tex$label[[i]], paste(cells, collapse = " & "))
})

note <- paste(
  "\\footnotesize\\textit{Note.} Entries are Holm-adjusted $p$-values for the own-arm",
  "condition $\\times$ moderator interaction. For each outcome, Holm is applied",
  "across the 14 moderators. The model is",
  "outcome $\\sim$ condition $\\times$ moderator $+$ condition $\\times$ centered",
  "pretreatment belief $+$ order, with HC2 standard errors and control as the",
  "reference. The coefficient is the change in the own-arm treatment effect per",
  "one-unit increase in the moderator. Bold cells are Holm $p < .05$.",
  sep = " "
)

tex <- c(
  "% Requires booktabs and pdflscape.",
  "\\begin{landscape}",
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{Holm-adjusted tests of treatment-effect moderation.}",
  "  \\label{tab:moderation-holm}",
  "  \\footnotesize",
  "  \\setlength{\\tabcolsep}{3pt}",
  "  \\begin{tabular}{@{}l*{12}{c}@{}}",
  "  \\toprule",
  "  & \\multicolumn{4}{c}{Immediate, prosocial} & \\multicolumn{4}{c}{Immediate, antisocial} & \\multicolumn{4}{c}{Follow-up} \\\\",
  "  \\cmidrule(lr){2-5} \\cmidrule(lr){6-9} \\cmidrule(lr){10-13}",
  paste0("  Moderator & ", col_heads, " \\\\"),
  "  \\midrule",
  body,
  "  \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{2pt}",
  "  \\begin{minipage}{\\linewidth}",
  paste0("  ", note),
  "  \\end{minipage}",
  "\\end{table}",
  "\\end{landscape}",
  ""
)

out_dir <- here("output", "study_2", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_tex <- file.path(out_dir, "moderation-holm.tex")
out_csv <- file.path(out_dir, "moderation-holm.csv")
write_paper_table(tex, out_tex)

results |>
  transmute(
    wave, outcome = header, moderator = as.character(label),
    n, matching, estimate, se, p, p_holm,
    p_holm_fmt = fmt_holm_txt(p_holm)
  ) |>
  write.csv(out_csv, row.names = FALSE)

section("HOLM-ADJUSTED OWN-ARM INTERACTIONS")
show_tbl <- results |>
  mutate(cell = fmt_holm_txt(p_holm)) |>
  select(label, col_id, cell) |>
  pivot_wider(names_from = col_id, values_from = cell)
print(as.data.frame(show_tbl), row.names = FALSE, right = FALSE)

section("HOLM p < .05")
hits <- results |> filter(p_holm < 0.05)
if (nrow(hits) == 0) {
  cat("None.\n")
} else {
  hits |>
    transmute(
      wave, outcome = header, moderator = as.character(label),
      estimate = round(estimate, 3),
      se = round(se, 3),
      p = fmt_holm_txt(p),
      p_holm = fmt_holm_txt(p_holm)
    ) |>
    as.data.frame() |>
    print(row.names = FALSE)
}

cat(sprintf("\nWrote %s\nWrote %s\n", out_tex, out_csv))
cat("\nStudy 2 treatment-effect moderation complete.\n")
