# Shared helpers for Study 2 mediation scripts (09_belief_mediation.R, 13_rating_mediators.R).
# Study 1 persistence and Study 2 follow-up persistence import the settings
# below and keep their own model formulas.

# Reported intervals: nonparametric bootstrap, 1,000 simulations, seed 42.
# main.R may set mediation_bootstrap <- FALSE before sourcing analyses.
# That smoke test uses a quasi-Bayesian approximation with 20 simulations.
# A script run on its own leaves the flag unset and keeps the reported bootstrap.
MEDIATION_SEED <- 42L
MEDIATION_N_SIMS_FULL <- 1000L
MEDIATION_N_SIMS_FAST <- 20L

mediation_bootstrap_enabled <- function() {
  isTRUE(get0(
    "mediation_bootstrap", envir = .GlobalEnv, ifnotfound = TRUE, inherits = TRUE
  ))
}

mediation_n_sims <- function() {
  if (mediation_bootstrap_enabled()) MEDIATION_N_SIMS_FULL else MEDIATION_N_SIMS_FAST
}

# Phrase for table notes. tex = TRUE escapes the percent sign.
# The reported branch stays "percentile bootstrap 95% interval from 1,000 simulations".
mediation_interval_phrase <- function(tex = FALSE) {
  n_txt <- format(mediation_n_sims(), big.mark = ",", scientific = FALSE, trim = TRUE)
  if (mediation_bootstrap_enabled()) {
    phrase <- sprintf("percentile bootstrap 95%% interval from %s simulations", n_txt)
  } else {
    phrase <- sprintf(
      paste0(
        "quasi-Bayesian 95%% interval from %s simulations ",
        "(smoke test, not the reported bootstrap)"
      ),
      n_txt
    )
  }
  if (isTRUE(tex)) phrase <- gsub("%", "\\%", phrase, fixed = TRUE)
  phrase
}

mediation_sims_line <- function() {
  n <- mediation_n_sims()
  if (mediation_bootstrap_enabled()) {
    sprintf("Bootstrap sims = %d", n)
  } else {
    sprintf(
      "SMOKE TEST: quasi-Bayesian sims = %d (not the reported bootstrap)",
      n
    )
  }
}

announce_mediation_settings <- function() {
  if (mediation_bootstrap_enabled()) {
    cat(sprintf(
      "Mediation intervals: nonparametric bootstrap, %s simulations, seed %d.\n",
      format(mediation_n_sims(), big.mark = ",", scientific = FALSE, trim = TRUE),
      MEDIATION_SEED
    ))
    return(invisible(TRUE))
  }
  if (!isTRUE(getOption("mediation.smoke.announced", FALSE))) {
    options(mediation.smoke.announced = TRUE)
    cat("====================================================================\n")
    cat("SMOKE TEST: mediation intervals are NOT the reported bootstrap.\n")
    cat(sprintf(
      "Using a quasi-Bayesian approximation with %d simulations (seed %d).\n",
      mediation_n_sims(), MEDIATION_SEED
    ))
    cat("Regression coefficients match a full run.\n")
    cat("Indirect, direct, and total effects are averages of these draws, not the paper estimates.\n")
    if (isTRUE(get0(
      "save_to_paper_repo", envir = .GlobalEnv, ifnotfound = FALSE, inherits = TRUE
    ))) {
      cat("Figures and tables will not be copied to the manuscript repo.\n")
    }
    cat("====================================================================\n")
  }
  cat(mediation_sims_line(), "\n", sep = "")
  invisible(FALSE)
}

# Outcome column names and confirmatory arms: study2_outcomes.R.
# Scripts that source this file should also source study2_outcomes.R first.
# Fall back to sourcing it here if they have not.
if (!exists("study2_behavior_outcomes", inherits = FALSE) &&
    !exists("study2_behavior_outcomes", inherits = TRUE)) {
  # local = TRUE keeps the dictionary in the environment that is sourcing this
  # file. import_mediation_settings() evaluates this file in a fresh env whose
  # parent is baseenv(), so a default source() would write into .GlobalEnv
  # and the lookups below would still miss the objects.
  source(here::here("scripts", "functions", "study2_outcomes.R"), local = TRUE)
}

condition_labels <- c(
  meat    = "Meat",
  charity = "Charity",
  lying   = "Lying",
  polviol = "Political Violence"
)

# Behaviour via post-treatment belief (t1 mediator = post_t1).
behavior_defs <- study2_behavior_outcomes |>
  dplyr::transmute(
    key,
    y = outcome_t1,
    post = study2_belief_outcomes$post_t1[match(key, study2_belief_outcomes$key)],
    pre,
    short,
    treat = unname(study2_primary_treat[key])
  )

behavior_defs_t2 <- study2_behavior_outcomes |>
  dplyr::filter(key %in% c("meat", "charity"), !is.na(outcome_t2)) |>
  dplyr::transmute(
    key,
    y = outcome_t2,
    post = study2_belief_outcomes$post_t1[match(key, study2_belief_outcomes$key)],
    t1_beh = outcome_t1,
    pre,
    short = paste0(short, " (t2)"),
    treat = unname(study2_primary_treat[key])
  )

# Behaviour via follow-up belief level (m = level_t2).
t2_belief_defs <- study2_behavior_outcomes |>
  dplyr::filter(key %in% c("meat", "charity"), !is.na(outcome_t2)) |>
  dplyr::transmute(
    key,
    y = outcome_t2,
    m = study2_belief_outcomes$level_t2[match(key, study2_belief_outcomes$key)],
    pre,
    short = paste0(short, " (t2)"),
    treat = unname(study2_primary_treat[key])
  )

rating_defs <- tibble::tribble(
  ~key,            ~var,             ~label,
  "understood",    "ai_understood",  "Understood",
  "respectful",    "ai_respectful",  "Respectful",
  "originality",   "ai_originality", "Originality",
  "composite",     "ai_composite",   "Composite (mean)"
)

belief_outcome_defs <- study2_belief_outcomes |>
  dplyr::transmute(
    key,
    y = post_t1,
    pre,
    treat = unname(study2_primary_treat[key])
  )

belief_outcome_defs_t2 <- study2_belief_outcomes |>
  dplyr::filter(key %in% c("meat", "charity"), !is.na(level_t2)) |>
  dplyr::transmute(
    key,
    y = level_t2,
    pre,
    treat = unname(study2_primary_treat[key])
  )

mediation_outcome_order <- c("meat", "charity", "lying", "polviol")
mediation_domain_labels <- c(
  meat = "Meat",
  charity = "Charity",
  lying = "Lying",
  polviol = "Political violence"
)

mediation_belief_panels <- list(
  list(
    panel = "t1_beh_via_t1_bel",
    title = "Time 1 behaviour, via time 1 belief",
    keys = c("meat", "charity", "lying", "polviol"),
    mediator = "t1_belief"
  ),
  list(
    panel = "t2_beh_via_t1_bel",
    title = "Time 2 behaviour, via time 1 belief",
    keys = c("meat", "charity"),
    mediator = "t1_belief"
  ),
  list(
    panel = "t2_beh_via_t2_bel",
    title = "Time 2 behaviour, via time 2 belief",
    keys = c("meat", "charity"),
    mediator = "t2_belief"
  )
)

mediation_rating_panels <- list(
  list(
    panel = "rating_t1_bel",
    title = "Time 1 beliefs",
    keys = c("meat", "charity", "lying", "polviol")
  ),
  list(
    panel = "rating_t2_bel",
    title = "Time 2 beliefs",
    keys = c("meat", "charity")
  ),
  list(
    panel = "rating_t1_beh",
    title = "Time 1 behaviour",
    keys = c("meat", "charity", "lying", "polviol")
  ),
  list(
    panel = "rating_t2_beh",
    title = "Time 2 behaviour",
    keys = c("meat", "charity")
  )
)

mediation_rating_mediators <- c(
  understood = "Understanding",
  respectful = "Respectfulness",
  originality = "Novelty"
)

load_study2_d1_mediation <- function() {
  d1 <- readRDS(here::here("data", "study_2", "study2_d1.rds"))
  d1 |>
    dplyr::mutate(
      condition = factor(
        condition,
        levels = c("control", "meat", "charity", "polviol", "lying")
      ),
      order = factor(order),
      voucher_is_veg = as.numeric(
        dplyr::coalesce(voucher_DV_beeffirst, voucher_DV_vegfirst) == 1
      ),
      coin_heads = as.numeric(lying_DV == 1),
      ai_composite = rowMeans(
        cbind(ai_understood, ai_respectful, ai_originality),
        na.rm = FALSE
      )
    )
}

load_study2_d_t2_mediation <- function() {
  readRDS(here::here("data", "study_2", "study2_d.rds")) |>
    dplyr::mutate(
      condition = droplevels(factor(
        condition, levels = c("control", "meat", "charity")
      )),
      order = droplevels(factor(order)),
      voucher_is_veg = as.numeric(voucher_is_veg),
      voucher_is_veg_t2 = as.numeric(voucher_is_veg_t2),
      ai_composite = rowMeans(
        cbind(ai_understood, ai_respectful, ai_originality),
        na.rm = FALSE
      )
    )
}

run_mediation <- function(data, y, m, t0, treat_group,
                          control_group = "control",
                          sims = mediation_n_sims(),
                          boot = mediation_bootstrap_enabled()) {
  d_med <- data |>
    dplyr::filter(
      !is.na(.data[[y]]), !is.na(.data[[m]]), !is.na(.data[[t0]]),
      !is.na(condition), !is.na(order)
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

  s_m1 <- summary(m1)$coefficients
  s_m2 <- summary(m2)$coefficients
  treat_coef <- paste0("condition", treat_group)
  a_est <- if (treat_coef %in% rownames(s_m1)) s_m1[treat_coef, "Estimate"] else NA_real_
  a_p   <- if (treat_coef %in% rownames(s_m1)) s_m1[treat_coef, "Pr(>|t|)"] else NA_real_
  b_est <- if (m %in% rownames(s_m2)) s_m2[m, "Estimate"] else NA_real_
  b_p   <- if (m %in% rownames(s_m2)) s_m2[m, "Pr(>|t|)"] else NA_real_

  list(
    m1 = m1, m2 = m2, med = med, n = n,
    a = a_est, a_p = a_p, b = b_est, b_p = b_p
  )
}

extract_med_row <- function(res, label_outcome, label_mediator, treat_lab) {
  med <- res$med
  tibble::tibble(
    outcome   = label_outcome,
    mediator  = label_mediator,
    contrast  = paste0(treat_lab, " vs Control"),
    n         = res$n,
    a         = round(res$a, 3),
    a_p       = fmt_p(res$a_p),
    b         = round(res$b, 3),
    b_p       = fmt_p(res$b_p),
    ACME      = round(med$d.avg, 4),
    ACME_lo   = round(med$d.avg.ci[1], 4),
    ACME_hi   = round(med$d.avg.ci[2], 4),
    ACME_p    = fmt_p(med$d.avg.p),
    ADE       = round(med$z.avg, 4),
    ADE_lo    = round(med$z.avg.ci[1], 4),
    ADE_hi    = round(med$z.avg.ci[2], 4),
    ADE_p     = fmt_p(med$z.avg.p),
    total     = round(med$tau.coef, 4),
    total_p   = fmt_p(med$tau.p),
    prop_med  = round(med$n.avg, 3),
    prop_p    = fmt_p(med$n.avg.p)
  )
}

med_record <- function(res, panel, outcome_key, mediator_key) {
  med <- res$med
  tibble::tibble(
    panel = panel,
    outcome_key = outcome_key,
    mediator_key = mediator_key,
    n = res$n,
    indirect = unname(med$d.avg),
    indirect_lo = unname(med$d.avg.ci[1]),
    indirect_hi = unname(med$d.avg.ci[2]),
    indirect_p = unname(med$d.avg.p),
    direct = unname(med$z.avg),
    direct_lo = unname(med$z.avg.ci[1]),
    direct_hi = unname(med$z.avg.ci[2]),
    direct_p = unname(med$z.avg.p),
    total = unname(med$tau.coef),
    total_lo = unname(med$tau.ci[1]),
    total_hi = unname(med$tau.ci[2]),
    total_p = unname(med$tau.p),
    prop = unname(med$n.avg),
    prop_lo = unname(med$n.avg.ci[1]),
    prop_hi = unname(med$n.avg.ci[2]),
    prop_p = unname(med$n.avg.p)
  )
}

print_mediation_full <- function(label, res) {
  cat("\n------", label, "------\n")
  cat(sprintf("N (complete cases) = %d\n", res$n))
  cat("\nModel 1 (mediator): M ~ condition + t0 + order\n")
  print(summary(res$m1))
  cat("\nModel 2 (outcome): Y ~ M + condition + t0 + order\n")
  print(summary(res$m2))
  cat(if (mediation_bootstrap_enabled()) {
    "\nMediation summary (bootstrap):\n"
  } else {
    "\nMediation summary (quasi-Bayesian smoke test; not the reported bootstrap):\n"
  })
  print(summary(res$med))
}

print_mediation_summary_table <- function(summary_df) {
  print(
    as.data.frame(
      summary_df |>
        dplyr::select(
          outcome, mediator, contrast, n,
          a, a_p, b, b_p,
          ACME, ACME_lo, ACME_hi, ACME_p,
          ADE, ADE_p, total, total_p, prop_med, prop_p
        )
    ),
    row.names = FALSE
  )
}

mediation_grab_row <- function(results, panel, outcome_key, mediator_key) {
  hit <- results[
    results$panel == panel &
      results$outcome_key == outcome_key &
      results$mediator_key == mediator_key,
    ,
    drop = FALSE
  ]
  if (nrow(hit) != 1L) {
    stop(sprintf(
      "Expected 1 row for panel %s, outcome %s, mediator %s; found %d.",
      panel, outcome_key, mediator_key, nrow(hit)
    ))
  }
  hit
}

fmt_tex_est <- function(est, lo, hi, p) {
  est_txt <- sprintf("%.2f", est)
  if (!is.na(p) && p < 0.05) est_txt <- paste0("\\textbf{", est_txt, "}")
  sprintf("\\shortstack{%s\\\\ {[%.2f, %.2f]}}", est_txt, lo, hi)
}

fmt_txt_est <- function(est, lo, hi, p) {
  star <- if (!is.na(p) && p < 0.05) "*" else ""
  sprintf("%.2f%s [%.2f, %.2f]", est, star, lo, hi)
}

fmt_tex_pct <- function(est, lo, hi, p) {
  est_txt <- sprintf("%.0f\\%%", 100 * est)
  if (!is.na(p) && p < 0.05) est_txt <- paste0("\\textbf{", est_txt, "}")
  sprintf("\\shortstack{%s\\\\ {[%.0f\\%%, %.0f\\%%]}}", est_txt, 100 * lo, 100 * hi)
}

fmt_txt_pct <- function(est, lo, hi, p) {
  star <- if (!is.na(p) && p < 0.05) "*" else ""
  sprintf("%.0f%%%s [%.0f%%, %.0f%%]", 100 * est, star, 100 * lo, 100 * hi)
}

build_belief_mediation_tex <- function(table_results) {
  belief_body <- unlist(lapply(mediation_belief_panels, function(spec) {
    row_lines <- vapply(spec$keys, function(key) {
      hit <- mediation_grab_row(table_results, spec$panel, key, spec$mediator)
      sprintf(
        "  %s & %s & %s & %s & %s \\\\",
        mediation_domain_labels[[key]],
        fmt_tex_est(hit$total, hit$total_lo, hit$total_hi, hit$total_p),
        fmt_tex_est(hit$indirect, hit$indirect_lo, hit$indirect_hi, hit$indirect_p),
        fmt_tex_est(hit$direct, hit$direct_lo, hit$direct_hi, hit$direct_p),
        fmt_tex_pct(hit$prop, hit$prop_lo, hit$prop_hi, hit$prop_p)
      )
    }, character(1))
    c(
      paste0("  \\midrule"),
      paste0("  \\multicolumn{5}{@{}l}{\\textbf{", spec$title, "}} \\\\"),
      row_lines
    )
  }), use.names = FALSE)

  belief_note <- paste(
    "\\footnotesize\\textit{Note.} Cells are the total effect, the indirect effect,",
    "the direct effect, and the percent mediated for the own-arm treatment versus",
    "control, with the",
    paste0(mediation_interval_phrase(tex = TRUE), ". Percent mediated"),
    "is the indirect effect as a percentage of the total effect. The mediator model is",
    "$M \\sim \\mathrm{condition} + \\mathrm{t0} + \\mathrm{order}$ and the outcome",
    "model is $Y \\sim M + \\mathrm{condition} + \\mathrm{t0} + \\mathrm{order}$,",
    "where t0 is the matching pretreatment belief. There is no",
    "condition $\\times$ mediator interaction, so the mediator--outcome slope is",
    "pooled across arms and the indirect effect is that slope times the own-arm",
    "contrast on the mediator. Bold estimates have $p < .05$.",
    sep = " "
  )

  c(
    "% Requires booktabs.",
    "\\begin{table}[!htbp]",
    "  \\centering",
    "  \\caption{Moral beliefs as mediators of behaviour.}",
    "  \\label{tab:mediation-beliefs}",
    "  \\footnotesize",
    "  \\setlength{\\tabcolsep}{8pt}",
    "  \\begin{tabular}{@{}lcccc@{}}",
    "  \\toprule",
    "   & Total effect & Indirect & Direct & \\% mediated \\\\",
    belief_body,
    "  \\bottomrule",
    "  \\end{tabular}",
    "  \\vspace{2pt}",
    "  \\begin{minipage}{\\linewidth}",
    paste0("  ", belief_note),
    "  \\end{minipage}",
    "\\end{table}",
    ""
  )
}

build_rating_mediation_tex <- function(table_results) {
  rating_body <- unlist(lapply(mediation_rating_panels, function(spec) {
    row_lines <- vapply(spec$keys, function(key) {
      cells <- unlist(lapply(names(mediation_rating_mediators), function(med_key) {
        hit <- mediation_grab_row(table_results, spec$panel, key, med_key)
        c(
          fmt_tex_est(hit$indirect, hit$indirect_lo, hit$indirect_hi, hit$indirect_p),
          fmt_tex_est(hit$direct, hit$direct_lo, hit$direct_hi, hit$direct_p),
          fmt_tex_pct(hit$prop, hit$prop_lo, hit$prop_hi, hit$prop_p)
        )
      }), use.names = FALSE)
      sprintf("  %s & %s \\\\", mediation_domain_labels[[key]], paste(cells, collapse = " & "))
    }, character(1))
    c(
      "  \\midrule",
      paste0("  \\multicolumn{10}{@{}l}{\\textbf{", spec$title, "}} \\\\"),
      row_lines
    )
  }), use.names = FALSE)

  rating_note <- paste(
    "\\footnotesize\\textit{Note.} Cells are the indirect effect, the direct effect,",
    "and the percent mediated for the own-arm treatment versus control, with the",
    paste0(mediation_interval_phrase(tex = TRUE), ". Percent mediated"),
    "is the indirect effect as a percentage of the total effect. The mediator model is",
    "$M \\sim \\mathrm{condition} + \\mathrm{t0} + \\mathrm{order}$ and the outcome",
    "model is $Y \\sim M + \\mathrm{condition} + \\mathrm{t0} + \\mathrm{order}$,",
    "where t0 is the matching pretreatment belief. There is no",
    "condition $\\times$ mediator interaction, so the mediator--outcome slope is",
    "pooled across arms and the indirect effect is that slope times the own-arm",
    "contrast on the mediator. Bold estimates have $p < .05$.",
    "When the total effect is very small, percent mediated figures can be unreliable.",
    sep = " "
  )

  c(
    "% Requires booktabs and pdflscape.",
    "\\begin{landscape}",
    "\\begin{table}[!htbp]",
    "  \\centering",
    "  \\caption{Post-conversation perceptions as mediators of beliefs and behaviour.}",
    "  \\label{tab:mediation-ratings}",
    "  \\footnotesize",
    "  \\setlength{\\tabcolsep}{4pt}",
    "  \\begin{tabular}{@{}l*{9}{c}@{}}",
    "  \\toprule",
    "   & \\multicolumn{3}{c}{Understanding} & \\multicolumn{3}{c}{Respectfulness} & \\multicolumn{3}{c}{Novelty} \\\\",
    "  \\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
    "   & Indirect & Direct & \\% mediated & Indirect & Direct & \\% mediated & Indirect & Direct & \\% mediated \\\\",
    rating_body,
    "  \\bottomrule",
    "  \\end{tabular}",
    "  \\vspace{2pt}",
    "  \\begin{minipage}{\\linewidth}",
    paste0("  ", rating_note),
    "  \\end{minipage}",
    "\\end{table}",
    "\\end{landscape}",
    ""
  )
}

write_mediation_csv <- function(results, panel_levels, path) {
  mediator_levels <- c("t1_belief", "t2_belief", "understood", "respectful", "originality")
  out <- results |>
    dplyr::filter(panel %in% panel_levels) |>
    dplyr::mutate(
      panel = factor(panel, levels = panel_levels),
      outcome_key = factor(outcome_key, levels = mediation_outcome_order),
      outcome = unname(mediation_domain_labels[as.character(outcome_key)]),
      mediator_key = factor(mediator_key, levels = mediator_levels)
    ) |>
    dplyr::arrange(panel, outcome_key, mediator_key) |>
    dplyr::select(
      panel, outcome_key, outcome, mediator_key, n,
      indirect, indirect_lo, indirect_hi, indirect_p,
      direct, direct_lo, direct_hi, direct_p,
      total, total_lo, total_hi, total_p,
      prop, prop_lo, prop_hi, prop_p
    )
  utils::write.csv(out, path, row.names = FALSE)
}

print_belief_mediation_console <- function(table_results) {
  if (mediation_bootstrap_enabled()) {
    cat("Stars mark p < .05. Intervals are percentile bootstrap 95% CIs.\n\n")
  } else {
    cat(sprintf(
      paste0(
        "Stars mark p < .05. SMOKE TEST: intervals are quasi-Bayesian ",
        "95%% draws from %d simulations, not the reported bootstrap.\n\n"
      ),
      mediation_n_sims()
    ))
  }
  for (spec in mediation_belief_panels) {
    cat(spec$title, "\n")
    show <- lapply(spec$keys, function(key) {
      hit <- mediation_grab_row(table_results, spec$panel, key, spec$mediator)
      data.frame(
        outcome = mediation_domain_labels[[key]],
        total = fmt_txt_est(hit$total, hit$total_lo, hit$total_hi, hit$total_p),
        indirect = fmt_txt_est(hit$indirect, hit$indirect_lo, hit$indirect_hi, hit$indirect_p),
        direct = fmt_txt_est(hit$direct, hit$direct_lo, hit$direct_hi, hit$direct_p),
        pct_mediated = fmt_txt_pct(hit$prop, hit$prop_lo, hit$prop_hi, hit$prop_p),
        stringsAsFactors = FALSE
      )
    })
    print(do.call(rbind, show), row.names = FALSE)
    cat("\n")
  }
}

print_rating_mediation_console <- function(table_results) {
  if (mediation_bootstrap_enabled()) {
    cat("Stars mark p < .05. Intervals are percentile bootstrap 95% CIs.\n\n")
  } else {
    cat(sprintf(
      paste0(
        "Stars mark p < .05. SMOKE TEST: intervals are quasi-Bayesian ",
        "95%% draws from %d simulations, not the reported bootstrap.\n\n"
      ),
      mediation_n_sims()
    ))
  }
  for (spec in mediation_rating_panels) {
    cat(spec$title, "\n")
    show <- lapply(spec$keys, function(key) {
      cells <- lapply(names(mediation_rating_mediators), function(med_key) {
        hit <- mediation_grab_row(table_results, spec$panel, key, med_key)
        c(
          indirect = fmt_txt_est(hit$indirect, hit$indirect_lo, hit$indirect_hi, hit$indirect_p),
          direct = fmt_txt_est(hit$direct, hit$direct_lo, hit$direct_hi, hit$direct_p),
          pct = fmt_txt_pct(hit$prop, hit$prop_lo, hit$prop_hi, hit$prop_p)
        )
      })
      stats::setNames(
        c(mediation_domain_labels[[key]], unlist(cells, use.names = FALSE)),
        c(
          "outcome",
          paste(
            rep(names(mediation_rating_mediators), each = 3),
            c("indirect", "direct", "pct"),
            sep = "_"
          )
        )
      )
    })
    print(as.data.frame(do.call(rbind, show)), row.names = FALSE, right = FALSE)
    cat("\n")
  }
}
