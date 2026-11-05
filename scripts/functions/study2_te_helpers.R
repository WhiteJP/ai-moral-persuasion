# =============================================================================
# Shared Study 2 treatment-effect helpers (prereg Lin-style models)
# =============================================================================
# Used by 04a / 04b (and spillover where applicable).
#
# Model: outcome ~ condition * pre_belief_c + order, HC2 SEs.
# Control is the reference level of `condition`.
# fmt_p / fmt_sig / fmt_bracket_sig / fmt_p_tex live in helpers.R.

fmt_mean_cell <- function(m, sd, type) {
  switch(
    type,
    binary = sprintf("%.1f (%.1f)", 100 * m, 100 * sd),
    cents  = sprintf("%.2f (%.2f)", m, sd),
    points = sprintf("%.2f (%.2f)", m, sd),
    z      = sprintf("%.3f (%.3f)", m, sd),
    sprintf("%.2f (%.2f)", m, sd)
  )
}

#' Frozen denominator for Cohen's d. The numerator is the model coefficient,
#' on the same scale as this SD. For a linear probability model that scale is
#' the 0/1 outcome, not percentage points.
#'
#' `role = "pre"`: ordinary `sd()` of a pretreatment item on `data`, condition
#' labels ignored. Pass the full baseline sample. Those scores were measured
#' before the conversation, so arm is not a grouping. Reuse the same number
#' at follow-up rather than recomputing it on returners.
#'
#' `role = "control"`: `sd()` in the control arm of an outcome that has no
#' pretreatment measure of itself. Pass the time-1 baseline for time-1
#' behaviors, including binary outcomes, and reuse that number at follow-up.
#' Self-reports exist only at follow-up, so pass the follow-up data.
study2_d_sd <- function(data, x, role = c("pre", "control")) {
  role <- match.arg(role)
  vals <- data[[x]]
  if (role == "control") {
    vals <- vals[as.character(data$condition) == "control"]
  }
  stats::sd(vals, na.rm = TRUE)
}

#' Fit preregistered model; returns model, analysis data, N, and SDs.
#' `sd_pre` and `sd_out` are descriptive SDs of the estimation sample.
#' Cohen's d does not use them. Pass `sd_by_key` to `collect_ates()`.
fit_prereg <- function(data, outcome_var, pre_var) {
  dat <- data |>
    dplyr::filter(
      !is.na(.data[[outcome_var]]), !is.na(.data[[pre_var]]),
      !is.na(condition), !is.na(order)
    ) |>
    dplyr::mutate(
      outcome = as.numeric(.data[[outcome_var]]),
      pre_belief_c = .data[[pre_var]] - mean(.data[[pre_var]], na.rm = TRUE)
    )
  m <- estimatr::lm_robust(
    outcome ~ condition * pre_belief_c + order,
    data = dat,
    se_type = "HC2"
  )
  list(
    model = m,
    data = dat,
    n = nrow(dat),
    sd_pre = stats::sd(dat[[pre_var]], na.rm = TRUE),
    sd_out = stats::sd(dat$outcome, na.rm = TRUE)
  )
}

extract_ate <- function(fit, treat_level) {
  co <- summary(fit$model)$coefficients
  term <- paste0("condition", treat_level)
  if (!term %in% rownames(co)) {
    return(tibble::tibble(
      treat = treat_level, estimate = NA_real_, se = NA_real_,
      t = NA_real_, p = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_
    ))
  }
  tibble::tibble(
    treat = treat_level,
    estimate = co[term, "Estimate"],
    se = co[term, "Std. Error"],
    t = co[term, "t value"],
    p = co[term, "Pr(>|t|)"],
    ci_lo = co[term, "CI Lower"],
    ci_hi = co[term, "CI Upper"]
  )
}

summarise_by_cond <- function(data, outcome_var, type) {
  data |>
    dplyr::filter(!is.na(.data[[outcome_var]]), !is.na(condition)) |>
    dplyr::group_by(condition) |>
    dplyr::summarise(
      n = dplyr::n(),
      M = mean(as.numeric(.data[[outcome_var]]), na.rm = TRUE),
      SD = stats::sd(as.numeric(.data[[outcome_var]]), na.rm = TRUE),
      se = SD / sqrt(n),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean_plot = if (type == "binary") 100 * M else M,
      se_plot   = if (type == "binary") 100 * se else se,
      ci_lower  = mean_plot - 1.96 * se_plot,
      ci_upper  = mean_plot + 1.96 * se_plot,
      cell      = fmt_mean_cell(M, SD, type)
    )
}

# LPM-adjusted P(Y=1) and mean design rows by condition.
lpm_cond_linpred <- function(fit) {
  m <- fit$model
  d <- fit$data
  b <- stats::coef(m)
  keep <- !is.na(b)
  b <- b[keep]
  V <- stats::vcov(m)[names(b), names(b), drop = FALSE]
  conds <- levels(d$condition)

  xbar <- lapply(conds, function(cond) {
    d_cf <- d
    d_cf$condition <- factor(cond, levels = conds)
    X <- stats::model.matrix(stats::formula(m), data = d_cf)[, names(b), drop = FALSE]
    colMeans(X)
  })
  names(xbar) <- conds
  p <- vapply(xbar, function(x) as.numeric(sum(x * b)), numeric(1))
  list(xbar = xbar, p = p, V = V)
}

safe_rr <- function(p_t, p_c) {
  if (is.na(p_t) || is.na(p_c) || p_c <= 0) NA_real_ else p_t / p_c
}

safe_or <- function(p_t, p_c) {
  if (is.na(p_t) || is.na(p_c) || p_t <= 0 || p_t >= 1 || p_c <= 0 || p_c >= 1) {
    NA_real_
  } else {
    (p_t / (1 - p_t)) / (p_c / (1 - p_c))
  }
}

safe_nnt <- function(p_t, p_c) {
  rd <- abs(p_t - p_c)
  if (is.na(rd) || rd < 1e-12) NA_real_ else 1 / rd
}

or_delta_ci <- function(xt, xc, p_t, p_c, V) {
  if (is.na(safe_or(p_t, p_c))) {
    return(list(lo = NA_real_, hi = NA_real_))
  }
  g <- (1 / (p_t * (1 - p_t))) * xt - (1 / (p_c * (1 - p_c))) * xc
  se <- sqrt(as.numeric(t(g) %*% V %*% g))
  logor <- log(p_t / (1 - p_t)) - log(p_c / (1 - p_c))
  z <- stats::qnorm(0.975)
  list(lo = exp(logor - z * se), hi = exp(logor + z * se))
}

#' Collect condition main-effect ATEs (and binary OR/RR/NNT when type == binary).
#'
#' Cohen's d = model coefficient / `sd_by_key[outcome]`. NA in `sd_by_key`
#' leaves d missing. `d_from` only chooses which printed column carries that
#' standardizer.
#' @param d_from "pre" stores the standardizer in sd_pre; "out" stores it in sd_out
#' @param sd_by_key Named numeric vector, one frozen standardizer per outcome key
collect_ates <- function(model_list, defs, treat_levels, primary_treat,
                         condition_labels, d_from = c("pre", "out"),
                         sd_by_key, with_nnt = TRUE) {
  d_from <- match.arg(d_from)
  purrr::map_dfr(defs$key, function(key) {
    fit <- model_list[[key]]
    row <- defs[defs$key == key, ]
    type_val <- if ("type" %in% names(defs)) row$type[[1]] else NA_character_
    is_bin <- identical(type_val, "binary")
    lin <- if (is_bin) lpm_cond_linpred(fit) else NULL
    p_ctrl <- if (is_bin) unname(lin$p["control"]) else NA_real_
    if (!key %in% names(sd_by_key)) {
      stop("No Cohen's d standardizer for outcome '", key, "'.", call. = FALSE)
    }
    # Frozen standardizer. NA leaves d missing.
    sd_den <- unname(sd_by_key[[key]])

    purrr::map_dfr(treat_levels, function(tr) {
      p_tr <- if (is_bin) unname(lin$p[tr]) else NA_real_
      or_ci <- if (is_bin) {
        or_delta_ci(lin$xbar[[tr]], lin$xbar[["control"]], p_tr, p_ctrl, lin$V)
      } else {
        list(lo = NA_real_, hi = NA_real_)
      }
      extract_ate(fit, tr) |>
        dplyr::mutate(
          outcome = key,
          n = fit$n,
          sd_den = sd_den,
          sd_pre = if (d_from == "pre") sd_den else fit$sd_pre,
          sd_out = if (d_from == "out") sd_den else fit$sd_out,
          d = if (is.na(sd_den)) NA_real_ else estimate / sd_den,
          type = type_val,
          primary = unname(primary_treat[key]) == tr,
          p_ctrl = p_ctrl,
          p_treat = p_tr,
          rr = if (is_bin) safe_rr(p_tr, p_ctrl) else NA_real_,
          nnt = if (is_bin && with_nnt) safe_nnt(p_tr, p_ctrl) else NA_real_,
          or = if (is_bin) safe_or(p_tr, p_ctrl) else NA_real_,
          or_lo = or_ci$lo,
          or_hi = or_ci$hi
        )
    })
  }) |>
    dplyr::mutate(
      outcome_lab = defs$short[match(outcome, defs$key)],
      treat_lab = unname(condition_labels[treat])
    )
}

print_primary_belief_table <- function(ates) {
  tbl <- ates |>
    dplyr::filter(primary) |>
    dplyr::mutate(
      contrast = paste0(treat_lab, " vs Control"),
      ATE = round(estimate, 3),
      SE = round(se, 3),
      t = round(t, 3),
      p_fmt = fmt_p(p),
      sig = fmt_sig(p),
      CI = sprintf("[%.2f, %.2f]", ci_lo, ci_hi),
      d = round(d, 3),
      sd_pre = round(sd_pre, 2)
    ) |>
    dplyr::select(
      outcome = outcome_lab, contrast, n, ATE, SE, t, p_fmt, sig, CI, d, sd_pre
    )
  cat("\nPreregistered primary estimands (own-arm belief change):\n")
  cat(paste(
    "Cohen's d = model coefficient / SD of the pretreatment item on the full",
    "baseline sample, condition ignored. The same SD is used at follow-up.\n\n"
  ))
  print(as.data.frame(tbl), row.names = FALSE)
  invisible(tbl)
}

print_primary_behavior_table <- function(ates, d_note) {
  tbl <- ates |>
    dplyr::filter(primary) |>
    dplyr::mutate(
      contrast = paste0(treat_lab, " vs Control"),
      ATE_disp = dplyr::if_else(type == "binary", 100 * estimate, estimate),
      SE_disp  = dplyr::if_else(type == "binary", 100 * se, se),
      ci_lo_d  = dplyr::if_else(type == "binary", 100 * ci_lo, ci_lo),
      ci_hi_d  = dplyr::if_else(type == "binary", 100 * ci_hi, ci_hi),
      ATE = round(ATE_disp, 3),
      SE  = round(SE_disp, 3),
      t   = round(t, 3),
      p_fmt = fmt_p(p),
      sig = fmt_sig(p),
      CI = sprintf("[%.2f, %.2f]", ci_lo_d, ci_hi_d),
      d = dplyr::if_else(is.na(d), "\u2014", sprintf("%.3f", d)),
      # Binary ATEs are printed in percentage points, so print the SD in
      # percentage points too. d is unchanged: both the coefficient and the
      # control-arm SD were on the 0/1 scale.
      sd_disp = dplyr::if_else(type == "binary", 100 * sd_out, sd_out),
      sd_out = dplyr::if_else(is.na(sd_disp), "\u2014", sprintf("%.3f", sd_disp)),
      unit = dplyr::case_when(
        type == "binary" ~ "pp",
        type == "cents" ~ "cents",
        type == "z" ~ "z",
        type == "count" ~ "count",
        TRUE ~ "points"
      ),
      OR = dplyr::case_when(
        type != "binary" | is.na(or) ~ "—",
        is.na(or_lo) | is.na(or_hi)  ~ sprintf("%.2f", or),
        TRUE                         ~ sprintf("%.2f [%.2f, %.2f]", or, or_lo, or_hi)
      ),
      RR = dplyr::if_else(type == "binary" & !is.na(rr), sprintf("%.2f", rr), "—"),
      NNT = dplyr::if_else(type == "binary" & !is.na(nnt), sprintf("%.1f", nnt), "—")
    ) |>
    dplyr::select(
      outcome = outcome_lab, contrast, n, unit, ATE, SE, t, p_fmt, sig, CI, d, sd_out,
      OR, RR, NNT
    )
  cat("\nPreregistered primary estimands (own-arm behaviour).\n")
  cat(d_note, "\n")
  if (any(ates$type == "binary", na.rm = TRUE)) {
    cat("Binary ATEs and SDs in percentage points. d is their ratio.\n")
    cat("OR / RR / NNT from LPM-adjusted P(treat) and P(control); OR CI is delta method.\n")
  }
  cat("\n")
  print(as.data.frame(tbl), row.names = FALSE)
  invisible(tbl)
}

#' Fit one model per row of defs; optionally print one-line fits instead of full summaries.
fit_prereg_models <- function(data, defs, outcome_col = "change",
                              verbose = isTRUE(getOption("amp.verbose.models", FALSE))) {
  models <- list()
  for (i in seq_len(nrow(defs))) {
    key <- defs$key[i]
    outcome_var <- defs[[outcome_col]][i]
    fit <- fit_prereg(data, outcome_var, defs$pre[i])
    models[[key]] <- fit
    if (verbose) {
      cat("\n------", defs$short[i], "------\n")
      print(summary(fit$model))
    } else {
      cat(sprintf(
        "  Fitted %-22s  N = %d\n",
        paste0(defs$short[i], ":"), fit$n
      ))
    }
  }
  models
}

save_fig_png <- function(plot, filename, width, height, output_dir, dpi = FIGURE_DPI) {
  path <- file.path(output_dir, filename)
  ggplot2::ggsave(
    path, plot,
    width = width, height = height, units = "in", dpi = dpi, bg = "white"
  )
  cat(sprintf("Saved: %s (%.2f in wide)\n", path, width))
  invisible(path)
}
