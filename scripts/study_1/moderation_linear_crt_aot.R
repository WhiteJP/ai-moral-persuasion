# =============================================================================
# Exploratory heterogeneity · Study 1 (AOT / CRT moderation)
# =============================================================================
# Linear moderation: outcome ~ condition * pretreatment (centered) +
# condition * moderator, fit separately for AOT_mean and CRT (bat-ball correct).
#
# Samples: T1 = initial completers (d1); T2 = follow-up matched sample (d).
# Study 1 is two-arm (Meat vs Charity); Charity is the factor reference.
#
# Multiple comparisons: Benjamini–Hochberg FDR and Holm on the condition ×
# moderator interaction p-value only. AOT and CRT are separate families
# (8 tests each = 4 outcomes × T1 and T2).

suppressPackageStartupMessages({
  library(dplyr)
  library(estimatr)
})

.required_t1_outcomes <- c(
  "meat_wrong_change", "charity_wrong_change",
  "charity_donation_cents", "voucher_is_veg"
)
.required_t2_outcomes <- c(
  "meat_wrong_change_t2", "charity_wrong_change_t2",
  "charity_donation_cents_t2", "voucher_is_veg_t2"
)
.required_t2_inputs <- c(
  "meat_wrong_t2", "charity_wrong_t2", "meat_wrong_pre", "charity_wrong_pre"
)

outcome_specs_t1 <- tibble::tribble(
  ~outcome, ~pre_var,
  "meat_wrong_change", "meat_wrong_pre_c",
  "charity_wrong_change", "charity_wrong_pre_c",
  "charity_donation_cents", "charity_wrong_pre_c",
  "voucher_is_veg", "meat_wrong_pre_c"
)

outcome_specs_t2 <- tibble::tribble(
  ~outcome, ~pre_var,
  "meat_wrong_change_t2", "meat_wrong_pre_c",
  "charity_wrong_change_t2", "charity_wrong_pre_c",
  "charity_donation_cents_t2", "charity_wrong_pre_c",
  "voucher_is_veg_t2", "meat_wrong_pre_c"
)

assert_cols <- function(data, cols, label) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    stop(
      "Missing column(s) in ", label, ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

prep_moderation_sample <- function(data, time) {
  assert_cols(data, c("condition", "meat_wrong_pre", "charity_wrong_pre", "AOT_mean"), time)

  out <- data |>
    mutate(
      condition = factor(condition, levels = c("Charity", "Meat")),
      meat_wrong_pre_c = meat_wrong_pre - mean(meat_wrong_pre, na.rm = TRUE),
      charity_wrong_pre_c = charity_wrong_pre - mean(charity_wrong_pre, na.rm = TRUE)
    )

  if ("crt_batball_cor" %in% names(out)) {
    out <- out |> mutate(crt_num = as.numeric(crt_batball_cor))
  } else {
    out <- out |> mutate(crt_num = NA_real_)
  }

  if (time == "T2") {
    assert_cols(out, .required_t2_inputs, time)
    out <- out |>
      mutate(
        meat_wrong_change_t2 = meat_wrong_t2 - meat_wrong_pre,
        charity_wrong_change_t2 = charity_wrong_t2 - charity_wrong_pre
      )
    assert_cols(out, .required_t2_outcomes, time)
  } else {
    assert_cols(out, .required_t1_outcomes, time)
  }

  out
}

p_condition_moderator <- function(fit, mod_var) {
  co <- names(coef(fit))
  hits <- co[grepl("^condition", co) & grepl(paste0(":", mod_var, "$"), co)]
  if (length(hits) != 1L) {
    return(NA_real_)
  }
  sm <- summary(fit)$coefficients
  term <- hits[[1L]]
  if (!term %in% rownames(sm)) {
    return(NA_real_)
  }
  unname(sm[term, "Pr(>|t|)"])
}

fit_moderation <- function(data, outcome, pre_var, mod_var) {
  d <- data |>
    filter(
      !is.na(.data[[outcome]]),
      !is.na(.data[[pre_var]]),
      !is.na(.data[[mod_var]]),
      !is.na(condition)
    ) |>
    mutate(outcome_y = as.numeric(.data[[outcome]]))

  rhs <- paste("condition *", pre_var, "+ condition *", mod_var)
  f <- stats::as.formula(paste("outcome_y ~", rhs))
  lm_robust(f, data = d, se_type = "HC2")
}

collect_moderator_family <- function(d1_prep, d2_prep, mod_var, moderator_label) {
  rows <- list()
  specs_list <- list(T1 = outcome_specs_t1, T2 = outcome_specs_t2)
  dat_list <- list(T1 = d1_prep, T2 = d2_prep)

  for (time in c("T1", "T2")) {
    dat <- dat_list[[time]]
    sp <- specs_list[[time]]
    for (i in seq_len(nrow(sp))) {
      fit <- fit_moderation(
        dat, sp$outcome[[i]], sp$pre_var[[i]], mod_var
      )
      rows[[length(rows) + 1L]] <- tibble::tibble(
        moderator_family = moderator_label,
        mod_var = mod_var,
        time = time,
        outcome = sp$outcome[[i]],
        n = stats::nobs(fit),
        p = p_condition_moderator(fit, mod_var)
      )
    }
  }
  dplyr::bind_rows(rows)
}

apply_family_corrections <- function(tbl) {
  tbl |>
    mutate(
      p_holm = stats::p.adjust(p, method = "holm"),
      p_bh = stats::p.adjust(p, method = "BH"),
      sig_holm_05 = p_holm < 0.05,
      sig_bh_05 = p_bh < 0.05
    )
}

print_fit_block <- function(data, outcome, pre_var, mod_var, label) {
  fit <- fit_moderation(data, outcome, pre_var, mod_var)
  cat(
    "\n--- ", label, " | outcome: ", outcome,
    " (N = ", stats::nobs(fit), ") ---\n",
    sep = ""
  )
  print(summary(fit))
  invisible(fit)
}

print_family_table <- function(x) {
  cat("\n--- ", unique(x$moderator_family), " family ---\n", sep = "")
  disp <- x |>
    select(time, outcome, n, p, p_holm, p_bh, sig_holm_05, sig_bh_05) |>
    mutate(across(c(p, p_holm, p_bh), ~ round(.x, 4)))
  print(as.data.frame(disp), row.names = FALSE, right = FALSE)
  cat(
    "Counts significant at adjusted p < .05:  Holm = ",
    sum(x$sig_holm_05, na.rm = TRUE),
    ", BH = ",
    sum(x$sig_bh_05, na.rm = TRUE),
    "\n",
    sep = ""
  )
}

run_crt_aot_heterogeneity <- function(d1, d, verbose = TRUE) {
  d1_prep <- prep_moderation_sample(d1, "T1")
  d2_prep <- prep_moderation_sample(d, "T2")

  if (all(is.na(d1_prep$crt_num))) {
    stop("Column crt_batball_cor not found; cannot fit CRT moderation.", call. = FALSE)
  }

  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("STUDY 1 · EXPLORATORY HETEROGENEITY (AOT / CRT moderation)\n")
  cat(strrep("=", 70), "\n", sep = "")
  cat(
    "Model: outcome ~ condition * pretreatment (mean-centered) + ",
    "condition * moderator; HC2 robust SEs (estimatr::lm_robust).\n",
    sep = ""
  )
  cat("T1 N = ", nrow(d1_prep), "; T2 N = ", nrow(d2_prep), "\n", sep = "")

  if (verbose) {
    run_block <- function(time_label, data, specs, mod_var, label) {
      cat("\n", strrep("=", 70), "\n", sep = "")
      cat(time_label, " — ", label, "\n", sep = "")
      cat(strrep("=", 70), "\n", sep = "")
      for (i in seq_len(nrow(specs))) {
        print_fit_block(
          data,
          specs$outcome[[i]],
          specs$pre_var[[i]],
          mod_var,
          label
        )
      }
    }

    run_block(
      "T1 (post minus pre)", d1_prep, outcome_specs_t1, "AOT_mean",
      "AOT: condition * pre + condition * AOT"
    )
    run_block(
      "T1 (post minus pre)", d1_prep, outcome_specs_t1, "crt_num",
      "CRT: condition * pre + condition * CRT"
    )
    run_block(
      "T2 (follow-up minus pre)", d2_prep, outcome_specs_t2, "AOT_mean",
      "AOT: condition * pre + condition * AOT"
    )
    run_block(
      "T2 (follow-up minus pre)", d2_prep, outcome_specs_t2, "crt_num",
      "CRT: condition * pre + condition * CRT"
    )
  }

  family_aot <- collect_moderator_family(d1_prep, d2_prep, "AOT_mean", "AOT") |>
    apply_family_corrections()
  family_crt <- collect_moderator_family(d1_prep, d2_prep, "crt_num", "CRT") |>
    apply_family_corrections()

  results <- bind_rows(family_aot, family_crt)

  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("MULTIPLE COMPARISONS: condition × moderator interaction only\n")
  cat("Each family = 8 tests (4 outcomes × T1 and T2). Holm + Benjamini–Hochberg.\n")
  cat(strrep("=", 70), "\n", sep = "")

  print_family_table(family_aot)
  print_family_table(family_crt)

  if (verbose) {
    cat("\nDone (exploratory AOT / CRT heterogeneity).\n")
  }

  invisible(results)
}
