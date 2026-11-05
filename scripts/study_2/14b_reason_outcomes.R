# =============================================================================
# 14b · Reason codes and t1 outcomes
# =============================================================================
# Links endorsed reason codes to belief change and behaviour. Sourced by
# 14_reasons.R.

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(estimatr)
library(ggplot2)
library(ggtext)
library(cowplot)
library(grid)
library(here)

source(here("scripts", "functions", "helpers.R"))

options(width = max(getOption("width"), 220L))

class_dir <- here("data", "study_2", "text_ratings")

output_dir <- here("output", "study_2")
figure_dir <- here("output", "study_2", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Codebooks (column stem -> short label). Consensus columns are stem_consensus.
# ---------------------------------------------------------------------------

MEAT_CATS <- c(
  natural     = "Natural",
  necessary   = "Necessary",
  amoral      = "Amoral",
  conditional = "Conditional",
  normal      = "Normal",
  nice        = "Nice",
  religious   = "Religious"
)

CHARITY_CATS <- c(
  financial_constraints = "Financial",
  supererogatory        = "Supererogatory",
  skepticism            = "Skepticism",
  relativism            = "Relativism",
  property_rights       = "Property Rights",
  systemic              = "Systemic",
  limited_scope         = "Limited Scope"
)

LYING_CATS <- c(
  harm                 = "Harm",
  absolute_prohibition = "Absolute Prohibition",
  unfair_advantage     = "Unfair Advantage",
  moral_character      = "Moral Character",
  broken_trust         = "Broken Trust",
  religious_duty       = "Religious Duty"
)

POLVIOL_CATS <- c(
  absolute_prohibition   = "Absolute Prohibition",
  ineffectiveness        = "Ineffective",
  unnecessary_force      = "Unnecessary Force",
  illegitimate_authority = "Illegitimate Authority",
  moral_character        = "Moral Character",
  harm                   = "Harm"
)

# One-line glosses, same wording as the prevalence figure.
GLOSS <- list(
  meat = c(
    Natural     = "Evolved to eat meat",
    Necessary   = "Required for nutrition",
    Amoral      = "Not a moral issue",
    Conditional = "OK if ethically sourced",
    Normal      = "Socially acceptable",
    Nice        = "Tastes good",
    Religious   = "God permits it"
  ),
  charity = c(
    Financial         = "Can't afford to give",
    Supererogatory    = "Nice but not required",
    Skepticism        = "Charity doesn't work",
    Relativism        = "No objective duty",
    "Property Rights" = "It's my money",
    Systemic          = "Need structural change",
    "Limited Scope"   = "Obligations are local"
  ),
  lying = c(
    Harm                   = "Causes harm",
    "Absolute Prohibition" = "Deception always wrong",
    "Unfair Advantage"     = "Unearned or stolen gain",
    "Moral Character"      = "Lying shows bad character",
    "Broken Trust"         = "Undermines cooperation",
    "Religious Duty"       = "God forbids it"
  ),
  polviol = c(
    "Absolute Prohibition"  = "Violence is always wrong",
    Ineffective             = "Doesn't achieve goals",
    "Unnecessary Force"     = "Other methods exist",
    "Illegitimate Authority" = "No right to use force",
    "Moral Character"       = "Violence shows bad character",
    Harm                    = "Causes harm"
  )
)

specs <- list(
  list(
    key = "meat", file = "classifications_meat.csv", cats = MEAT_CATS,
    sample = "meat arm, pretreatment belief <= 50",
    belief = "belief_meat_change", pre = "belief_meat_pre",
    behavior = "voucher_is_veg", behavior_lab = "Veg voucher (0/1)",
    belief_note = "positive change = persuaded toward 'eating meat is wrong'"
  ),
  list(
    key = "charity", file = "classifications_charity.csv", cats = CHARITY_CATS,
    sample = "charity arm, pretreatment belief <= 50",
    belief = "belief_charity_change", pre = "belief_charity_pre",
    behavior = "donation_cents", behavior_lab = "Donation (cents)",
    belief_note = "positive change = persuaded toward 'not giving is wrong'"
  ),
  list(
    key = "lying", file = "classifications_lying.csv", cats = LYING_CATS,
    sample = "lying arm, pretreatment belief >= 50",
    belief = "belief_lying_change", pre = "belief_lying_pre",
    behavior = "coin_heads", behavior_lab = "Coin heads / lied (0/1)",
    belief_note = "negative change = persuaded against 'deceiving for gain is wrong'"
  ),
  list(
    key = "polviol", file = "classifications_polviol.csv", cats = POLVIOL_CATS,
    sample = "political-violence arm, pretreatment belief >= 50",
    belief = "belief_polviol_change", pre = "belief_polviol_pre",
    behavior = "pv_participation", behavior_lab = "Would join crowd (0-100)",
    belief_note = "negative change = persuaded against 'political violence is wrong'"
  )
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

as_logical_flag <- function(x) {
  if (is.logical(x)) return(x)
  tolower(as.character(x)) %in% c("true", "t", "1")
}

read_codes <- function(path, cats) {
  df <- read_csv(path, show_col_types = FALSE)
  cons <- paste0(names(cats), "_consensus")
  missing <- setdiff(cons, names(df))
  if (length(missing) > 0) {
    stop("Missing consensus columns in ", basename(path), ": ",
         paste(missing, collapse = ", "))
  }
  df |>
    transmute(
      participantId = as.character(id),
      across(all_of(cons), as_logical_flag)
    ) |>
    mutate(across(all_of(cons), ~ as.integer(.x %in% TRUE)))
}

# Bivariate: outcome ~ code. Difference of means equals the OLS coefficient.
bivariate_rows <- function(data, outcome, cats) {
  y <- data[[outcome]]
  map_dfr(names(cats), function(stem) {
    x <- data[[paste0(stem, "_consensus")]]
    ok <- !is.na(x) & !is.na(y)
    xx <- x[ok]
    yy <- as.numeric(y[ok])
    n1 <- sum(xx == 1)
    n0 <- sum(xx == 0)
    m1 <- if (n1 > 0) mean(yy[xx == 1]) else NA_real_
    m0 <- if (n0 > 0) mean(yy[xx == 0]) else NA_real_
    ct <- if (n1 > 1 && n0 > 1) cor.test(xx, yy) else NULL
    tibble(
      code = unname(cats[[stem]]),
      n_code = n1,
      pct = round(100 * n1 / sum(ok), 1),
      mean_with = m1,
      mean_without = m0,
      diff = m1 - m0,
      r = if (is.null(ct)) NA_real_ else unname(ct$estimate),
      p = if (is.null(ct)) NA_real_ else ct$p.value,
      n = sum(ok)
    )
  })
}

print_bivariate <- function(tab, outcome_lab) {
  cat("\n", outcome_lab, " — each code alone\n", sep = "")
  cat("diff = mean(with code) − mean(without). r = Pearson correlation.\n\n")
  shown <- tab |>
    mutate(
      mean_with = round(mean_with, 2),
      mean_without = round(mean_without, 2),
      diff = round(diff, 2),
      r = round(r, 3),
      p_fmt = fmt_p(p),
      sig = fmt_sig(p)
    ) |>
    select(code, n_code, pct, mean_with, mean_without, diff, r, p_fmt, sig)
  print(as.data.frame(shown), row.names = FALSE, right = FALSE)
}

fit_joint <- function(data, outcome, cats) {
  code_vars <- paste0(names(cats), "_consensus")
  d <- data |>
    filter(
      !is.na(.data[[outcome]]),
      !is.na(pre_belief),
      !is.na(order),
      if_all(all_of(code_vars), ~ !is.na(.x))
    ) |>
    mutate(
      y = as.numeric(.data[[outcome]]),
      pre_belief_c = pre_belief - mean(pre_belief),
      order = droplevels(factor(order))
    )

  varying <- code_vars[vapply(code_vars, function(v) {
    length(unique(d[[v]])) > 1
  }, logical(1))]
  dropped <- setdiff(code_vars, varying)
  if (length(dropped) > 0) {
    cat("Dropped (no variation): ",
        paste(dropped, collapse = ", "), "\n", sep = "")
  }

  f <- reformulate(c(varying, "pre_belief_c", "order"), response = "y")
  m <- lm_robust(f, data = d, se_type = "HC2")
  co <- as.data.frame(summary(m)$coefficients)
  co$term <- rownames(co)
  rownames(co) <- NULL

  list(model = m, coefs = co, n = nrow(d), dropped = dropped, data = d)
}

print_joint <- function(fit, cats, outcome_lab) {
  code_vars <- paste0(names(cats), "_consensus")
  labels <- c(
    stats::setNames(unname(cats), code_vars),
    pre_belief_c = "Pretreatment belief (centered)"
  )
  tab <- fit$coefs |>
    filter(term %in% names(labels)) |>
    mutate(
      code = unname(labels[term]),
      est = round(Estimate, 3),
      se = round(`Std. Error`, 3),
      p_fmt = fmt_p(`Pr(>|t|)`),
      sig = fmt_sig(`Pr(>|t|)`),
      CI = sprintf("[%.2f, %.2f]", `CI Lower`, `CI Upper`)
    ) |>
    select(code, est, se, p_fmt, sig, CI)

  cat("\n", outcome_lab, " — all codes together\n", sep = "")
  cat(sprintf(
    "N = %d. R^2 = %.3f. HC2. Covariates: centered pretreatment belief, order.\n",
    fit$n, fit$model$r.squared
  ))
  cat("Coefficient is the partial difference associated with that code.\n\n")
  print(as.data.frame(tab), row.names = FALSE, right = FALSE)
}

# Design matrix and HC2 covariance for linear combinations of the joint model.
design_of <- function(fit) {
  m <- fit$model
  b <- coef(m)
  keep <- !is.na(b)
  b <- b[keep]
  list(
    b = b,
    V = vcov(m)[names(b), names(b), drop = FALSE],
    X = stats::model.matrix(stats::formula(m), data = fit$data)[, names(b), drop = FALSE],
    d = fit$data,
    df = m$df.residual
  )
}

# Average fitted belief change for a row mask. xbar is the mean design row,
# so contrasts between groups use the same covariance matrix.
summarise_rows <- function(des, rows) {
  xbar <- colMeans(des$X[rows, , drop = FALSE])
  est <- sum(xbar * des$b)
  se <- sqrt(as.numeric(t(xbar) %*% des$V %*% xbar))
  t_stat <- est / se
  crit <- stats::qt(0.975, des$df)
  list(
    xbar = xbar,
    tab = tibble(
      n = sum(rows),
      estimate = est,
      se = se,
      t = t_stat,
      df = des$df,
      p = 2 * stats::pt(-abs(t_stat), des$df),
      ci_lo = est - crit * se,
      ci_hi = est + crit * se
    )
  )
}

contrast_p <- function(des, x_yes, x_no) {
  delta <- x_yes - x_no
  est <- sum(delta * des$b)
  se <- sqrt(as.numeric(t(delta) %*% des$V %*% delta))
  2 * stats::pt(-abs(est / se), des$df)
}

# code_var = NULL is the all-zero group. present = FALSE is everyone
# without that one code.
fitted_group_mean <- function(fit, code_var = NULL, present = TRUE) {
  des <- design_of(fit)
  if (is.null(code_var)) {
    code_vars <- grep("_consensus$", names(des$d), value = TRUE)
    rows <- rowSums(des$d[code_vars]) == 0
  } else if (present) {
    rows <- des$d[[code_var]] == 1
  } else {
    rows <- des$d[[code_var]] == 0
  }
  summarise_rows(des, rows)$tab
}

level_rows <- function(fit, cats) {
  coded <- map_dfr(names(cats), function(stem) {
    fitted_group_mean(fit, paste0(stem, "_consensus")) |>
      mutate(code = unname(cats[[stem]]))
  })
  none <- fitted_group_mean(fit, NULL) |>
    mutate(code = "No code")
  bind_rows(coded, none)
}

# Yes / no for each code, then the all-zero group as its own row.
# p_diff tests the yes mean against the no mean in this model.
yn_rows <- function(fit, cats) {
  des <- design_of(fit)
  coded <- map_dfr(names(cats), function(stem) {
    var <- paste0(stem, "_consensus")
    yes <- summarise_rows(des, des$d[[var]] == 1)
    no <- summarise_rows(des, des$d[[var]] == 0)
    bind_rows(
      yes$tab |> mutate(group = "Yes"),
      no$tab |> mutate(group = "No")
    ) |>
      mutate(
        code = unname(cats[[stem]]),
        sort = match(stem, names(cats)),
        p_diff = contrast_p(des, yes$xbar, no$xbar)
      )
  })
  code_vars <- paste0(names(cats), "_consensus")
  none <- summarise_rows(des, rowSums(des$d[code_vars]) == 0)
  none <- none$tab |>
    mutate(
      group = "No coded reason",
      code = "No coded reason",
      sort = length(cats) + 1L,
      p_diff = NA_real_
    )
  bind_rows(coded, none)
}

print_levels <- function(tab) {
  cat("\nBelief change — average fitted value for people with each code\n")
  cat("From the joint model, averaged over their other codes, pretest, and order.\n")
  cat("Test is two-sided against 0. CI above 0 means the average change is positive.\n\n")
  shown <- tab |>
    mutate(
      mean = round(estimate, 2),
      se = round(se, 2),
      CI = sprintf("[%.2f, %.2f]", ci_lo, ci_hi),
      p_fmt = fmt_p(p),
      sig = fmt_sig(p),
      above_0 = if_else(ci_lo > 0, "yes", if_else(ci_hi < 0, "no", ""))
    ) |>
    select(code, n, mean, se, CI, p_fmt, sig, above_0)
  print(as.data.frame(shown), row.names = FALSE, right = FALSE)
}

coef_frame <- function(fit, spec, outcome_name) {
  fit$coefs |>
    transmute(
      issue = spec$key,
      outcome = outcome_name,
      term,
      estimate = Estimate,
      se = `Std. Error`,
      t = `t value`,
      p = `Pr(>|t|)`,
      ci_lo = `CI Lower`,
      ci_hi = `CI Upper`,
      n = fit$n,
      r_squared = fit$model$r.squared
    )
}

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

d1 <- readRDS(here("data", "study_2", "study2_d1.rds")) |>
  mutate(
    participantId = as.character(participantId),
    voucher_is_veg = as.integer(
      coalesce(voucher_DV_beeffirst, voucher_DV_vegfirst) == 1
    ),
    coin_heads = as.integer(lying_DV == 1),
    order = factor(order)
  )

section("REASON CODES AND t1 OUTCOMES")
cat(paste(
  "Reasons are written after the pretreatment slider and before the chat,",
  "the post slider, and the behaviours, so the outcomes do not cause the text.",
  "The joint model holds pretreatment belief and task order fixed, which takes",
  "out the part of the association that is just starting position.",
  "The coefficient still mixes who states the reason with how the model replies",
  "to it: the chat is shown the reason. Control wrote about summer versus winter,",
  "so these codebooks have no untreated arm.",
  sep = "\n"
), "\n")

bivariate_all <- list()
joint_all <- list()
level_all <- list()
yn_all <- list()

issue_labels <- c(
  meat    = "Meat",
  charity = "Charity",
  lying   = "Deceiving others",
  polviol = "Political violence"
)

statement_labels <- c(
  meat    = "Eating meat is\nmorally wrong",
  charity = "Not giving to charity\nis morally wrong",
  lying   = "Deceiving others for financial\ngain is morally wrong",
  polviol = "Using violence to advance\npolitical goals is morally wrong"
)

for (spec in specs) {
  codes <- read_codes(file.path(class_dir, spec$file), spec$cats)
  merged <- codes |>
    inner_join(
      d1 |>
        select(
          participantId, order,
          all_of(c(spec$belief, spec$pre, spec$behavior))
        ) |>
        rename(pre_belief = all_of(spec$pre)),
      by = "participantId"
    )

  n_unmatched <- nrow(codes) - nrow(merged)
  if (n_unmatched > 0) {
    warning(spec$key, ": ", n_unmatched, " classified ids missing from d1")
  }

  section(sprintf(
    "%s  (N classified = %d, matched = %d)",
    toupper(spec$key), nrow(codes), nrow(merged)
  ))
  cat(spec$sample, "\n", sep = "")
  cat("Belief change: ", spec$belief_note, "\n", sep = "")
  cat("Behaviour: ", spec$behavior_lab, "\n", sep = "")

  n_codes <- rowSums(merged[paste0(names(spec$cats), "_consensus")])
  cat(sprintf(
    "Codes per text: mean %.2f, share with 2+ codes %.1f%%, uncoded %.1f%%\n",
    mean(n_codes),
    100 * mean(n_codes >= 2),
    100 * mean(n_codes == 0)
  ))

  b_bel <- bivariate_rows(merged, spec$belief, spec$cats) |>
    mutate(issue = spec$key, outcome = "belief_change")
  b_beh <- bivariate_rows(merged, spec$behavior, spec$cats) |>
    mutate(issue = spec$key, outcome = "behavior")
  bivariate_all[[spec$key]] <- bind_rows(b_bel, b_beh)

  print_bivariate(b_bel, "Belief change (points)")
  print_bivariate(b_beh, spec$behavior_lab)

  fit_bel <- fit_joint(merged, spec$belief, spec$cats)
  fit_beh <- fit_joint(merged, spec$behavior, spec$cats)
  print_joint(fit_bel, spec$cats, "Belief change (points)")
  print_joint(fit_beh, spec$cats, spec$behavior_lab)

  levels <- level_rows(fit_bel, spec$cats) |>
    mutate(issue = spec$key)
  print_levels(levels)
  level_all[[spec$key]] <- levels

  # Shares match the prevalence figure: majority-vote codes over
  # every classified text in the arm, not only complete outcome cases.
  code_vars <- paste0(names(spec$cats), "_consensus")
  n_texts <- nrow(codes)
  n_by_code <- vapply(
    code_vars,
    function(v) sum(codes[[v]] == 1, na.rm = TRUE),
    numeric(1)
  )
  names(n_by_code) <- unname(spec$cats)
  n_by_code <- c(
    n_by_code,
    "No coded reason" = sum(rowSums(codes[code_vars]) == 0)
  )

  yn_all[[spec$key]] <- yn_rows(fit_bel, spec$cats) |>
    mutate(
      issue = spec$key,
      issue_lab = unname(issue_labels[[spec$key]]),
      statement = unname(statement_labels[[spec$key]]),
      n_arm = fit_bel$n,
      n_texts = n_texts,
      pct = unname(round(100 * n_by_code[code] / n_texts, 1)),
      gloss = unname(GLOSS[[spec$key]][code])
    )

  joint_all[[spec$key]] <- bind_rows(
    coef_frame(fit_bel, spec, "belief_change"),
    coef_frame(fit_beh, spec, "behavior")
  )
}

bivariate_out <- bind_rows(bivariate_all)
joint_out <- bind_rows(joint_all)
level_out <- bind_rows(level_all)
yn_out <- bind_rows(yn_all)

bivariate_path <- file.path(output_dir, "reasons_outcome_bivariate.csv")
joint_path <- file.path(output_dir, "reasons_outcome_models.csv")
level_path <- file.path(output_dir, "reasons_belief_change_levels.csv")
write_csv(bivariate_out, bivariate_path)
write_csv(joint_out, joint_path)
write_csv(level_out, level_path)

# ---------------------------------------------------------------------------
# Belief change with prevalence. Reasons are ordered by how common they
# were, most common at the top. "No coded reason" stays at the bottom.
# Two layouts: the share in the row label ("Harm (37.5%)"), or a bar for
# that share on the right of the estimates.
# ---------------------------------------------------------------------------

# Reason figures leave non-significant cells blank (no "n.s.").
fmt_bracket_sig_blank <- function(p) fmt_bracket_sig(p, ns = "")

axis_label_html <- function(label, desc, pct = NULL) {
  desc <- ifelse(is.na(desc), "", desc)
  pct_txt <- if (is.null(pct)) {
    ""
  } else {
    ifelse(
      is.na(pct),
      "",
      paste0(
        " <span style='color:#737373;'>(",
        sprintf("%.1f%%", pct),
        ")</span>"
      )
    )
  }
  name <- paste0("<b>", label, "</b>", pct_txt)
  ifelse(
    !nzchar(desc),
    paste0("<p style='text-align:right;margin:0'>", name, "</p>"),
    paste0(
      "<p style='text-align:right;margin:0'>",
      name, "<br>",
      "<span style='font-size:6.5pt;color:#737373;'>", desc, "</span>",
      "</p>"
    )
  )
}

# ggplot2 4 puts a 1null spacer in the y-axis so leftover width opens a
# gap between the labels and the bars. Zero those nulls.
zero_null_units <- function(u) {
  n <- length(u)
  out <- u
  for (i in seq_len(n)) {
    ti <- grid::unitType(u[i])
    if (is.list(ti)) ti <- ti[[1]]
    if (identical(as.character(ti), "null")) out[i] <- unit(0, "cm")
  }
  out
}

tighten_y_axis <- function(p) {
  gt <- if (inherits(p, "ggplot")) ggplotGrob(p) else p
  idx <- which(gt$layout$name == "axis-l")
  if (length(idx) != 1L) return(gt)
  axis <- gt$grobs[[idx]]
  inner <- axis$children$axis
  if (!inherits(inner, "gtable")) return(gt)
  inner$widths <- zero_null_units(inner$widths)
  axis$children$axis <- inner
  axis$width <- sum(inner$widths)
  gt$grobs[[idx]] <- axis
  gt$widths[gt$layout$l[idx]] <- axis$width
  gt
}

# Most common at the top. Ties keep codebook order. The residual row is
# pinned to the bottom, which is the first discrete limit.
# n_slots pads with blank rows between the reasons and "No coded reason", so
# panels with fewer reasons keep the same row spacing when aligned.
reason_layout <- function(d, with_pct = FALSE, n_slots = NULL) {
  reason_order <- d |>
    filter(group == "Yes") |>
    distinct(code, pct, sort) |>
    arrange(desc(pct), sort) |>
    pull(code)
  n_pad <- if (is.null(n_slots)) 0 else max(0, n_slots - length(reason_order))
  pad_levels <- if (n_pad > 0) paste0(".pad", seq_len(n_pad)) else character(0)
  y_limits <- c("No coded reason", pad_levels, rev(reason_order))
  d <- d |> mutate(code = factor(code, levels = y_limits))

  label_rows <- d |>
    distinct(code, gloss, pct) |>
    mutate(
      html = axis_label_html(
        as.character(code),
        coalesce(gloss, ""),
        pct = if (with_pct) pct else NA_real_
      )
    )
  list(
    d = d,
    y_limits = y_limits,
    label_map = c(
      setNames(label_rows$html, as.character(label_rows$code)),
      setNames(rep("", length(pad_levels)), pad_levels)
    )
  )
}

make_forest_plot <- function(layout, x_lab = NULL, margin_r = 6,
                             axis_line_y = FALSE, dashed_zero = FALSE) {
  d <- layout$d
  yn <- d |>
    filter(group %in% c("Yes", "No")) |>
    mutate(group = factor(group, levels = c("Yes", "No")))
  none <- d |> filter(group == "No coded reason")

  # Yes is dodged below the row center, No above. width 0.55 -> ±0.1375.
  dodge <- 0.1375
  brackets <- yn |>
    group_by(code) |>
    summarise(
      y = as.numeric(code)[1],
      x_right = max(ci_hi),
      p_diff = p_diff[1],
      .groups = "drop"
    ) |>
    filter(!is.na(p_diff), p_diff < 0.05) |>
    mutate(stars = fmt_bracket_sig_blank(p_diff))

  span <- diff(range(c(d$ci_lo, d$ci_hi, 0), na.rm = TRUE))
  pad <- 0.04 * span
  tick <- 0.018 * span
  if (nrow(brackets) > 0) {
    brackets <- brackets |>
      mutate(
        x_tick = x_right + pad,
        x_bar = x_right + pad + tick,
        x_txt = x_bar + 0.35 * pad,
        y_yes = y - dodge,
        y_no = y + dodge
      )
    x_hi <- max(c(d$ci_hi, brackets$x_txt), na.rm = TRUE) + 1.2 * pad
  } else {
    x_hi <- max(c(d$ci_hi, 0), na.rm = TRUE) + pad
  }
  x_lo <- min(c(d$ci_lo, 0), na.rm = TRUE) - pad

  p <- ggplot() +
    (if (!dashed_zero) {
      geom_vline(xintercept = 0, color = "grey55", linewidth = 0.3)
    } else if (x_lo < 0) {
      geom_vline(xintercept = 0, color = "grey45", linewidth = 0.3,
                 linetype = "dashed")
    }) +
    geom_hline(yintercept = 1.5, color = "grey85", linewidth = 0.3) +
    geom_pointrange(
      data = yn,
      aes(
        x = estimate, xmin = ci_lo, xmax = ci_hi, y = code,
        color = group
      ),
      position = position_dodge(width = 0.55),
      linewidth = 0.4,
      size = 0.55
    ) +
    geom_pointrange(
      data = none,
      aes(x = estimate, xmin = ci_lo, xmax = ci_hi, y = code),
      color = "#1a1a1a",
      shape = 18,
      linewidth = 0.4,
      size = 0.7,
      show.legend = FALSE
    )

  if (nrow(brackets) > 0) {
    p <- p +
      geom_segment(
        data = brackets,
        aes(x = x_tick, xend = x_bar, y = y_yes, yend = y_yes),
        linewidth = 0.3, color = "black", inherit.aes = FALSE
      ) +
      geom_segment(
        data = brackets,
        aes(x = x_tick, xend = x_bar, y = y_no, yend = y_no),
        linewidth = 0.3, color = "black", inherit.aes = FALSE
      ) +
      geom_segment(
        data = brackets,
        aes(x = x_bar, xend = x_bar, y = y_yes, yend = y_no),
        linewidth = 0.3, color = "black", inherit.aes = FALSE
      ) +
      geom_text(
        data = brackets,
        aes(x = x_txt, y = y, label = stars),
        size = 3.2, hjust = 0, vjust = 0.75, inherit.aes = FALSE
      )
  }

  p +
    scale_color_manual(
      values = c(Yes = "#1a1a1a", No = "#A3A3A3"),
      labels = c(Yes = "Reason present", No = "Reason absent"),
      name = NULL
    ) +
    scale_y_discrete(
      limits = layout$y_limits,
      drop = FALSE,
      expand = expansion(add = 0.6),
      labels = layout$label_map
    ) +
    scale_x_continuous(limits = c(x_lo, x_hi)) +
    labs(x = x_lab, y = NULL) +
    # theme_bw rather than theme_minimal: ggplot2 4 draws element_markdown
    # as literal HTML under theme_minimal.
    theme_bw(base_size = 9, base_family = "Helvetica") +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      axis.line.x = element_line(color = "grey40", linewidth = 0.3),
      axis.line.y = if (axis_line_y) {
        element_line(color = "grey40", linewidth = 0.3)
      } else {
        element_blank()
      },
      axis.ticks.y = element_blank(),
      axis.text.y = element_markdown(
        size = 7, lineheight = 0.95,
        hjust = 1, halign = 1,
        colour = "grey10",
        margin = margin(r = 4)
      ),
      axis.text.x = element_text(color = "grey20", size = 7),
      axis.title.x = element_text(size = 8, color = "grey10", margin = margin(t = 3)),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      plot.margin = margin(3, margin_r, 2, 2)
    )
}

# Paper figure, in the style of the reason-prevalence figure from
# 07_reasons_analysis.R: a centered title and sample line over each panel,
# reason names in bold with a gray gloss, and each reason's share beside its
# name. Rebuilt after switching to Quartz text metrics so the label widths
# measured by tighten_y_axis() match the PDF.
paper_titles <- c(
  meat    = "Reasons for Acceptability of Meat-eating",
  charity = "Reasons Against Charitable Obligation",
  lying   = "Reasons Why Lying for Financial Gain Is Wrong",
  polviol = "Reasons Why Political Violence Is Wrong"
)
paper_samples <- c(
  meat    = "Skeptics (pre \u2264 50)",
  charity = "Skeptics (pre \u2264 50)",
  lying   = "Believers (pre \u2265 50)",
  polviol = "Believers (pre \u2265 50)"
)

paper_panel_header <- function(title, subtitle) {
  ggdraw() +
    draw_label(title, fontface = "bold", fontfamily = "Helvetica", size = 8,
               x = 0.5, y = 0.7, hjust = 0.5, vjust = 0.5) +
    draw_label(subtitle, fontfamily = "Helvetica", size = 7, colour = "grey30",
               x = 0.5, y = 0.2, hjust = 0.5, vjust = 0.5)
}

# Built like fig_2x2 in 07_reasons_analysis.R: the four forest plots are
# padded to the same number of rows, aligned on both axes, and only then
# given their headers, so plotting regions and label columns line up.
build_paper_figure <- function(dat, x_lab) {
  panel_keys <- c("meat", "charity", "lying", "polviol")
  n_slots <- dat |>
    filter(group == "Yes") |>
    count(issue) |>
    pull(n) |>
    max()
  forests <- lapply(panel_keys, function(key) {
    layout <- reason_layout(dat |> filter(issue == key), with_pct = TRUE,
                            n_slots = n_slots)
    p <- make_forest_plot(
      layout,
      x_lab = if (key %in% c("lying", "polviol")) x_lab else " ",
      margin_r = 6, axis_line_y = TRUE, dashed_zero = TRUE
    ) +
      theme(legend.position = "none")
    tighten_y_axis(p)
  })
  # cowplot:: so a prior library(patchwork) cannot mask this with patchwork::align_plots.
  aligned <- do.call(cowplot::align_plots, c(forests, list(align = "hv", axis = "tblr")))
  panels <- lapply(seq_along(panel_keys), function(i) {
    key <- panel_keys[i]
    n_texts <- unique(dat$n_texts[dat$issue == key])
    plot_grid(
      paper_panel_header(
        paper_titles[[key]],
        sprintf("%s, N = %d", paper_samples[[key]], n_texts)
      ),
      aligned[[i]],
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
  })
  legend <- get_legend(
    make_forest_plot(
      reason_layout(dat |> filter(issue == "meat"), with_pct = TRUE)
    ) +
      theme(legend.position = "bottom", legend.text = element_text(size = 7))
  )
  fig <- do.call(
    plot_grid,
    c(list(plotlist = panels, ncol = 2), list(labels = c("A", "B", "C", "D")), PAPER_LABEL_ARGS)
  )
  plot_grid(fig, legend, ncol = 1, rel_heights = c(1, 0.05))
}

use_quartz_text_metrics()
fig_path <- save_paper_pdf(
  build_paper_figure(yn_out, "Belief change (post \u2212 pre)"),
  "fig-reason-belief-change.pdf",
  width = 7.08, height = 5.7, output_dir = figure_dir
)

section("YES VERSUS NO")
yn_out |>
  filter(group == "Yes") |>
  mutate(p_fmt = fmt_p(p_diff), sig = fmt_bracket_sig_blank(p_diff)) |>
  select(issue, code, p_fmt, sig) |>
  print(n = 40)

section("SAVED")
cat(
  bivariate_path, "\n", joint_path, "\n", level_path, "\n",
  fig_path, "\n",
  sep = ""
)
