# =============================================================================
# 08 · Pro vs anti persuasion (own-domain belief effects)
# =============================================================================
# Compares the four own-domain belief effects. Pro domains are meat and
# charity, where the intended movement is toward "this is morally wrong."
# Anti domains are deceiving others and political violence, where the
# intended movement is toward "this is less morally wrong."
#
# Each effect is the own-arm versus control coefficient from the
# preregistered model in 04_main_analyses.R:
#   outcome ~ condition * centered pretreatment belief + order
# The pretreatment belief stays on its original 0–100 scale and is centered
# at that item's sample mean, so the coefficient is the effect at the mean
# of the original item. Lying and political-violence change scores are
# multiplied by −1 so every coefficient is movement in the persuaded
# direction. Reverse-coding the pretreatment belief before centering leaves
# this point, and these coefficients, unchanged.
#
# The four outcomes are stacked, with every term interacted with domain, so
# the own-domain coefficients match the separate regressions. Standard
# errors are CR2, clustered by participant. Clustering carries the
# dependence from the shared control group and from the same person
# contributing all four beliefs into the contrasts below.
#
# Contrasts, on the persuasion-aligned scale:
#   Anti average minus pro average:
#     (lying + political violence) / 2 − (meat + charity) / 2
#   Lying vs meat, lying vs charity,
#   political violence vs meat, political violence vs charity.
# A positive contrast means the anti-domain effect is larger.
#
# Cohen's d divides each persuasion-aligned change by the SD of that item's
# pretreatment belief on the full baseline sample, condition ignored. The SD
# is fixed, and it is the same denominator as the main belief models.
#
# Prints both scales and writes:
#   output/study_2/tables/pro-v-anti-persuasion.csv
#   output/study_2/tables/pro-v-anti-persuasion.tex
# The .tex table is the slider-point comparison reported in the paper.

library(dplyr)
library(estimatr)
library(here)

source(here("scripts", "functions", "helpers.R"))
source(here("scripts", "functions", "study2_outcomes.R"))

options(width = max(getOption("width"), 220L))

d1 <- readRDS(here("data", "study_2", "study2_d1.rds")) |>
  mutate(
    condition = factor(
      condition,
      levels = c("control", "meat", "charity", "polviol", "lying")
    ),
    order = factor(order)
  )

stopifnot(!anyNA(d1$participant_id))

cat("Analysis sample (d1 completers): N =", nrow(d1), "\n")
print(table(d1$condition))

# flip / group live in study2_outcomes.R with the shared belief columns.
belief_defs <- study2_belief_defs_pro_anti()
print(as.data.frame(belief_defs), row.names = FALSE)

# One row per person per domain. flip applies only to the change score.
long <- bind_rows(lapply(seq_len(nrow(belief_defs)), function(i) {
  ch <- belief_defs$change[i]
  pre <- belief_defs$pre[i]
  d1 |>
    filter(
      !is.na(.data[[ch]]), !is.na(.data[[pre]]),
      !is.na(condition), !is.na(order)
    ) |>
    transmute(
      participant_id,
      domain = belief_defs$domain[i],
      condition,
      order,
      y = belief_defs$flip[i] * .data[[ch]],
      pre = .data[[pre]]
    )
})) |>
  group_by(domain) |>
  mutate(
    pre_c = pre - mean(pre),
    sd_pre = sd(pre),
    y_d = y / sd_pre[1]
  ) |>
  ungroup() |>
  mutate(domain = factor(domain, levels = belief_defs$domain))

fit_stack <- function(data, outcome) {
  dat <- data
  dat$outcome <- dat[[outcome]]
  lm_robust(
    outcome ~ 0 + domain + domain:condition + domain:pre_c +
      domain:condition:pre_c + domain:order,
    data = dat,
    clusters = participant_id,
    se_type = "CR2"
  )
}

own_term <- function(model, domain) {
  candidates <- c(
    paste0("domain", domain, ":condition", domain),
    paste0("condition", domain, ":domain", domain)
  )
  hit <- candidates[candidates %in% names(coef(model))]
  if (length(hit) != 1L) {
    stop(
      "Could not find the own-domain coefficient for ", domain,
      ".\nCoefficients:\n", paste(names(coef(model)), collapse = "\n")
    )
  }
  hit
}

# Linear combinations of the four own-domain coefficients.
contrast_defs <- tibble::tribble(
  ~contrast,                              ~meat, ~charity, ~lying, ~polviol,
  "Anti average minus pro average",       -0.5,  -0.5,     0.5,    0.5,
  "Lying vs meat",                        -1,     0,       1,      0,
  "Lying vs charity",                      0,    -1,       1,      0,
  "Political violence vs meat",           -1,     0,       0,      1,
  "Political violence vs charity",         0,    -1,       0,      1
)

report_scale <- function(model, scale_label, digits) {
  domains <- belief_defs$domain
  terms <- vapply(domains, function(dom) own_term(model, dom), character(1))
  b <- coef(model)[terms]
  V <- vcov(model)[terms, terms]
  df <- model$df.residual
  crit <- qt(0.975, df)

  pre_ref <- long |>
    group_by(domain) |>
    summarise(mean_pre = mean(pre), .groups = "drop")

  own <- belief_defs |>
    left_join(pre_ref, by = "domain") |>
    mutate(
      estimate = as.numeric(b),
      SE = sqrt(diag(V)),
      t = estimate / SE,
      p = 2 * pt(-abs(t), df),
      ci_lo = estimate - crit * SE,
      ci_hi = estimate + crit * SE,
      scale = scale_label
    )

  section(paste("OWN-DOMAIN EFFECTS —", scale_label))
  cat(
    "Effect at the mean of the original pretreatment item.",
    "Lying and political violence are persuasion-aligned (change × −1).\n"
  )
  own_print <- own |>
    transmute(
      domain = label,
      group,
      mean_pre = round(mean_pre, 2),
      estimate = round(estimate, digits),
      SE = round(SE, digits),
      t = round(t, 2),
      p = fmt_p(p),
      CI = sprintf(
        "[%s, %s]",
        formatC(ci_lo, digits, format = "f"),
        formatC(ci_hi, digits, format = "f")
      )
    )
  print(as.data.frame(own_print), row.names = FALSE)

  contrasts <- bind_rows(lapply(seq_len(nrow(contrast_defs)), function(i) {
    L <- as.numeric(contrast_defs[i, domains])
    est <- sum(L * b)
    se <- sqrt(as.numeric(t(L) %*% V %*% L))
    t_stat <- est / se
    p <- 2 * pt(-abs(t_stat), df)
    tibble(
      contrast = contrast_defs$contrast[i],
      scale = scale_label,
      estimate = est,
      SE = se,
      t = t_stat,
      df = df,
      p = p,
      ci_lo = est - crit * se,
      ci_hi = est + crit * se
    )
  }))

  section(paste("PRO VS ANTI CONTRASTS —", scale_label))
  cat("Positive means the anti-domain effect is larger than the pro-domain effect.\n")
  contrast_print <- contrasts |>
    transmute(
      contrast,
      estimate = round(estimate, digits),
      SE = round(SE, digits),
      t = round(t, 2),
      p = fmt_p(p),
      CI = sprintf(
        "[%s, %s]",
        formatC(ci_lo, digits, format = "f"),
        formatC(ci_hi, digits, format = "f")
      )
    )
  print(as.data.frame(contrast_print), row.names = FALSE)

  bind_rows(
    own |>
      transmute(
        result = "own-domain",
        contrast = label,
        group,
        scale,
        estimate, SE, t, df, p, ci_lo, ci_hi
      ),
    contrasts |>
      mutate(result = "contrast", group = NA_character_) |>
      select(result, contrast, group, scale, estimate, SE, t, df, p, ci_lo, ci_hi)
  )
}

section("STACKED MODEL")
cat(
  "Rows =", nrow(long),
  "| clusters =", n_distinct(long$participant_id), "\n"
)

m_points <- fit_stack(long, "y")
m_d <- fit_stack(long, "y_d")

cat("Cluster-robust df:", m_points$df.residual, "\n")

points_tbl <- report_scale(m_points, "Slider points", digits = 2)
d_tbl <- report_scale(m_d, "Cohen's d", digits = 3)

out <- bind_rows(points_tbl, d_tbl)
out_dir <- here("output", "study_2", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir, "pro-v-anti-persuasion.csv")
write.csv(out, out_path, row.names = FALSE)

# Display labels. The average contrast is stored as
# "Anti average minus pro average".
row_labels <- c(
  "Meat" = "Meat vs.\\ control",
  "Charity" = "Charity vs.\\ control",
  "Lying" = "Lying vs.\\ control",
  "Political violence" = "Political violence vs.\\ control",
  "Anti average minus pro average" = "Antisocial average minus prosocial average",
  "Lying vs meat" = "Lying effect minus meat effect",
  "Lying vs charity" = "Lying effect minus charity effect",
  "Political violence vs meat" = "Political-violence effect minus meat effect",
  "Political violence vs charity" = "Political-violence effect minus charity effect"
)
section_headers <- c(
  "own-domain" = "Own-arm effects",
  "contrast" = "Comparing effects across domains"
)

fmt_num <- function(x, digits) formatC(x, digits = digits, format = "f")

tex_row <- function(rows, digits) {
  vapply(seq_len(nrow(rows)), function(i) {
    sprintf(
      "  \\quad %s & %s & %s & [%s, %s] & %s \\\\",
      row_labels[[rows$contrast[[i]]]],
      fmt_num(rows$estimate[[i]], digits),
      fmt_num(rows$SE[[i]], digits),
      fmt_num(rows$ci_lo[[i]], digits),
      fmt_num(rows$ci_hi[[i]], digits),
      fmt_p_tex(rows$p[[i]], bold = FALSE)
    )
  }, character(1))
}

# Paper table is the slider-point comparison. Cohen's d stays in the
# console printout and the csv.
slider <- out[out$scale == "Slider points", , drop = FALSE]
own_rows <- tex_row(slider[slider$result == "own-domain", , drop = FALSE], 2)
contrast_rows <- tex_row(slider[slider$result == "contrast", , drop = FALSE], 2)

note <- paste(
  "\\footnotesize\\textit{Note.} Own-arm rows are the own-arm versus",
  "control effect on immediate belief change, on the original 0--100 scale.",
  "Lying and political-violence changes are multiplied by $-$1, so a",
  "positive estimate is movement toward the position the AI argued for.",
  "Comparisons use that same orientation: positive values mean the",
  "antisocial effect is larger. Estimates are from a stacked regression",
  "in which every term of the preregistered model is interacted with",
  "domain, with CR2 standard errors clustered by participant. Intervals",
  "use the cluster-robust residual degrees of freedom. $p$-values are",
  "two-sided and unadjusted.",
  sep = " "
)

tex <- c(
  "% Requires booktabs.",
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{Own-arm belief effects and comparisons across domains.}",
  "  \\label{tab:pro-anti}",
  "  \\footnotesize",
  "  \\setlength{\\tabcolsep}{5pt}",
  "  \\begin{tabular}{@{}lcccc@{}}",
  "  \\toprule",
  "   & Estimate & SE & 95\\% CI & $p$ \\\\",
  "  \\midrule",
  sprintf(
    "  \\multicolumn{5}{@{}l}{\\textbf{%s}} \\\\",
    section_headers[["own-domain"]]
  ),
  "  \\addlinespace[2pt]",
  own_rows,
  "  \\addlinespace[6pt]",
  sprintf(
    "  \\multicolumn{5}{@{}l}{\\textbf{%s}} \\\\",
    section_headers[["contrast"]]
  ),
  "  \\addlinespace[2pt]",
  contrast_rows,
  "  \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{2pt}",
  "  \\begin{minipage}{\\linewidth}",
  paste0("  ", note),
  "  \\end{minipage}",
  "\\end{table}",
  ""
)

out_tex <- file.path(out_dir, "pro-v-anti-persuasion.tex")
write_paper_table(tex, out_tex)
cat("\nSaved:", out_path, "\n")
