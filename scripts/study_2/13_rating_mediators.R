# =============================================================================
# 13 · Mediation — conversation ratings as mediators (beliefs and behaviour)
# =============================================================================
# Prereg paths: M ~ condition + t0 + order; Y ~ M + condition + t0 + order
# M = understood / respectful / originality (+ composite, estimated but omitted
# from tables). Own-arm vs control. Intervals follow mediation_helpers.R
# (1,000 bootstrap sims unless main.R sets mediation_bootstrap <- FALSE).
#
# Writes mediation-ratings.tex (and .csv).

library(dplyr)
library(tidyr)
library(here)
# Do not attach mediation: it pulls MASS onto the search path and masks
# dplyr::select in every script sourced after this one.
invisible(loadNamespace("mediation"))

source(here("scripts", "functions", "helpers.R"))
source(here("scripts", "functions", "study2_style.R"))
source(here("scripts", "functions", "study2_outcomes.R"))
source(here("scripts", "functions", "mediation_helpers.R"))

options(width = max(getOption("width"), 220L))

d1 <- load_study2_d1_mediation()

cat("Analysis sample (d1 completers): N =", nrow(d1), "\n")
print(table(d1$condition))

announce_mediation_settings()
set.seed(MEDIATION_SEED)
table_rows <- list()

# =============================================================================
# CONVERSATION RATINGS AS MEDIATORS OF t1 BEHAVIOUR
# =============================================================================

section("MEDIATION: BEHAVIOUR VIA CONVERSATION RATINGS")
cat("Prereg paths: M ~ condition + t0 + order; Y ~ M + condition + t0 + order\n")
cat("M = perceived understanding / respectfulness / originality (novelty).\n")
cat("Also a composite (row mean of the three). Own-arm vs control.\n")
cat(mediation_sims_line(), "\n", sep = "")

rating_rows <- list()
idx <- 0L

for (i in seq_len(nrow(behavior_defs))) {
  brow <- behavior_defs[i, ]
  treat_lab <- unname(condition_labels[brow$treat])

  for (j in seq_len(nrow(rating_defs))) {
    rrow <- rating_defs[j, ]
    idx <- idx + 1L
    cat(sprintf(
      "\n--- [%d/%d] %s via %s (%s vs Control) ---\n",
      idx, nrow(behavior_defs) * nrow(rating_defs),
      brow$short, rrow$label, treat_lab
    ))

    res <- run_mediation(
      d1,
      y = brow$y, m = rrow$var, t0 = brow$pre,
      treat_group = brow$treat
    )
    cat(sprintf(
      "  N = %d | ACME = %.4f (p = %s) | ADE = %.4f (p = %s) | prop = %.3f\n",
      res$n, res$med$d.avg, fmt_p(res$med$d.avg.p),
      res$med$z.avg, fmt_p(res$med$z.avg.p), res$med$n.avg
    ))

    rating_rows[[paste(brow$key, rrow$key, sep = "_")]] <- extract_med_row(
      res, brow$short, rrow$label, treat_lab
    )
    if (rrow$key != "composite") {
      table_rows[[paste(brow$key, "t1beh", rrow$key, sep = "_")]] <- med_record(
        res, "rating_t1_beh", brow$key, rrow$key
      )
    }
  }
}

rating_summary <- bind_rows(rating_rows)

section("MEDIATION SUMMARY: CONVERSATION RATINGS -> BEHAVIOUR")
cat("ACME = average causal mediation effect; ADE = average direct effect.\n")
cat("a = condition→M (own-arm); b = M→Y in the outcome model.\n\n")
print_mediation_summary_table(rating_summary)

# =============================================================================
# CONVERSATION RATINGS AS MEDIATORS OF t2 BEHAVIOUR
# =============================================================================

section("MEDIATION: t2 BEHAVIOUR VIA CONVERSATION RATINGS")
cat("Returners only. Y = t2 behaviour. Own-arm vs control.\n")
cat(mediation_sims_line(), "\n", sep = "")

d_t2 <- load_study2_d_t2_mediation()

cat("Follow-up sample: N =", nrow(d_t2), "\n")
print(table(d_t2$condition))

set.seed(MEDIATION_SEED)
t2_rows <- list()

for (i in seq_len(nrow(behavior_defs_t2))) {
  brow <- behavior_defs_t2[i, ]
  treat_lab <- unname(condition_labels[brow$treat])

  for (j in seq_len(nrow(rating_defs))) {
    rrow <- rating_defs[j, ]
    label <- paste0(brow$short, " via ", rrow$label, "  [", treat_lab, " vs Control]")
    cat("\n=== Fitting:", label, "===\n")
    res <- run_mediation(
      d_t2,
      y = brow$y, m = rrow$var, t0 = brow$pre,
      treat_group = brow$treat
    )
    cat(sprintf(
      "  N = %d | ACME = %.4f (p = %s) | ADE = %.4f (p = %s) | prop = %.3f\n",
      res$n, res$med$d.avg, fmt_p(res$med$d.avg.p),
      res$med$z.avg, fmt_p(res$med$z.avg.p), res$med$n.avg
    ))
    t2_rows[[paste(brow$key, rrow$key, sep = "_")]] <- extract_med_row(
      res, brow$short, rrow$label, treat_lab
    )
    if (rrow$key != "composite") {
      table_rows[[paste(brow$key, "t2beh", rrow$key, sep = "_")]] <- med_record(
        res, "rating_t2_beh", brow$key, rrow$key
      )
    }
  }
}

t2_summary <- bind_rows(t2_rows)

section("MEDIATION SUMMARY: CONVERSATION RATINGS -> t2 BEHAVIOUR")
cat("ACME = average causal mediation effect; ADE = average direct effect.\n")
cat("a = condition→M (own-arm); b = M→Y in the outcome model.\n\n")
print_mediation_summary_table(t2_summary)

# =============================================================================
# CONVERSATION RATINGS AS MEDIATORS OF BELIEFS (t1 and t2)
# =============================================================================

section("MEDIATION: BELIEFS VIA CONVERSATION RATINGS")
cat("Prereg paths: M ~ condition + t0 + order; Y ~ M + condition + t0 + order\n")
cat("Y = matching belief (t1 post-treatment, or t2 follow-up).\n")
cat("M = perceived understanding / respectfulness / originality (novelty).\n")
cat("Own-arm vs control.", mediation_sims_line(), "\n")

rating_table_defs <- dplyr::filter(rating_defs, key != "composite")

set.seed(MEDIATION_SEED)
rating_belief_jobs <- list(
  list(defs = belief_outcome_defs, data = d1, panel = "rating_t1_bel", wave = "t1"),
  list(defs = belief_outcome_defs_t2, data = d_t2, panel = "rating_t2_bel", wave = "t2")
)

for (job in rating_belief_jobs) {
  for (i in seq_len(nrow(job$defs))) {
    brow <- job$defs[i, ]
    treat_lab <- unname(condition_labels[brow$treat])
    for (j in seq_len(nrow(rating_table_defs))) {
      rrow <- rating_table_defs[j, ]
      label <- paste0(
        brow$key, " belief (", job$wave, ") via ", rrow$label,
        "  [", treat_lab, " vs Control]"
      )
      cat("\n=== Fitting:", label, "===\n")
      res <- run_mediation(
        job$data,
        y = brow$y, m = rrow$var, t0 = brow$pre,
        treat_group = brow$treat
      )
      cat(sprintf(
        "  N = %d | ACME = %.4f (p = %s) | ADE = %.4f (p = %s) | prop = %.3f\n",
        res$n, res$med$d.avg, fmt_p(res$med$d.avg.p),
        res$med$z.avg, fmt_p(res$med$z.avg.p), res$med$n.avg
      ))
      table_rows[[paste(job$panel, brow$key, rrow$key, sep = "_")]] <- med_record(
        res, job$panel, brow$key, rrow$key
      )
    }
  }
}

# =============================================================================
# TABLE: mediation-ratings.tex
# =============================================================================

section("MEDIATION TABLE: RATINGS")

table_results <- dplyr::bind_rows(table_rows)
expected_rows <- 4L * 3L + 2L * 3L + 4L * 3L + 2L * 3L
if (nrow(table_results) != expected_rows) {
  stop(sprintf(
    "Expected %d rating mediation rows for the table, found %d.",
    expected_rows, nrow(table_results)
  ))
}

out_dir <- here("output", "study_2", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
rating_path <- file.path(out_dir, "mediation-ratings.tex")
rating_csv <- file.path(out_dir, "mediation-ratings.csv")

rating_tex <- build_rating_mediation_tex(table_results)
write_paper_table(rating_tex, rating_path)
write_mediation_csv(
  table_results,
  vapply(mediation_rating_panels, function(spec) spec$panel, character(1)),
  rating_csv
)

print_rating_mediation_console(table_results)

cat(sprintf("Wrote %s\nWrote %s\n", rating_path, rating_csv))
cat("\nStudy 2 rating mediation complete.\n")
