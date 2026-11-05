# =============================================================================
# 09 · Mediation — behaviour via post-treatment beliefs (t1 and t2)
# =============================================================================
# Prereg paths: M ~ condition + t0 + order; Y ~ M + condition + t0 + order
# Own-arm vs control. Intervals follow mediation_helpers.R
# (1,000 bootstrap sims unless main.R sets mediation_bootstrap <- FALSE).
#
# Writes mediation-beliefs.tex (and .csv).

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
# t1: POST-TREATMENT BELIEF AS MEDIATOR OF t1 BEHAVIOUR
# =============================================================================

section("MEDIATION: BEHAVIOUR VIA POST-TREATMENT BELIEF")
cat("Prereg paths: M ~ condition + t0 + order; Y ~ M + condition + t0 + order\n")
cat("M = matching post-treatment belief; t0 = matching pre-treatment belief.\n")
cat(mediation_sims_line(), "| own-arm vs control.\n")

belief_results <- list()
belief_rows <- list()

for (i in seq_len(nrow(behavior_defs))) {
  row <- behavior_defs[i, ]
  treat_lab <- unname(condition_labels[row$treat])
  label <- paste0(
    row$short, " via ", gsub("_", " ", row$post),
    "  [", treat_lab, " vs Control]"
  )
  cat("\n=== Fitting:", label, "===\n")

  res <- run_mediation(
    d1,
    y = row$y, m = row$post, t0 = row$pre,
    treat_group = row$treat
  )
  belief_results[[row$key]] <- res
  print_mediation_full(label, res)
  belief_rows[[row$key]] <- extract_med_row(
    res, row$short, paste0("post belief (", row$key, ")"), treat_lab
  )
  table_rows[[paste0("t1beh_", row$key)]] <- med_record(
    res, "t1_beh_via_t1_bel", row$key, "t1_belief"
  )
}

belief_summary <- bind_rows(belief_rows)

section("MEDIATION SUMMARY: BELIEF -> BEHAVIOUR")
cat("ACME = average causal mediation effect; ADE = average direct effect.\n")
cat("a = condition→M (own-arm); b = M→Y in the outcome model.\n\n")
print_mediation_summary_table(belief_summary)

# =============================================================================
# t2 BEHAVIOUR VIA t1 POST-TREATMENT BELIEF
# =============================================================================

section("MEDIATION: t2 BEHAVIOUR VIA t1 BELIEF")
cat("Returners only. Y = t2 behaviour. M = matching post-treatment belief.\n")
cat("Own-arm vs control.", mediation_sims_line(), "\n")

d_t2 <- load_study2_d_t2_mediation()

cat("Follow-up sample: N =", nrow(d_t2), "\n")
print(table(d_t2$condition))

t2_mediator_defs <- tibble::tribble(
  ~kind,     ~var_from, ~label,
  "belief",  "post",    "Post-treatment belief"
)

set.seed(MEDIATION_SEED)
t2_rows <- list()

for (i in seq_len(nrow(behavior_defs_t2))) {
  brow <- behavior_defs_t2[i, ]
  treat_lab <- unname(condition_labels[brow$treat])

  for (j in seq_len(nrow(t2_mediator_defs))) {
    mdef <- t2_mediator_defs[j, ]
    m_var <- brow[[mdef$var_from]]
    label <- paste0(brow$short, " via ", mdef$label, "  [", treat_lab, " vs Control]")
    cat("\n=== Fitting:", label, "===\n")
    res <- run_mediation(
      d_t2,
      y = brow$y, m = m_var, t0 = brow$pre,
      treat_group = brow$treat
    )
    print_mediation_full(label, res)
    t2_rows[[paste(brow$key, mdef$kind, sep = "_")]] <- extract_med_row(
      res, brow$short, mdef$label, treat_lab
    )
    table_rows[[paste(brow$key, "t2beh", "t1bel", sep = "_")]] <- med_record(
      res, "t2_beh_via_t1_bel", brow$key, "t1_belief"
    )
  }
}

t2_summary <- bind_rows(t2_rows)

section("MEDIATION SUMMARY: t1 BELIEF -> t2 BEHAVIOUR")
cat("ACME = average causal mediation effect; ADE = average direct effect.\n")
cat("a = condition→M (own-arm); b = M→Y in the outcome model.\n\n")
print_mediation_summary_table(t2_summary)

# =============================================================================
# t2 BEHAVIOUR VIA t2 BELIEF
# =============================================================================

section("MEDIATION: t2 BEHAVIOUR VIA t2 BELIEF")
cat("Returners only. Y = t2 behaviour. M = matching t2 belief. Own-arm vs control.\n")
cat("Paths: M ~ condition + t0 + order; Y ~ M + condition + t0 + order.\n")
cat(mediation_sims_line(), "\n", sep = "")

set.seed(MEDIATION_SEED)
for (i in seq_len(nrow(t2_belief_defs))) {
  row <- t2_belief_defs[i, ]
  treat_lab <- unname(condition_labels[row$treat])
  label <- paste0(row$short, " via t2 belief  [", treat_lab, " vs Control]")
  cat("\n=== Fitting:", label, "===\n")
  res <- run_mediation(
    d_t2,
    y = row$y, m = row$m, t0 = row$pre,
    treat_group = row$treat
  )
  print_mediation_full(label, res)
  table_rows[[paste(row$key, "t2beh", "t2bel", sep = "_")]] <- med_record(
    res, "t2_beh_via_t2_bel", row$key, "t2_belief"
  )
}

# =============================================================================
# TABLE: mediation-beliefs.tex
# =============================================================================

section("MEDIATION TABLE: BELIEFS")

table_results <- dplyr::bind_rows(table_rows)
expected_rows <- 4L + 2L + 2L
if (nrow(table_results) != expected_rows) {
  stop(sprintf(
    "Expected %d belief mediation rows for the table, found %d.",
    expected_rows, nrow(table_results)
  ))
}

out_dir <- here("output", "study_2", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
belief_path <- file.path(out_dir, "mediation-beliefs.tex")
belief_csv <- file.path(out_dir, "mediation-beliefs.csv")

belief_tex <- build_belief_mediation_tex(table_results)
write_paper_table(belief_tex, belief_path)
write_mediation_csv(
  table_results,
  vapply(mediation_belief_panels, function(spec) spec$panel, character(1)),
  belief_csv
)

print_belief_mediation_console(table_results)

cat(sprintf("Wrote %s\nWrote %s\n", belief_path, belief_csv))
cat("\nStudy 2 belief mediation complete.\n")
