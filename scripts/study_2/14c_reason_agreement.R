# =============================================================================
# 14c · Reason-code unanimous agreement across LLM coders
# =============================================================================
# Agreement table for SI. Sourced by 14_reasons.R.

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

models <- c("claude", "gpt", "deepseek")

# Display order matches the participant-reasons figure.
reason_specs <- list(
  list(
    group = "Meat",
    file = "classifications_meat.csv",
    categories = c(
      natural = "Natural",
      necessary = "Necessary",
      amoral = "Amoral",
      conditional = "Conditional",
      normal = "Normal",
      nice = "Nice",
      religious = "Religious"
    )
  ),
  list(
    group = "Charity",
    file = "classifications_charity.csv",
    categories = c(
      financial_constraints = "Financial",
      supererogatory = "Supererogatory",
      skepticism = "Skepticism",
      relativism = "Relativism",
      property_rights = "Property rights",
      systemic = "Systemic",
      limited_scope = "Limited scope"
    )
  ),
  list(
    group = "Lying",
    file = "classifications_lying.csv",
    categories = c(
      harm = "Harm",
      absolute_prohibition = "Absolute prohibition",
      unfair_advantage = "Unfair advantage",
      moral_character = "Moral character",
      broken_trust = "Broken trust",
      religious_duty = "Religious duty"
    )
  ),
  list(
    group = "Political violence",
    file = "classifications_polviol.csv",
    categories = c(
      absolute_prohibition = "Absolute prohibition",
      ineffectiveness = "Ineffective",
      unnecessary_force = "Unnecessary force",
      illegitimate_authority = "Illegitimate authority",
      moral_character = "Moral character",
      harm = "Harm"
    )
  )
)

as_logical_safe <- function(x) {
  if (is.logical(x)) return(x)
  if (is.character(x)) {
    x <- tolower(trimws(x))
    out <- rep(NA, length(x))
    out[x %in% c("true", "t", "1", "yes")] <- TRUE
    out[x %in% c("false", "f", "0", "no")] <- FALSE
    return(out)
  }
  as.logical(x)
}

# One column per model. TRUE when every model gave the same non-missing value.
unanimous_mask <- function(d, category, models) {
  cols <- paste0(category, "_", models)
  if (!all(cols %in% names(d))) {
    return(rep(NA, nrow(d)))
  }
  sub <- lapply(d[cols], as_logical_safe)
  complete <- Reduce(`&`, lapply(sub, function(x) !is.na(x)))
  all_true <- Reduce(`&`, lapply(sub, function(x) x %in% TRUE))
  all_false <- Reduce(`&`, lapply(sub, function(x) x %in% FALSE))
  ifelse(complete, all_true | all_false, NA)
}

pct_from_mask <- function(mask) {
  n_valid <- sum(!is.na(mask))
  n_agree <- sum(mask %in% TRUE)
  tibble(
    n_valid = n_valid,
    n_agree = n_agree,
    pct_agree = if (n_valid == 0) NA_real_ else round(100 * n_agree / n_valid, 1)
  )
}

results <- map_dfr(reason_specs, function(spec) {
  d <- read_csv(file.path(class_dir, spec$file), show_col_types = FALSE)
  masks <- lapply(names(spec$categories), unanimous_mask, d = d, models = models)
  names(masks) <- names(spec$categories)
  overall <- Reduce(`&`, masks)

  bind_rows(
    pct_from_mask(overall) |>
      mutate(
        group = spec$group,
        code = "overall",
        label = "Overall",
        row = 0L
      ),
    map_dfr(seq_along(spec$categories), function(i) {
      stem <- names(spec$categories)[i]
      pct_from_mask(masks[[stem]]) |>
        mutate(
          group = spec$group,
          code = stem,
          label = unname(spec$categories[[stem]]),
          row = i
        )
    })
  )
})

fmt_pct <- function(x) ifelse(is.na(x), "", sprintf("%.1f", x))

n_by_group <- results |>
  filter(code == "overall") |>
  distinct(group, n_valid)

wide <- results |>
  mutate(pct = fmt_pct(pct_agree)) |>
  select(group, row, label, pct, n_valid, n_agree, pct_agree) |>
  pivot_wider(
    names_from = group,
    values_from = c(label, pct, n_valid, n_agree, pct_agree),
    names_sep = "___"
  ) |>
  arrange(row)

groups <- vapply(reason_specs, function(s) s$group, character(1))

cell <- function(group, field, i) {
  col <- paste0(field, "___", group)
  val <- wide[[col]][i]
  if (length(val) == 0 || is.na(val)) "" else as.character(val)
}

body <- vapply(seq_len(nrow(wide)), function(i) {
  pieces <- unlist(lapply(groups, function(g) c(cell(g, "label", i), cell(g, "pct", i))))
  paste0("  ", paste(pieces, collapse = " & "), " \\\\")
}, character(1))

# Blank the padded rows of the shorter codebooks (lying, political violence).
# Overall stays on row 1. A midrule separates it from the codes.
overall_row <- body[1]
code_rows <- body[-1]

spanners <- paste(
  vapply(seq_along(groups), function(j) {
    g <- groups[j]
    n <- n_by_group$n_valid[n_by_group$group == g]
    lo <- 2 * j - 1
    hi <- 2 * j
    sprintf(
      "\\multicolumn{2}{c}{%s (N = %s)}",
      g,
      format(n, big.mark = ",")
    )
  }, character(1)),
  collapse = " & "
)

cmid <- paste(
  vapply(seq_along(groups), function(j) {
    sprintf("\\cmidrule(lr){%d-%d}", 2 * j - 1, 2 * j)
  }, character(1)),
  collapse = " "
)

subheads <- paste(rep(c("Code", "\\% unanimous"), length(groups)), collapse = " & ")

note <- paste(
  "\\scriptsize\\raggedright\\textit{Note.} Entries show the percent of texts",
  "on which Claude, GPT, and DeepSeek agreed in their classification for each",
  "category and overall (i.e., they agreed on all classifications to all",
  "categories for a participant)."
)

tex <- c(
  "% Requires booktabs.",
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{Model agreement for Participant Reason Classifications.}",
  "  \\label{tab:reason-agreement}",
  "  \\scriptsize",
  "  \\setlength{\\tabcolsep}{4pt}",
  "  \\begin{tabular}{@{}lr lr lr lr@{}}",
  "  \\toprule",
  paste0("  ", spanners, " \\\\"),
  paste0("  ", cmid),
  paste0("  ", subheads, " \\\\"),
  "  \\midrule",
  overall_row,
  "  \\midrule",
  code_rows,
  "  \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{2pt}",
  "  \\begin{minipage}{\\linewidth}",
  paste0("  ", note),
  "  \\end{minipage}",
  "\\end{table}",
  ""
)

out_dir <- here("output", "study_2", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_tex <- file.path(out_dir, "reason-agreement.tex")
out_csv <- file.path(out_dir, "reason-agreement.csv")
write_paper_table(tex, out_tex)

results |>
  select(group, code, label, n_valid, n_agree, pct_agree) |>
  write.csv(out_csv, row.names = FALSE)

section("REASON-CODE UNANIMOUS AGREEMENT")
show_tbl <- results |>
  mutate(cell = paste0(label, "  ", fmt_pct(pct_agree))) |>
  select(row, group, cell) |>
  tidyr::pivot_wider(names_from = group, values_from = cell) |>
  arrange(row) |>
  select(-row)
print(as.data.frame(show_tbl), row.names = FALSE, right = FALSE)

cat(sprintf("\nWrote %s\nWrote %s\n", out_tex, out_csv))
