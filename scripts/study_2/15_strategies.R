# =============================================================================
# 15 · AI persuasion strategies (figure + agreement)
# =============================================================================
# Mean of Claude, GPT, and Grok strategy ratings by treatment condition.
# Control conversations are not rated.
#
# Inputs: data/study_2/text_ratings/strategy_ratings.csv
#   Claude, GPT, and Grok ratings.
#
# Figures: output/study_2/figures/
#   fig-ai-strategies.png / fig-ai-strategies.pdf (paper copy via save_paper_pdf)
#
# Tables: output/study_2/tables/
#   strategy-agreement.tex / strategy-agreement.csv

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggtext)
library(ggdist)
library(cowplot)
library(here)

source(here("scripts", "functions", "helpers.R"))
source(here("scripts", "functions", "study2_style.R"))

options(width = max(getOption("width"), 220L))

class_dir <- here("data", "study_2", "text_ratings")
fig_dir <- here("output", "study_2", "figures")
table_dir <- here("output", "study_2", "tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

FIGURE_WIDTH  <- 7.08
FIGURE_HEIGHT <- 3.4
FIGURE_DPI    <- 300

models <- c("claude", "gpt", "grok")
rating_to_num <- c(none = 0, low = 1, moderate = 2, high = 3)

condition_labels <- c(
  meat    = "Meat",
  charity = "Charity",
  polviol = "Political Violence",
  lying   = "Lying"
)

colors_condition <- c(
  "Meat"               = "#009E73",
  "Charity"            = "#0072B2",
  "Political Violence" = "#CC79A7",
  "Lying"              = "#E69F00"
)

strategy_labels <- c(
  evidence_facts      = "Evidence & Facts",
  logical_consistency = "Logical Consistency",
  moral_principles    = "Moral Principles",
  emotional_appeal    = "Emotional Appeal",
  analogies_reframing = "Analogies & Reframing"
)

strategy_labels_tex <- c(
  evidence_facts      = "Evidence \\& facts",
  logical_consistency = "Logical consistency",
  moral_principles    = "Moral principles",
  emotional_appeal    = "Emotional appeal",
  analogies_reframing = "Analogies \\& reframing"
)

strategy_descriptions <- c(
  evidence_facts      = "Claims about how the world is",
  logical_consistency = "Conflict with a commitment already held",
  moral_principles    = "A general moral standard",
  emotional_appeal    = "Stirring sympathy, distress, or guilt",
  analogies_reframing = "A different case or perspective"
)

strategies <- names(strategy_labels)

strategy_ratings_path <- function() {
  candidates <- c(
    file.path(class_dir, "strategy_ratings.csv")
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop(
      "Missing strategy ratings in ", class_dir,
      " (expected strategy_ratings.csv)."
    )
  }
  found[[1]]
}

add_strategy_means <- function(d, raters) {
  out <- d
  for (s in strategies) {
    model_cols <- paste0(s, "_", raters)
    out[[paste0(s, "_mean")]] <- out |>
      dplyr::select(all_of(model_cols)) |>
      mutate(across(everything(), ~ rating_to_num[.])) |>
      rowMeans(na.rm = TRUE)
  }
  out
}

strategy_long <- function(d, strategy_order) {
  d |>
    pivot_longer(
      cols = all_of(paste0(strategies, "_mean")),
      names_to = "strategy",
      values_to = "rating"
    ) |>
    mutate(strategy = str_remove(strategy, "_mean")) |>
    filter(!is.na(rating), is.finite(rating)) |>
    mutate(
      strategy = factor(strategy, levels = strategy_order),
      condition = factor(
        condition_labels[as.character(condition)],
        levels = unname(condition_labels)
      )
    )
}

d_strat <- read_csv(strategy_ratings_path(), show_col_types = FALSE)
d_strat <- add_strategy_means(d_strat, models)

strategy_order <- d_strat |>
  pivot_longer(
    cols = all_of(paste0(strategies, "_mean")),
    names_to = "strategy",
    values_to = "rating"
  ) |>
  mutate(strategy = str_remove(strategy, "_mean")) |>
  filter(!is.na(rating)) |>
  group_by(strategy) |>
  summarise(mean_rating = mean(rating), .groups = "drop") |>
  arrange(mean_rating) |>
  pull(strategy)

strat_label_map <- setNames(
  paste0(
    "<b>", strategy_labels[strategies], "</b><br>",
    "<span style='font-size:8pt;'>", strategy_descriptions[strategies], "</span>"
  ),
  strategies
)

strat_long <- strategy_long(d_strat, strategy_order)

# =============================================================================
# Section 1 · Strategy-use figure (Claude, GPT, Grok)
# =============================================================================

finish_panel <- function(p, title = "AI Persuasive Strategy Use") {
  p +
    scale_colour_manual(values = colors_condition, name = NULL) +
    scale_y_discrete(labels = strat_label_map) +
    scale_x_continuous(
      breaks = 0:3,
      labels = c("Not\nused", "Limited\nuse", "Used with\nemphasis", "Used\nextensively")
    ) +
    coord_cartesian(xlim = c(-0.15, 3.15), clip = "off") +
    labs(x = "Strategy Rating", y = NULL, title = title) +
    theme_bw(base_size = 9) +
    theme(
      plot.title      = element_text(face = "bold", size = 8, hjust = 0.5,
                                     margin = margin(b = 2)),
      axis.text.y     = element_markdown(lineheight = 0.9, size = 7),
      axis.text.x     = element_text(size = 7),
      axis.title.x    = element_text(face = "bold", size = 8),
      panel.grid      = element_blank(),
      legend.position = "none",
      plot.margin     = margin(3, 5, 3, 3)
    )
}

legend_from <- function(p) {
  get_legend(
    p +
      guides(fill = guide_legend(direction = "vertical", override.aes = list(alpha = 1))) +
      guides(colour = guide_legend(direction = "vertical")) +
      theme(
        legend.position = "right",
        legend.direction = "vertical",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm")
      )
  )
}

save_strategy_fig <- function(panel, filename) {
  fig <- plot_grid(
    panel, legend_from(panel),
    nrow = 1, rel_widths = c(3.4, 1.15), align = "none"
  )
  save_plot(
    file.path(fig_dir, filename),
    fig,
    base_width = FIGURE_WIDTH,
    base_height = FIGURE_HEIGHT,
    dpi = FIGURE_DPI,
    bg = "white"
  )
  cat("Saved: output/study_2/figures/", filename, "\n", sep = "")
  invisible(fig)
}

strat_means <- strat_long |>
  group_by(strategy, condition) |>
  summarise(mean = mean(rating), .groups = "drop")

density_dodge  <- 0.68
density_height <- 0.62

panel_density <- finish_panel(
  ggplot(strat_long, aes(x = rating, y = strategy, fill = condition)) +
    stat_slab(
      density = density_bounded(bandwidth = 0.15, bounds = c(0, 3)),
      height = density_height,
      justification = 0.5,
      position = position_dodge(width = density_dodge),
      alpha = 0.55,
      normalize = "groups"
    ) +
    geom_point(
      data = strat_means,
      aes(x = mean, y = strategy, colour = condition, group = condition),
      position = position_dodge(width = density_dodge),
      size = 1.25,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = colors_condition, name = NULL)
)

fig_main <- save_strategy_fig(panel_density, "fig-ai-strategies.png")

use_quartz_text_metrics()
save_paper_pdf(
  fig_main, "fig-ai-strategies.pdf",
  width = FIGURE_WIDTH, height = FIGURE_HEIGHT,
  output_dir = fig_dir, png = FALSE
)

# =============================================================================
# Section 2 · Strategy-rating agreement (Claude, GPT, Grok)
# =============================================================================

blocks <- list(
  list(id = "ALL",     label = "Overall"),
  list(id = "meat",    label = "Meat conversations"),
  list(id = "charity", label = "Charity conversations"),
  list(id = "polviol", label = "Political violence conversations"),
  list(id = "lying",   label = "Lying conversations")
)

fmt_icc <- function(x) {
  s <- sprintf("%.3f", x)
  ifelse(startsWith(s, "-0"), paste0("-", substring(s, 3)), sub("^0", "", s))
}

fmt_pct <- function(x) sprintf("%.1f", x)
fmt_range <- function(x) sprintf("%.2f", x)
fmt_n <- function(n) format(n, big.mark = "{,}", scientific = FALSE)

icc_mean <- function(mat) {
  res <- psych::ICC(mat)$results
  c(
    icc2k = res$ICC[res$type == "ICC2k"],
    icc3k = res$ICC[res$type == "ICC3k"]
  )
}

agreement_row <- function(mat, strategy, block_id) {
  spread <- apply(mat, 1, function(x) max(x) - min(x))
  icc <- icc_mean(mat)
  tibble(
    block = block_id,
    strategy = strategy,
    n = nrow(mat),
    icc2k = unname(icc["icc2k"]),
    icc3k = unname(icc["icc3k"]),
    pct_exact = 100 * mean(spread == 0),
    pct_within_1 = 100 * mean(spread <= 1),
    mean_range = mean(spread)
  )
}

results <- map_dfr(blocks, function(block) {
  map_dfr(strategies, function(strategy) {
    cols <- paste0(strategy, "_", models)
    sub <- d_strat
    if (block$id != "ALL") sub <- sub[sub$condition == block$id, , drop = FALSE]
    mat <- sub[cols]
    mat[] <- lapply(mat, function(x) unname(rating_to_num[x]))
    mat <- na.omit(mat)
    mat <- as.matrix(mat)
    storage.mode(mat) <- "numeric"
    agreement_row(mat, strategy, block$id)
  })
})

block_n <- results |>
  group_by(block) |>
  summarise(n = unique(n), .groups = "drop")

n_check <- results |>
  group_by(block) |>
  summarise(n_strategies = n(), n_distinct = n_distinct(n), .groups = "drop")
if (any(n_check$n_strategies != 5) || any(n_check$n_distinct != 1)) {
  stop("N differs across strategies within a block; the section header cannot carry a single N.")
}

body <- map(blocks, function(blk) {
  block_id <- blk$id
  n <- block_n$n[block_n$block == block_id]
  header <- sprintf(
    "  \\multicolumn{6}{@{}l}{\\textit{%s} (N = %s)} \\\\",
    blk$label,
    fmt_n(n)
  )
  rows <- results |>
    filter(block == block_id) |>
    mutate(label = unname(strategy_labels_tex[strategy])) |>
    mutate(line = sprintf(
      "  \\quad %s & %s & %s & %s & %s & %s \\\\",
      label, fmt_icc(icc2k), fmt_icc(icc3k), fmt_pct(pct_exact),
      fmt_pct(pct_within_1), fmt_range(mean_range)
    )) |>
    pull(line)
  c(header, rows)
})

body_tex <- paste(unlist(lapply(seq_along(body), function(i) {
  chunk <- body[[i]]
  if (i == 1) return(chunk)
  c("  \\addlinespace", chunk)
})), collapse = "\n")

note <- paste(
  "\\footnotesize\\raggedright\\textit{Note.} Claude, GPT, and Grok rated each treatment",
  "conversation on the degree it used five persuasive strategies (coded 0--3).",
  "The analyses use the average of the three ratings.",
  "ICC(3,$k$) is the reliability of that average for these models, and",
  "ICC(2,$k$) is the reliability of an average of three models drawn at random,",
  "so differences in level among models count as disagreement.",
  "`Exact' reports the percent of conversations on which all three models",
  "assigned the exact same label. `Within one point' reports the percent of",
  "conversations in which the highest and lowest model ratings differed by at",
  "most one scale point. Mean range is the average of highest minus lowest,",
  "in scale points. Overall pools the four treatment conditions.",
  sep = " "
)

tex <- c(
  "% Requires booktabs.",
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{Model agreement for AI persuasion strategy ratings.}",
  "  \\label{tab:strategy-agreement}",
  "  \\footnotesize",
  "  \\setlength{\\tabcolsep}{4pt}",
  "  \\begin{tabular}{@{}lccccc@{}}",
  "  \\toprule",
  "  & ICC(2,$k$) & ICC(3,$k$) & Exact (\\%) & Within one point (\\%) & Mean range \\\\",
  "  \\midrule",
  body_tex,
  "  \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{2pt}",
  "  \\begin{minipage}{\\linewidth}",
  paste0("  ", note),
  "  \\end{minipage}",
  "\\end{table}",
  ""
)

out_tex <- file.path(table_dir, "strategy-agreement.tex")
out_csv <- file.path(table_dir, "strategy-agreement.csv")
write_paper_table(tex, out_tex)

results |>
  mutate(
    block_label = vapply(block, function(id) {
      blocks[[which(vapply(blocks, function(b) b$id, character(1)) == id)]]$label
    }, character(1)),
    strategy_label = unname(strategy_labels_tex[strategy])
  ) |>
  select(block, block_label, strategy, strategy_label, n, icc2k, icc3k, pct_exact, pct_within_1, mean_range) |>
  write_csv(out_csv)

section("STRATEGY AGREEMENT: CLAUDE, GPT, GROK")
show_tbl <- results |>
  mutate(
    block = factor(block, levels = vapply(blocks, function(b) b$id, character(1))),
    strategy = factor(strategy, levels = strategies),
    cell = sprintf(
      "ICC2k %s   ICC3k %s   exact %s   within1 %s   range %s",
      fmt_icc(icc2k), fmt_icc(icc3k), fmt_pct(pct_exact),
      fmt_pct(pct_within_1), fmt_range(mean_range)
    )
  ) |>
  arrange(block, strategy) |>
  select(block, strategy, n, cell)
print(as.data.frame(show_tbl), row.names = FALSE, right = FALSE)

cat(sprintf("\nWrote %s\nWrote %s\n", out_tex, out_csv))
