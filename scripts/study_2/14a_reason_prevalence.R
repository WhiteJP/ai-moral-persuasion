# =============================================================================
# 14a · Participant reason prevalence (LLM consensus figures)
# =============================================================================
# Reason-code prevalence by domain. Sourced by 14_reasons.R.
# Inputs: data/study_2/text_ratings/classifications_*.csv
# Figures: output/study_2/figures/fig-participant-reasoning.*

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

source(here("scripts", "functions", "study2_style.R"))

output_dir <- here("output", "study_2", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
use_quartz_text_metrics()

FIGURE_WIDTH_1ROW <- 14
# Extra space between the top row (meat, charity) and the bottom row
# (lying, political violence), as a fraction of one row.
ROW_GAP <- 0.07
FIGURE_HEIGHT_2X2 <- 4.15 * (2 + ROW_GAP) / 2

# ---------------------------------------------------------------------------
# Codebooks (short name + one-line gloss, Study 1 wording where it matches)
# ---------------------------------------------------------------------------

MEAT_CATS <- list(
  natural_consensus     = c("Natural",     "Evolved to eat meat"),
  necessary_consensus   = c("Necessary",   "Required for nutrition"),
  amoral_consensus      = c("Amoral",      "Not a moral issue"),
  conditional_consensus = c("Conditional", "OK if ethically sourced"),
  normal_consensus      = c("Normal",      "Socially acceptable"),
  nice_consensus        = c("Nice",        "Tastes good"),
  religious_consensus   = c("Religious",   "God permits it")
)

CHARITY_CATS <- list(
  financial_constraints_consensus = c("Financial",      "Can't afford to give"),
  supererogatory_consensus        = c("Supererogatory", "Nice but not required"),
  skepticism_consensus            = c("Skepticism",     "Charity doesn't work"),
  relativism_consensus            = c("Relativism",     "No objective duty"),
  property_rights_consensus       = c("Property Rights","It's my money"),
  systemic_consensus              = c("Systemic",       "Need structural change"),
  limited_scope_consensus         = c("Limited Scope",  "Obligations are local")
)

LYING_CATS <- list(
  harm_consensus                 = c("Harm",                 "Causes harm"),
  absolute_prohibition_consensus = c("Absolute Prohibition", "Deception always wrong"),
  unfair_advantage_consensus     = c("Unfair Advantage",     "Unearned or stolen gain"),
  moral_character_consensus      = c("Moral Character",      "Lying shows bad character"),
  broken_trust_consensus         = c("Broken Trust",         "Undermines cooperation"),
  religious_duty_consensus       = c("Religious Duty",       "God forbids it")
)

POLVIOL_CATS <- list(
  absolute_prohibition_consensus   = c("Absolute Prohibition",  "Violence is always wrong"),
  ineffectiveness_consensus        = c("Ineffective",           "Doesn't achieve goals"),
  unnecessary_force_consensus      = c("Unnecessary Force",     "Other methods exist"),
  illegitimate_authority_consensus = c("Illegitimate Authority","No right to use force"),
  moral_character_consensus        = c("Moral Character",       "Violence shows bad character"),
  harm_consensus                   = c("Harm",                  "Causes harm")
)

# ---------------------------------------------------------------------------
# Load classifications (Python True/False → logical)
# ---------------------------------------------------------------------------

as_logical_flag <- function(x) {
  if (is.logical(x)) return(x)
  tolower(as.character(x)) %in% c("true", "t", "1")
}

read_classifications <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  cons <- names(df)[grepl("_consensus$", names(df))]
  df |>
    mutate(across(all_of(cons), as_logical_flag))
}

df_meat    <- read_classifications(file.path(class_dir, "classifications_meat.csv"))
df_charity <- read_classifications(file.path(class_dir, "classifications_charity.csv"))
df_lying   <- read_classifications(file.path(class_dir, "classifications_lying.csv"))
df_polviol <- read_classifications(file.path(class_dir, "classifications_polviol.csv"))

# ---------------------------------------------------------------------------
# Console summary
# ---------------------------------------------------------------------------

reason_counts <- function(data, categories) {
  n_texts <- nrow(data)
  cons <- names(categories)
  any_true <- rowSums(sapply(cons, function(v) data[[v]] %in% TRUE)) > 0
  n_uncoded <- sum(!any_true)
  purrr::map_dfr(cons, function(var) {
    info <- categories[[var]]
    count <- sum(data[[var]] %in% TRUE, na.rm = TRUE)
    tibble(
      n_texts     = n_texts,
      uncoded     = n_uncoded,
      uncoded_pct = round(100 * n_uncoded / n_texts, 1),
      code        = info[1],
      n           = count,
      pct         = round(100 * count / n_texts, 1)
    )
  }) |>
    arrange(desc(n))
}

print_reason_table <- function(label, data, categories) {
  tab <- reason_counts(data, categories)
  section(sprintf(
    "%s  (N = %d, uncoded = %d, %.1f%%)",
    label, nrow(data), tab$uncoded[1], tab$uncoded_pct[1]
  ))
  print(as.data.frame(tab |> dplyr::select(code, n, pct)), row.names = FALSE)
}

print_reason_table("MEAT (skeptics, pre \u2264 50)", df_meat, MEAT_CATS)
print_reason_table("CHARITY (skeptics, pre \u2264 50)", df_charity, CHARITY_CATS)
print_reason_table("LYING (believers, pre \u2265 50)", df_lying, LYING_CATS)
print_reason_table("POLITICAL VIOLENCE (believers, pre \u2265 50)", df_polviol, POLVIOL_CATS)

# ---------------------------------------------------------------------------
# Panel
# ---------------------------------------------------------------------------

# No blank rows under the shorter panels. Those empty slots were the
# white band between the last bar and the caption.
N_BAR_SLOTS <- 0

axis_label_html <- function(label, desc) {
  paste0(
    "<p style='text-align:right;margin:0'>",
    "<b>", label, "</b><br>",
    "<span style='font-size:8pt;color:grey50;'>", desc, "</span>",
    "</p>"
  )
}

make_reasoning_panel <- function(data, categories, panel_title, subtitle,
                                 compact = FALSE) {
  bar_w    <- if (compact) 0.55 else 1
  title_sz <- if (compact) 7.5 else 8
  sub_sz   <- if (compact) 6 else 7.5
  y_sz     <- if (compact) 6.5 else 7
  pct_sz   <- if (compact) 1.9 else 2.3
  margins  <- if (compact) margin(2, 4, 1, 1) else margin(2, 4, 1, 2)

  counts <- purrr::map_dfr(names(categories), function(var) {
    info  <- categories[[var]]
    label <- info[1]
    desc  <- info[2]
    count <- if (var %in% names(data)) sum(data[[var]] %in% TRUE, na.rm = TRUE) else 0
    pct   <- round(100 * count / nrow(data), 1)
    tibble(
      Cat       = label,
      AxisLabel = if (compact) paste0("<b>", label, "</b>") else axis_label_html(label, desc),
      Count     = count,
      Pct       = pct,
      Label     = sprintf("%.1f%%", pct)
    )
  }) |>
    arrange(desc(Count))

  if (!compact && nrow(counts) < N_BAR_SLOTS) {
    n_pad <- N_BAR_SLOTS - nrow(counts)
    pad <- tibble(
      Cat       = paste0(".", seq_len(n_pad)),
      AxisLabel = "",
      Count     = NA_real_,
      Pct       = NA_real_,
      Label     = ""
    )
    counts <- bind_rows(counts, pad)
  }

  y_levels <- rev(counts$Cat)
  counts <- counts |>
    mutate(Cat = factor(Cat, levels = y_levels))

  ggplot(counts, aes(x = Pct, y = Cat)) +
    geom_col(fill = "skyblue", color = "black", width = bar_w, na.rm = TRUE) +
    geom_text(
      aes(label = Label), hjust = -0.15, size = pct_sz, fontface = "bold",
      na.rm = TRUE
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.28)), limits = c(0, NA)) +
    scale_y_discrete(
      drop = FALSE,
      labels = setNames(counts$AxisLabel, as.character(counts$Cat))
    ) +
    labs(title = panel_title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_bw(base_size = 9) +
    theme(
      plot.title          = element_text(face = "bold", size = title_sz, hjust = 0.5,
                                         margin = margin(b = 1)),
      plot.subtitle       = element_text(color = "grey30", size = sub_sz,
                                         hjust = 0.5, margin = margin(b = 3)),
      plot.title.position = "plot",
      panel.grid          = element_blank(),
      panel.border        = element_blank(),
      axis.line.y         = element_blank(),
      axis.line.x         = element_blank(),
      axis.ticks.y        = element_blank(),
      axis.ticks.x        = element_blank(),
      axis.ticks.length   = unit(0, "pt"),
      axis.text.x         = element_blank(),
      axis.text.y         = element_markdown(
        size = y_sz, lineheight = 1.05,
        hjust = 1, halign = 1,
        margin = margin(r = 2)
      ),
      plot.margin         = margins
    )
}

panel_meat <- function(compact) {
  make_reasoning_panel(
    df_meat, MEAT_CATS, "Reasons for Acceptability of Meat-eating",
    if (compact) {
      paste0("Skeptics, N = ", nrow(df_meat))
    } else {
      paste0("Skeptics (pre \u2264 50), N = ", nrow(df_meat))
    },
    compact = compact
  )
}

panel_charity <- function(compact) {
  make_reasoning_panel(
    df_charity, CHARITY_CATS,
    "Reasons Against Charitable Obligation",
    if (compact) {
      paste0("Skeptics, N = ", nrow(df_charity))
    } else {
      paste0("Skeptics (pre \u2264 50), N = ", nrow(df_charity))
    },
    compact = compact
  )
}

panel_lying <- function(compact) {
  make_reasoning_panel(
    df_lying, LYING_CATS,
    "Reasons Why Lying for Financial Gain Is Wrong",
    if (compact) {
      paste0("Believers, N = ", nrow(df_lying))
    } else {
      paste0("Believers (pre \u2265 50), N = ", nrow(df_lying))
    },
    compact = compact
  )
}

panel_polviol <- function(compact) {
  make_reasoning_panel(
    df_polviol, POLVIOL_CATS,
    "Reasons Why Political Violence Is Wrong",
    if (compact) {
      paste0("Believers, N = ", nrow(df_polviol))
    } else {
      paste0("Believers (pre \u2265 50), N = ", nrow(df_polviol))
    },
    compact = compact
  )
}

tag_panels <- function(compact) {
  list(
    A = panel_meat(compact),
    B = panel_charity(compact),
    C = panel_lying(compact),
    D = panel_polviol(compact)
  )
}

# ggplot2 4 puts a 1null spacer in the y-axis so leftover figure width
# inflates the label column (stretched words, gap before bars, titles
# centered on the bars). Zero those nulls so the axis is text-width only.
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

panel_header <- function(title, subtitle) {
  ggdraw() +
    draw_label(title, fontface = "bold", size = 8,
               x = 0.5, y = 0.68, hjust = 0.5, vjust = 0.5) +
    draw_label(subtitle, size = 7.5, colour = "grey30",
               x = 0.5, y = 0.22, hjust = 0.5, vjust = 0.5)
}

add_panel_header <- function(g, title, subtitle) {
  plot_grid(
    panel_header(title, subtitle), g,
    ncol = 1, rel_heights = c(0.13, 1)
  )
}

# ---------------------------------------------------------------------------
# 2x2 (Study 1 bar width) and 1-row (thinner bars)
# ---------------------------------------------------------------------------

p2 <- tag_panels(compact = FALSE)
caps <- lapply(p2, function(p) list(title = p$labels$title, subtitle = p$labels$subtitle))
untitled <- lapply(p2, function(p) {
  p + labs(title = NULL, subtitle = NULL) +
    theme(plot.margin = margin(1, 3, 0, 2))
})
tight <- lapply(untitled, tighten_y_axis)
# cowplot:: so a prior library(patchwork) cannot mask this with patchwork::align_plots.
aligned_2x2 <- do.call(cowplot::align_plots, c(tight, list(align = "hv", axis = "tblr")))
label_args <- list(
  label_size = 10,
  label_fontface = "bold",
  label_x = 0.01,
  label_y = 0.99,
  hjust = 0,
  vjust = 1
)
top_row <- do.call(plot_grid, c(
  list(
    add_panel_header(aligned_2x2[[1]], caps$A$title, caps$A$subtitle),
    add_panel_header(aligned_2x2[[2]], caps$B$title, caps$B$subtitle),
    nrow = 1,
    labels = c("A", "B")
  ),
  label_args
))
bottom_row <- do.call(plot_grid, c(
  list(
    add_panel_header(aligned_2x2[[3]], caps$C$title, caps$C$subtitle),
    add_panel_header(aligned_2x2[[4]], caps$D$title, caps$D$subtitle),
    nrow = 1,
    labels = c("C", "D")
  ),
  label_args
))
fig_2x2 <- plot_grid(
  top_row,
  ggdraw(),
  bottom_row,
  ncol = 1,
  rel_heights = c(1, ROW_GAP, 1)
)

p1 <- tag_panels(compact = TRUE)
fig_1row <- plot_grid(
  p1$A, p1$B, p1$C, p1$D,
  nrow = 1, align = "none",
  labels = c("A", "B", "C", "D"),
  label_size = 9,
  label_fontface = "bold",
  label_x = 0.01,
  label_y = 0.99,
  hjust = 0,
  vjust = 1
)

save_plot(
  file.path(output_dir, "fig-participant-reasoning-2x2.png"),
  fig_2x2,
  base_width = FIGURE_WIDTH_DOUBLE, base_height = FIGURE_HEIGHT_2X2,
  dpi = FIGURE_DPI, bg = "white"
)
cat("Saved: output/study_2/figures/fig-participant-reasoning-2x2.png\n")

save_plot(
  file.path(output_dir, "fig-participant-reasoning-1row.png"),
  fig_1row,
  base_width = FIGURE_WIDTH_1ROW, base_height = 2.4,
  dpi = FIGURE_DPI, bg = "white"
)
cat("Saved: output/study_2/figures/fig-participant-reasoning-1row.png\n")

save_paper_pdf(
  fig_2x2, "fig-participant-reasoning.pdf",
  width = FIGURE_WIDTH_DOUBLE, height = FIGURE_HEIGHT_2X2, output_dir = output_dir
)

