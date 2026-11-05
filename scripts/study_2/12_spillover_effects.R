# =============================================================================
# 12 · Spillover effects (beliefs and behaviour)
# =============================================================================
# Re-fits the preregistered models from 04_main_analyses.R (no dependency on
# that script having been sourced) and prints spillover console matrices:
#   belief ATE and Cohen's d grids (all treatment arms × outcomes)
#   behaviour spillover tables (ATE, d, OR/RR for binary outcomes)
# Also writes Holm-adjusted detailed grids and treatment-effect matrix figures.
#
# Model (each outcome):
#   outcome ~ condition * centered pretreatment belief + order
#   HC2 SEs; control is reference; condition main effect = ATE at mean pre.
#
# Figures (output/study_2/figures/):
#   fig-belief-te-matrix-d.png
#   fig-belief-te-matrix-db.png
#   fig-behavior-te-matrix.png
#   fig-te-matrix-belief-behavior.png
#   fig-te-matrix.pdf (double column, also copied to paper/figures/study_2/)

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggtext)
library(patchwork)
library(estimatr)
library(here)

source(here("scripts", "functions", "helpers.R"))
source(here("scripts", "functions", "study2_style.R"))
source(here("scripts", "functions", "study2_te_helpers.R"))
source(here("scripts", "functions", "study2_outcomes.R"))

options(width = max(getOption("width"), 220L))

# condition_labels from study2_style.R; fit_prereg / collect_ates from
# study2_te_helpers.R; outcome columns from study2_outcomes.R
# (no need to source 04_main_analyses.R).

primary_treat <- study2_primary_treat
treat_levels <- study2_treat_levels
belief_defs <- study2_belief_defs_t1() |> dplyr::select(key, pre, change, short)
behavior_defs <- study2_behavior_defs_t1(with_x_lab = FALSE)
print(as.data.frame(belief_defs), row.names = FALSE)
print(as.data.frame(behavior_defs), row.names = FALSE)

d1 <- readRDS(here("data", "study_2", "study2_d1.rds")) |>
  mutate(
    condition = factor(
      condition,
      levels = c("control", "meat", "charity", "polviol", "lying")
    ),
    order = factor(order),
    voucher_is_veg = as.integer(
      coalesce(voucher_DV_beeffirst, voucher_DV_vegfirst) == 1
    ),
    coin_heads = as.integer(lying_DV == 1)
  )

output_dir <- here("output", "study_2", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
FIGURE_DPI <- 300

domain_order <- c("meat", "charity", "lying", "polviol")
domain_labels <- c(
  meat    = "Meat",
  charity = "Charity",
  lying   = "Lying",
  polviol = "Political Violence"
)
domain_labels_x <- c(
  meat    = "Meat",
  charity = "Charity",
  lying   = "Lying",
  polviol = "Political\nViolence"
)

# --- Spillover display (matrices keep all arms × outcomes) -------------------

fmt_cell <- function(est, p) {
  stars <- fmt_sig(p)
  base <- sprintf("%.2f", est)
  ifelse(
    !is.na(p) & p < 0.05,
    paste0("**", base, "** ", stars),
    paste0(base, stars)
  )
}

print_spillover_tables <- function(ates) {
  disp <- ates |>
    mutate(
      est_disp = if_else(type == "binary", 100 * estimate, estimate),
      cell_est = fmt_cell(est_disp, p),
      cell_d   = fmt_cell(d, p),
      cell_or  = if_else(
        type == "binary" & !is.na(or),
        fmt_cell(or, p),
        "—"
      ),
      cell_rr  = if_else(
        type == "binary" & !is.na(rr),
        fmt_cell(rr, p),
        "—"
      ),
      cell_num = sprintf(
        "%.2f [%s]%s",
        est_disp, fmt_p(p), fmt_sig(p)
      )
    )

  mk_wide <- function(cell_col) {
    disp |>
      select(outcome_lab, treat_lab, cell = all_of(cell_col)) |>
      pivot_wider(names_from = treat_lab, values_from = cell) |>
      rename(Outcome = outcome_lab) |>
      select(
        Outcome,
        any_of(c("Meat", "Charity", "Lying", "Political Violence"))
      )
  }

  cat("\nTreatment–control ATEs from preregistered models (rows = outcomes).\n")
  cat("Binary outcomes in percentage points. Bold + * = p < .05 (two-sided).\n")
  cat("Diagonal cells are primary estimands.\n\n")
  print(as.data.frame(mk_wide("cell_est")), row.names = FALSE)

  cat("\nCohen's d (coefficient / control-arm time-1 SD, 0/1 scale for binary outcomes; bold + * = p < .05):\n")
  print(as.data.frame(mk_wide("cell_d")), row.names = FALSE)

  if (any(ates$type == "binary")) {
    cat("\nOdds ratio (LPM-adjusted rates; bold + * = LPM p < .05):\n")
    print(as.data.frame(mk_wide("cell_or")), row.names = FALSE)

    cat("\nRisk ratio (LPM-adjusted P(treat) / P(control); bold + * = LPM p < .05):\n")
    print(as.data.frame(mk_wide("cell_rr")), row.names = FALSE)
  }

  cat("\nCompanion (ATE [p]):\n")
  print(as.data.frame(mk_wide("cell_num")), row.names = FALSE)
}

# --- Fit preregistered models & console spillover tables ---------------------

cat("Analysis sample (d1 completers): N =", nrow(d1), "\n")

# d = coefficient / SD of the pretreatment item on this full baseline sample,
# condition ignored.
sd_belief <- stats::setNames(
  vapply(belief_defs$pre, function(v) study2_d_sd(d1, v, "pre"), numeric(1)),
  belief_defs$key
)
belief_models <- fit_prereg_models(d1, belief_defs, outcome_col = "change")
all_ates <- collect_ates(
  belief_models, belief_defs, treat_levels, primary_treat, condition_labels,
  d_from = "pre", sd_by_key = sd_belief
)

section("ALL TREATMENT EFFECTS (INCLUDING SPILLOVERS)")

cat("\nTreatment–control ATEs from preregistered models (rows = outcomes).\n")
cat("Bold + * = p < .05 (two-sided). Diagonal cells are primary estimands.\n\n")

spill_wide <- all_ates |>
  mutate(cell = fmt_cell(estimate, p)) |>
  select(outcome_lab, treat_lab, cell) |>
  pivot_wider(names_from = treat_lab, values_from = cell) |>
  rename(Outcome = outcome_lab) |>
  select(
    Outcome,
    any_of(c("Meat", "Charity", "Lying", "Political Violence"))
  )
print(as.data.frame(spill_wide), row.names = FALSE)

cat("\nCohen's d (coefficient / full-baseline pretreatment SD, condition ignored; bold + * = p < .05):\n")
spill_d <- all_ates |>
  mutate(cell = fmt_cell(d, p)) |>
  select(outcome_lab, treat_lab, cell) |>
  pivot_wider(names_from = treat_lab, values_from = cell) |>
  rename(Outcome = outcome_lab) |>
  select(
    Outcome,
    any_of(c("Meat", "Charity", "Lying", "Political Violence"))
  )
print(as.data.frame(spill_d), row.names = FALSE)

cat("\nCompanion (ATE [p]):\n")
spill_numeric <- all_ates |>
  mutate(cell = sprintf("%.2f [%s]%s", estimate, fmt_p(p), fmt_sig(p))) |>
  select(outcome_lab, treat_lab, cell) |>
  pivot_wider(names_from = treat_lab, values_from = cell) |>
  rename(Outcome = outcome_lab) |>
  select(
    Outcome,
    any_of(c("Meat", "Charity", "Lying", "Political Violence"))
  )
print(as.data.frame(spill_numeric), row.names = FALSE)

# No behavior was measured before the conversation. d uses the control arm's
# time-1 SD of that outcome, including the 0/1 voucher and coin flip.
sd_beh <- study2_behavior_d_sds(d1)
beh_models <- fit_prereg_models(d1, behavior_defs, outcome_col = "outcome")
beh_ates <- collect_ates(
  beh_models, behavior_defs, treat_levels, primary_treat, condition_labels,
  d_from = "out", sd_by_key = sd_beh
)

section("BEHAVIOUR: ALL TREATMENT EFFECTS (INCLUDING SPILLOVERS)")
print_spillover_tables(beh_ates)

# --- Matrix figures (former 04b) ---------------------------------------------

assign_inference <- function(dat) {
  dat$own_domain <- dat$treat == dat$outcome
  dat$p_holm <- NA_real_
  off <- !dat$own_domain
  dat$p_holm[off] <- p.adjust(dat$p[off], method = "holm")
  dat$treat <- factor(dat$treat, levels = domain_order)
  dat$outcome <- factor(dat$outcome, levels = domain_order)
  dat
}

pretty_num <- function(x, digits) {
  s <- sprintf(paste0("%+.", digits, "f"), x)
  gsub("-", "\u2212", s, fixed = TRUE)
}

stars_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
}

scale_limit <- function(x, step = 0.1) {
  m <- max(abs(x), na.rm = TRUE)
  lim <- ceiling(m / step) * step
  max(step, round(lim, 10))
}

b_label <- function(estimate, type) {
  case_when(
    type == "binary" ~ paste0(pretty_num(100 * estimate, 1), " pp"),
    type == "cents" ~ paste0(pretty_num(estimate, 2), "\u00a2"),
    type == "points" ~ paste0(pretty_num(estimate, 1), " pts"),
    TRUE ~ pretty_num(estimate, 1)
  )
}

style_cells <- function(dat, fill_var, fill_lim) {
  dat |>
    mutate(
      fill_value = .data[[fill_var]],
      stars = stars_p(p),
      text_col = ifelse(abs(fill_value) >= 0.85 * fill_lim, "white", "#1c1c1c"),
      text_face = ifelse(p < 0.05, "bold", "plain"),
      d_text = paste0(pretty_num(d, 2), stars),
      db_text = paste0("d = ", pretty_num(d, 2), stars, "\nb = ", b_text),
      b_only = paste0(b_text, stars),
      b_plain = paste0(b_num, stars)
    )
}

fmt_p_print <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.0001, "<.0001", sprintf("%.4f", p)))
}

print_grid <- function(dat, title, note) {
  section(title)
  cat(
    "\n", note, "\n",
    "Diagonal p is unadjusted. p_holm is Holm across the 12 off-diagonal cells.\n\n",
    sep = ""
  )
  show <- dat |>
    mutate(
      treat = domain_labels[as.character(treat)],
      outcome = domain_labels[as.character(outcome)],
      estimate = round(estimate, 3),
      t = round(t, 2),
      d = round(d, 3),
      p = fmt_p_print(p),
      p_holm = fmt_p_print(p_holm)
    ) |>
    select(treat, outcome, own_domain, n, estimate, t, d, p, p_holm)
  print(as.data.frame(show), row.names = FALSE)

  lost <- dat |> filter(!own_domain, p < 0.05, p_holm >= 0.05)
  cat("\nOff-diagonal cells with unadjusted p < .05 that do not survive Holm:\n")
  if (nrow(lost) == 0) {
    cat("  none\n")
  } else {
    print(
      as.data.frame(
        lost |>
          transmute(
            treat = domain_labels[as.character(treat)],
            outcome = domain_labels[as.character(outcome)],
            estimate = round(estimate, 2),
            t = round(t, 2),
            p = round(p, 4),
            p_holm = round(p_holm, 4)
          )
      ),
      row.names = FALSE
    )
  }
}

belief_ates <- all_ates |>
  mutate(b_text = pretty_num(estimate, 1), b_num = b_text) |>
  assign_inference()

behavior_ates <- beh_ates |>
  mutate(
    type = behavior_defs$type[match(as.character(outcome), behavior_defs$key)],
    b_text = b_label(estimate, type),
    b_num = case_when(
      type == "binary" ~ pretty_num(100 * estimate, 1),
      type == "cents" ~ pretty_num(estimate, 2),
      TRUE ~ pretty_num(estimate, 1)
    )
  ) |>
  assign_inference()

print_grid(
  belief_ates,
  "BELIEF TREATMENT EFFECTS VS CONTROL",
  "Cohen's d = model coefficient / SD of the pretreatment belief on the full baseline sample, condition ignored. b is in points."
)
print_grid(
  behavior_ates,
  "BEHAVIOR TREATMENT EFFECTS VS CONTROL",
  paste(
    "Coefficient from the linear model (LPM for voucher and coin flip).",
    "Displayed in percentage points, cents, or points. Color uses the t statistic."
  )
)

d_lim_belief <- scale_limit(belief_ates$d, step = 0.1)
t_lim_behavior <- scale_limit(behavior_ates$t, step = 2)

belief_cells <- style_cells(belief_ates, "d", d_lim_belief)
behavior_cells <- style_cells(behavior_ates, "t", t_lim_behavior)

fill_scale <- function(lim, name, digits = 2, horizontal = FALSE) {
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-lim, lim),
    breaks = seq(-lim, lim, by = lim / 2),
    labels = function(x) pretty_num(x, digits),
    name = name,
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0,
      barwidth = if (horizontal) unit(3.2, "cm") else unit(0.42, "cm"),
      barheight = if (horizontal) unit(0.22, "cm") else unit(4.6, "cm"),
      frame.colour = "#4d4d4d",
      frame.linewidth = 0.25,
      ticks.colour = "#4d4d4d",
      ticks.linewidth = 0.25
    )
  )
}

theme_matrix <- function(title_hjust = 0, axis_size = 11) {
  theme_minimal(base_size = 12, base_family = "Helvetica") +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(
        color = "#1c1c1c", size = axis_size, face = "bold",
        family = "Helvetica", lineheight = 0.95,
        margin = margin(t = 6)
      ),
      axis.text.y = element_text(
        color = "#1c1c1c", size = axis_size, face = "bold",
        family = "Helvetica", margin = margin(r = 6), hjust = 1
      ),
      axis.title = element_text(color = "#111111", face = "bold", size = axis_size + 1),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.title = element_text(
        face = "bold", size = 15, hjust = title_hjust, color = "#111111",
        margin = margin(b = 3)
      ),
      plot.subtitle = element_markdown(
        color = "#333333", size = 9.5, hjust = title_hjust, lineheight = 1.25,
        margin = margin(b = 10)
      ),
      plot.caption = element_markdown(
        color = "#5c5c5c", size = 8, hjust = 0, lineheight = 1.2,
        margin = margin(t = 10)
      ),
      legend.title = element_text(face = "bold", size = 10, color = "#111111"),
      legend.text = element_text(size = 8.5, color = "#222222"),
      legend.position = "right",
      plot.margin = margin(12, 14, 10, 12),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

make_matrix <- function(dat, mode = c("d", "db", "b", "bp"), fill_lim, fill_name,
                        fill_digits = 2,
                        title, subtitle, caption, x_title,
                        label_size = NULL, axis_size = 11, title_hjust = 0,
                        legend_bottom = FALSE,
                        x_labels = domain_labels_x, y_labels = domain_labels,
                        show_legend = TRUE, tile_gap = 1.5, fixed = TRUE) {
  mode <- match.arg(mode)
  if (is.null(label_size)) {
    label_size <- if (mode == "db") 3.45 else 4.2
  }
  dat <- dat |>
    mutate(
      label = switch(mode, d = d_text, db = db_text, b = b_only, bp = b_plain),
      label_size = label_size
    )

  ggplot(dat, aes(x = outcome, y = treat, fill = fill_value)) +
    geom_tile(color = "white", linewidth = tile_gap, linejoin = "mitre") +
    geom_tile(
      data = function(x) dplyr::filter(x, own_domain),
      fill = NA, color = "#161616", linewidth = 0.7,
      width = 0.90, height = 0.90
    ) +
    geom_text(
      aes(label = label, color = text_col, fontface = text_face, size = label_size),
      family = "Helvetica", lineheight = 0.96
    ) +
    scale_size_identity() +
    scale_color_identity() +
    fill_scale(fill_lim, fill_name, fill_digits, horizontal = legend_bottom) +
    (if (show_legend) NULL else guides(fill = "none")) +
    scale_x_discrete(labels = function(x) x_labels[x], expand = expansion(add = 0.02)) +
    scale_y_discrete(
      labels = function(x) y_labels[x],
      limits = rev(domain_order),
      expand = expansion(add = 0.02)
    ) +
    (if (fixed) coord_fixed(clip = "off") else coord_cartesian(clip = "off")) +
    labs(
      title = title, subtitle = subtitle, caption = caption,
      x = x_title, y = "Treatment arm"
    ) +
    theme_matrix(title_hjust = title_hjust, axis_size = axis_size)
}

caption_holm <- paste(
  "Stars use unadjusted *p* in every cell.",
  "*p* < .05, .01, .001. Boxes mark own-domain effects.",
  sep = "<br>"
)

caption_belief_d <- paste(
  "Belief change ~ condition \u00d7 centered pretreatment belief + question order, HC2 SEs.",
  "Cohen's *d* = effect / SD of the pretreatment belief.",
  caption_holm,
  sep = "<br>"
)

caption_belief_db <- paste(
  "Belief change ~ condition \u00d7 centered pretreatment belief + question order, HC2 SEs.",
  "Cohen's *d* = effect / SD of the pretreatment belief. *b* is the effect in points.",
  caption_holm,
  sep = "<br>"
)

caption_behavior <- paste(
  "Linear model: behavior ~ condition \u00d7 centered pretreatment belief + question order, HC2 SEs.",
  "Voucher and coin flip are linear probability models. Numbers are percentage points, cents, or points.",
  "Color is the *t* statistic from that model.",
  caption_holm,
  sep = "<br>"
)

sub_belief <- "Positive = greater agreement that the act is morally wrong, versus control."
sub_behavior <- paste(
  "Linear-model coefficients. Voucher and coin flip are linear probability models.",
  "Positive = a higher score versus control.",
  sep = "<br>"
)

fig_belief_d <- make_matrix(
  belief_cells, "d", d_lim_belief, "Cohen's d",
  title = "Treatment effects on moral beliefs",
  subtitle = paste("Cohen's *d* versus control.", sub_belief, sep = "<br>"),
  caption = caption_belief_d,
  x_title = "Belief outcome"
)

fig_belief_db <- make_matrix(
  belief_cells, "db", d_lim_belief, "Cohen's d",
  title = "Treatment effects on moral beliefs",
  subtitle = paste(
    "Each cell is the effect versus control: Cohen's *d*, then the unstandardized effect *b*.",
    sub_belief,
    sep = "<br>"
  ),
  caption = caption_belief_db,
  x_title = "Belief outcome"
)

fig_behavior <- make_matrix(
  behavior_cells, "b", t_lim_behavior, "t statistic", fill_digits = 0,
  title = "Treatment effects on behavior",
  subtitle = sub_behavior,
  caption = caption_behavior,
  x_title = "Behavioral outcome"
)

fig_both <- (
  make_matrix(
    belief_cells, "d", d_lim_belief, "Cohen's d",
    title = "Belief",
    subtitle = "Cohen's *d*. Positive = greater agreement that the act is morally wrong.",
    caption = NULL,
    x_title = "Belief outcome",
    label_size = 3.6, axis_size = 9.5, title_hjust = 0.5
  ) +
    make_matrix(
      behavior_cells, "b", t_lim_behavior, "t statistic", fill_digits = 0,
      title = "Behavior",
      subtitle = "Coefficient (pp, cents, or points). Color is the linear-model *t*.",
      caption = NULL,
      x_title = "Behavioral outcome",
      label_size = 3.15, axis_size = 9.5, title_hjust = 0.5
    )
) +
  plot_layout(widths = c(1, 1.05)) +
  plot_annotation(
    title = "Treatment effects versus control",
    caption = paste(
      "Belief color is Cohen's *d* (effect / SD of the pretreatment belief).",
      "Behavior numbers are linear-model coefficients: percentage points for the voucher and coin flip, cents for the donation, points for joining a crowd. Behavior color is the *t* statistic.",
      "Stars use unadjusted *p* in every cell.",
      sep = "<br>"
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold", size = 16, family = "Helvetica", color = "#111111",
        margin = margin(b = 6)
      ),
      plot.caption = element_markdown(
        size = 8, family = "Helvetica", color = "#5c5c5c", hjust = 0,
        lineheight = 1.25, margin = margin(t = 8)
      ),
      plot.margin = margin(8, 10, 8, 10)
    )
  )

save_matrix <- function(plot, filename, width, height) {
  ggsave(
    file.path(output_dir, filename),
    plot,
    width = width, height = height, dpi = FIGURE_DPI, bg = "white"
  )
  cat("Saved: output/study_2/figures/", filename, "\n", sep = "")
}

save_matrix(fig_belief_d, "fig-belief-te-matrix-d.png", 8.5, 7.3)
save_matrix(fig_belief_db, "fig-belief-te-matrix-db.png", 8.7, 7.5)
save_matrix(fig_behavior, "fig-behavior-te-matrix.png", 8.7, 7.6)
save_matrix(fig_both, "fig-te-matrix-belief-behavior.png", 14.2, 7.4)

behavior_labels_x <- c(
  meat    = "Dietary\nchoice (pp)",
  charity = "Donation\n(\u00a2)",
  lying   = "Lie rate\n(pp)",
  polviol = "Hypoth. PV\nparticipation"
)
arm_labels_2line <- c(
  meat    = "Meat",
  charity = "Charity",
  lying   = "Lying",
  polviol = "Political\nViolence"
)

paper_matrix_theme <- function(axis_size, title_size) {
  theme(
    plot.title = element_text(face = "bold", size = title_size, hjust = 0.5,
                              lineheight = 0.95, margin = margin(b = 3)),
    axis.title.x = element_text(face = "bold", size = axis_size, margin = margin(t = 4)),
    axis.title.y = element_text(face = "bold", size = axis_size, margin = margin(r = 3)),
    axis.text.x = element_text(size = axis_size - 0.5, face = "bold",
                               lineheight = 0.9, margin = margin(t = 2)),
    axis.text.y = element_text(size = axis_size, face = "bold", lineheight = 0.9,
                               margin = margin(r = 2)),
    plot.margin = margin(4, 2, 2, 2)
  )
}

build_paper_matrix <- function(label_size, axis_size, title_size, tag_size,
                               tile_gap, gap_pt) {
  common <- list(
    caption = NULL, subtitle = NULL,
    axis_size = axis_size, title_hjust = 0.5, show_legend = FALSE,
    label_size = label_size, y_labels = arm_labels_2line,
    tile_gap = tile_gap, fixed = FALSE
  )
  belief <- do.call(make_matrix, c(
    list(belief_cells, "d", d_lim_belief, "Cohen's d",
         title = "Moral Belief\nTreatment Effects",
         x_title = "Moral Belief measures"),
    common
  )) +
    paper_matrix_theme(axis_size, title_size) +
    paper_tag_theme(size = tag_size) +
    labs(tag = "A", y = "Condition")
  behavior <- do.call(make_matrix, c(
    list(behavior_cells, "bp", t_lim_behavior, "t statistic", fill_digits = 0,
         title = "Behavior\nTreatment Effects",
         x_title = "Moral Behavior measures",
         x_labels = behavior_labels_x),
    common
  )) +
    paper_matrix_theme(axis_size, title_size) +
    paper_tag_theme(size = tag_size) +
    labs(tag = "B") +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
          plot.margin = margin(4, 2, 2, gap_pt))
  belief + behavior + plot_layout(widths = c(1, 1))
}

use_quartz_text_metrics()

fig_paper <- build_paper_matrix(
  label_size = 3.2, axis_size = 8.5, title_size = 9.5, tag_size = 10,
  tile_gap = 1.5, gap_pt = 16
)
save_paper_pdf(
  fig_paper, "fig-te-matrix.pdf", width = 7.08, height = 4.35,
  output_dir = output_dir
)

old_figs <- c(
  "fig-belief-te-matrix-split.png",
  "fig-behavior-te-matrix-d.png",
  "fig-behavior-te-matrix-db.png",
  "fig-te-matrix-col.pdf",
  "fig-te-matrix-col.png"
)
for (f in old_figs) {
  path <- file.path(output_dir, f)
  if (file.exists(path)) file.remove(path)
}

cat("\nSpillover analyses and TE matrix figures complete.\n")
