# =============================================================================
# 05 · Main-text figures by condition
# =============================================================================
# Paper figures, saved to output/figures/:
#   fig-meat.pdf and fig-charity.pdf are the wide two-wave figures
#   fig-lying.pdf and fig-polviol.pdf are the one-column, one-row figures
# Exploratory figure variants (not paper) live under extra_analyses/ if present.

library(dplyr)
library(ggplot2)
library(cowplot)
library(estimatr)
library(here)

source(here("scripts", "functions", "helpers.R"))
source(here("scripts", "functions", "study2_style.R"))

s1_t1 <- readRDS(here("data", "study_1", "d1.rds"))
s1_t2 <- readRDS(here("data", "study_1", "d.rds"))
s2_t1 <- readRDS(here("data", "study_2", "study2_d1.rds"))
s2_t2 <- readRDS(here("data", "study_2", "study2_d.rds"))

output_dir <- here("output", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Arial metrics for cowplot / ggplotGrob (helpers.R).
use_quartz_text_metrics()

# PNAS widths from study2_style.R. Two-wave figures use FIGURE_HEIGHT;
# the legend is the bottom LEGEND_REL of it.
FIGURE_WIDTH <- FIGURE_WIDTH_DOUBLE
FIGURE_WIDTH_COLUMN <- FIGURE_WIDTH_SINGLE
FIGURE_HEIGHT <- 2.7
LEGEND_REL <- 0.11
TWO_WAVE_Y_SPAN <- 0.605

# Labels from study2_style.R. Stack order keeps Political Violence above Lying
# so the two-wave and one-row paper figures match the existing layout.
cond_levels <- c(
  "Control", "Meat", "Charity", "Political Violence", "Lying"
)
other_levels <- setdiff(cond_levels, "Control")

# Gap between adjacent conditions, and the tighter gap between a Study 2
# point and its pilot diamond. These reproduce the pilot-small stack.
COND_GAP <- 0.05
PAIR_GAP <- 0.018

# Timepoint centers for the two-wave figures. Follow-up is shifted up by
# 0.05 inside place_points(), matching the pilot-small figures, so the
# shorter follow-up stack does not collide with post-treatment.
y_t1 <- 1.34
y_t2 <- 1

wave_labels <- c(
  "T1" = "Immediately\npost-treatment",
  "T2" = "1-month\nfollow-up"
)

# Control stays teal. Each non-focal arm has a blue it keeps whenever it
# is not the focal arm. Meat and charity share the dark blue in that map;
# when both are non-focal, charity takes the blue the focal arm freed.
established_blue <- c(
  "Meat"               = "#163E5C",
  "Charity"            = "#163E5C",
  "Political Violence" = "#8FB8D4",
  "Lying"              = "#3E7CA8"
)
blue_palette <- c("#163E5C", "#3E7CA8", "#8FB8D4")

colors_for <- function(focal) {
  others <- setdiff(other_levels, focal)
  assigned <- established_blue[others]
  if (any(duplicated(assigned))) {
    used <- assigned[names(assigned) != "Charity"]
    assigned["Charity"] <- setdiff(blue_palette, used)[1]
  }
  cols <- c("Control" = "#0E9B8A", assigned, setNames("#E23D32", focal))
  cols[cond_levels]
}

# Paper combo figures: Arial, no y-grid (unlike theme_study2_fig for SI).
theme_fig <- theme_minimal(base_size = 9, base_family = "Arial") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "grey30", linewidth = 0.4),
    axis.ticks = element_line(color = "grey30", linewidth = 0.3),
    axis.text = element_text(color = "grey20"),
    axis.title = element_text(color = "grey10", face = "bold", size = 8),
    plot.title = element_text(
      face = "bold", size = 8, hjust = 0.5,
      lineheight = 1.05, margin = margin(b = 2)
    ),
    legend.position = "none",
    plot.margin = margin(2, 8, 1, 3)
  )

tag_style <- paper_tag_theme()

# =============================================================================
# SUMMARIES
# =============================================================================

label_study2 <- function(dat) {
  dat |>
    mutate(condition_lab = unname(condition_labels[as.character(condition)]))
}

label_study1 <- function(dat) {
  dat |>
    mutate(condition_lab = as.character(condition))
}

summarise_wave <- function(dat, outcome_var, type, timepoint, study, focal) {
  dat |>
    filter(!is.na(.data[[outcome_var]]), !is.na(condition_lab)) |>
    group_by(condition_lab) |>
    summarise(
      n = n(),
      M = mean(as.numeric(.data[[outcome_var]]), na.rm = TRUE),
      SD = sd(as.numeric(.data[[outcome_var]]), na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      se = SD / sqrt(n),
      mean = if (type == "binary") 100 * M else M,
      se_plot = if (type == "binary") 100 * se else se,
      ci_lower = mean - 1.96 * se_plot,
      ci_upper = mean + 1.96 * se_plot,
      timepoint = timepoint,
      study = study,
      is_primary = condition_lab == focal
    )
}

# Colors from the study-design figure. A condition keeps its color on every
# plot; the focal arm is picked out by size and line weight, not by recoloring.
design_colors <- c(
  "Control"            = "#7F7F7F",
  "Meat"               = "#009E73",
  "Charity"            = "#0072B2",
  "Lying"              = "#E69F00",
  "Political Violence" = "#CC79A7"
)
# Bottom to top, matching the study-design figure from left to right.
design_order <- c(
  "Control", "Meat", "Charity", "Lying", "Political Violence"
)

# Bottom to top. cond_order overrides the default (control, focal, then the
# other arms). A condition in both studies is a tight pair.
stack_offsets <- function(dat, cond_order = NULL) {
  present <- dat |>
    distinct(condition_lab, study) |>
    mutate(condition_lab = as.character(condition_lab), study = as.character(study))
  has_pair <- tapply(present$study, present$condition_lab, function(s) {
    length(unique(s)) > 1
  })
  focal_now <- unique(dat$focal[1])
  conds <- if (is.null(cond_order)) {
    c("Control", focal_now, setdiff(other_levels, focal_now))
  } else {
    cond_order
  }
  conds <- conds[conds %in% names(has_pair)]
  slots <- unlist(lapply(conds, function(cond) {
    if (isTRUE(has_pair[[cond]])) c(paste0(cond, "|Study 2"), paste0(cond, "|Study 1")) else cond
  }), use.names = FALSE)

  pos <- numeric(length(slots))
  if (length(slots) > 1) {
    for (i in 2:length(slots)) {
      same_cond <- sub("\\|.*$", "", slots[i]) == sub("\\|.*$", "", slots[i - 1]) &&
        grepl("\\|", slots[i]) && grepl("\\|", slots[i - 1])
      pos[i] <- pos[i - 1] + if (same_cond) PAIR_GAP else COND_GAP
    }
  }
  pos <- pos - mean(range(pos))
  setNames(pos, slots)
}

place_points <- function(dat, focal, cond_order = NULL) {
  dat <- dat |>
    mutate(focal = focal)
  offsets <- stack_offsets(dat, cond_order)
  two_waves <- length(unique(dat$timepoint)) > 1
  dat |>
    mutate(
      slot = if_else(
        paste(as.character(condition_lab), as.character(study), sep = "|") %in% names(offsets),
        paste(as.character(condition_lab), as.character(study), sep = "|"),
        as.character(condition_lab)
      ),
      y_base = if (two_waves) {
        if_else(timepoint == "T1", y_t1, y_t2)
      } else {
        1
      },
      # Follow-up is a shorter stack (no political violence, no deceiving
      # others). Nudge it up so it keeps clear of the post-treatment cluster.
      y = y_base + unname(offsets[slot]) + if_else(two_waves & timepoint == "T2", 0.05, 0),
      condition_lab = factor(condition_lab, levels = cond_levels),
      study = factor(study, levels = c("Study 2", "Study 1")),
      is_primary = factor(is_primary, levels = c(FALSE, TRUE))
    )
}

# Padding around the outermost points, in y units. Matches the two-wave
# figures (0.047 below the lowest follow-up point, 0.032 above the
# highest post-treatment point).
y_scale_for <- function(dat) {
  if (length(unique(dat$timepoint)) > 1) {
    return(list(
      breaks = c(y_t1, y_t2),
      labels = c(wave_labels[["T1"]], wave_labels[["T2"]]),
      limits = c(0.885, y_t1 + 0.15)
    ))
  }
  list(
    breaks = 1,
    labels = wave_labels[["T1"]],
    limits = c(min(dat$y) - 0.047, max(dat$y) + 0.032)
  )
}

# =============================================================================
# PANELS
# =============================================================================

# Preregistered Study 2 contrast: focal arm vs control, at mean pretreatment
# belief. * / ** / *** or n.s. via fmt_bracket_sig() in helpers.R.
# The pilot is not part of the test.

contrast_p <- function(dat, outcome_var, pre_var, treat) {
  d <- dat |>
    filter(
      !is.na(.data[[outcome_var]]),
      !is.na(.data[[pre_var]]),
      !is.na(condition),
      !is.na(order)
    ) |>
    mutate(
      outcome = as.numeric(.data[[outcome_var]]),
      pre_belief_c = as.numeric(.data[[pre_var]]) -
        mean(as.numeric(.data[[pre_var]]), na.rm = TRUE),
      condition = relevel(factor(as.character(condition)), "control"),
      order = factor(order)
    )
  fit <- lm_robust(
    outcome ~ condition * pre_belief_c + order,
    data = d,
    se_type = "HC2"
  )
  term <- paste0("condition", treat)
  unname(summary(fit)$coefficients[term, "Pr(>|t|)"])
}

# Bracket beside the Study 2 control and focal points. sig_by_time is a
# named vector of labels, one per timepoint ("T1", "T2").
add_study2_brackets <- function(p, dat, focal, sig_by_time,
                                discrete = FALSE, y_order = NULL,
                                right_margin = 26, left_margin = NULL,
                                inside_label = FALSE) {
  if (is.null(sig_by_time) || length(sig_by_time) == 0) {
    return(p)
  }
  compared <- dat |>
    filter(as.character(condition_lab) %in% c("Control", focal))
  if ("study" %in% names(compared)) {
    compared <- compared |> filter(as.character(study) == "Study 2")
  }
  # Stacked panels share one x, just past the furthest bar, so the uprights
  # line up. Side-by-side panels each sit just past their own control and
  # focal bars.
  x_right <- if (inside_label) {
    max(compared$ci_upper, na.rm = TRUE)
  } else {
    max(dat$ci_upper, na.rm = TRUE)
  }
  if (discrete) {
    rows <- data.frame(
      y0 = match("Control", y_order),
      y1 = match(focal, y_order),
      lab = unname(sig_by_time[[1]]),
      stringsAsFactors = FALSE
    )
  } else {
    rows <- compared |>
      filter(timepoint %in% names(sig_by_time)) |>
      group_by(timepoint) |>
      summarise(y0 = min(y), y1 = max(y), .groups = "drop") |>
      mutate(lab = unname(sig_by_time[as.character(timepoint)]))
  }
  rows <- rows |>
    filter(!is.na(y0), !is.na(y1), !is.na(lab))
  if (nrow(rows) == 0) {
    return(p)
  }

  built <- ggplot_build(p)
  params <- built$layout$panel_params[[1]]
  xrng <- params$x.range
  yrng <- params$y.range
  span <- diff(xrng)
  pad <- if (inside_label) 0.045 * span else 0.012 * span
  leg <- 0.040 * span
  x_tick <- x_right + pad
  x_bar <- x_tick + leg
  # Compact row panels keep the stars inside the panel so the two graphs
  # can sit close together and the outer margins stay small.
  x_txt <- if (inside_label) x_bar + 0.010 * (x_bar - xrng[1]) else x_bar + 0.006 * span
  xlim_right <- if (inside_label) {
    x_txt + 0.13 * (x_bar - xrng[1])
  } else {
    x_bar + 0.008 * span
  }

  ticks <- rows |>
    rowwise() |>
    reframe(
      x = c(x_tick, x_bar, x_tick),
      xend = c(x_bar, x_bar, x_bar),
      y = c(y1, y1, y0),
      yend = c(y1, y0, y0)
    )

  p <- p +
    geom_segment(
      data = ticks,
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      linewidth = 0.3,
      color = "grey20"
    )
  for (i in seq_len(nrow(rows))) {
    p <- p + annotate(
      "text",
      x = x_txt,
      y = (rows$y0[i] + rows$y1[i]) / 2,
      label = rows$lab[i],
      hjust = 0, vjust = 0.5,
      size = if (identical(rows$lab[i], "n.s.")) 2.15 else 2.6,
      color = "grey20",
      family = "Arial"
    )
  }
  p +
    coord_cartesian(
      xlim = c(xrng[1], xlim_right),
      ylim = yrng, clip = "off", expand = FALSE
    ) +
    theme(plot.margin = margin(
      if (discrete) 2 else 2,
      right_margin,
      if (discrete) 1 else 1,
      if (is.null(left_margin)) if (discrete) 0 else 3 else left_margin
    ))
}

# Mean ± CI on x. y_mode = "stack" places Study 1/2 pairs on a continuous
# y from place_points(); "condition" puts condition names on a discrete y.
scale_x_outcome <- function(type, x_expand) {
  if (identical(type, "change")) {
    return(scale_x_continuous(expand = x_expand))
  }
  if (identical(type, "cents")) {
    return(scale_x_continuous(
      labels = function(x) paste0(x, "\u00A2"),
      expand = x_expand,
      limits = c(0, NA)
    ))
  }
  if (identical(type, "points")) {
    return(scale_x_continuous(expand = x_expand, limits = c(0, NA)))
  }
  # binary / percent: the % sits to the right of the number, so a small
  # shift puts the digits under the tick.
  list(
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = x_expand,
      limits = c(0, NA)
    ),
    theme(axis.text.x = element_text(hjust = 0.35))
  )
}

build_mean_ci_panel <- function(dat, title, x_lab, type, colors, focal,
                                y_mode = c("stack", "condition"),
                                y_scale = NULL, show_y = TRUE, pop = FALSE,
                                sig_by_time = NULL, sig_label = NULL,
                                compact = FALSE, y_expand = 0.4,
                                panel_margin = NULL, y_lab = NULL) {
  y_mode <- match.arg(y_mode)
  heavy_labs <- c("Control", focal)
  y_order <- NULL

  if (identical(y_mode, "condition")) {
    y_order <- dat |>
      distinct(condition_lab, y) |>
      arrange(y) |>
      mutate(condition_lab = as.character(condition_lab)) |>
      pull(condition_lab)
    if (pop) {
      dat <- dat |>
        mutate(
          condition_lab = factor(as.character(condition_lab), levels = y_order),
          mark = if_else(
            as.character(condition_lab) %in% c(focal, "Control"), "focal", "other"
          ),
          mark = factor(mark, levels = c("other", "focal"))
        ) |>
        arrange(mark, as.character(condition_lab) == focal)
      size_vals <- c(focal = 2.32, other = 1.45)
      line_vals <- c(focal = 0.8, other = 0.15)
    } else {
      dat <- dat |>
        mutate(
          condition_lab = factor(as.character(condition_lab), levels = y_order),
          mark = if_else(as.character(condition_lab) %in% heavy_labs, "heavy", "light")
        ) |>
        arrange(as.character(condition_lab) == focal)
      size_vals <- c(heavy = 2.15, light = 1.25)
      line_vals <- c(heavy = 0.7, light = 0.2)
    }
    base <- ggplot(dat, aes(x = mean, y = condition_lab, color = condition_lab))
  } else {
    if (pop) {
      # Control and the focal Study 2 arm share the heavy point and bar.
      # Everything else, including the focal pilot, is smaller, with a thin
      # bar. Pilots are upside-down triangles so the shape still reads at that size.
      dat <- dat |>
        mutate(
          mark = if_else(
            as.character(condition_lab) %in% c(focal, "Control") &
              as.character(study) == "Study 2",
            "focal",
            "other"
          ),
          mark = factor(mark, levels = c("other", "focal"))
        ) |>
        arrange(mark, as.character(condition_lab) == focal)
      size_vals <- c(focal = 2.32, other = 1.45)
      line_vals <- c(focal = 0.8, other = 0.15)
    } else {
      dat <- dat |>
        arrange(is_primary) |>
        mutate(
          mark = if_else(
            as.character(study) == "Study 2" &
              as.character(condition_lab) %in% heavy_labs,
            "heavy",
            "light"
          )
        )
      size_vals <- c(heavy = 2.15, light = 1.25)
      line_vals <- c(heavy = 0.7, light = 0.2)
    }
    base <- ggplot(dat, aes(x = mean, y = y, color = condition_lab, shape = study))
  }

  if (type == "change") {
    base <- base +
      geom_vline(
        xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.35
      )
  }
  base <- base +
    geom_errorbar(
      aes(xmin = ci_lower, xmax = ci_upper, linewidth = mark),
      width = 0, orientation = "y"
    ) +
    scale_size_manual(values = size_vals, guide = "none") +
    scale_linewidth_manual(values = line_vals, guide = "none")

  if (identical(y_mode, "stack")) {
    base <- base +
      geom_point(aes(size = mark, fill = condition_lab), stroke = 0.15) +
      scale_color_manual(values = colors, limits = cond_levels, drop = FALSE) +
      scale_fill_manual(values = colors, limits = cond_levels, drop = FALSE, guide = "none") +
      scale_shape_manual(values = c("Study 2" = 16, "Study 1" = 25), drop = FALSE) +
      scale_y_continuous(
        breaks = y_scale$breaks,
        labels = y_scale$labels,
        limits = y_scale$limits,
        expand = expansion(mult = 0, add = 0)
      ) +
      labs(x = x_lab, y = NULL, title = title) +
      theme_fig +
      theme(
        axis.text.y = element_text(size = 6.5, lineheight = 0.92),
        plot.subtitle = element_text(
          size = 6.5, hjust = 0.5, color = "grey30", margin = margin(b = 1)
        )
      )
    right_pad <- if (is.null(sig_by_time)) 0.08 else 0.20
    x_expand <- if (type == "change") {
      expansion(mult = c(0.06, right_pad))
    } else {
      expansion(mult = c(0.02, right_pad))
    }
  } else {
    base <- base +
      geom_point(aes(size = mark)) +
      scale_color_manual(values = colors, limits = y_order, drop = FALSE) +
      scale_y_discrete(drop = FALSE, expand = expansion(add = y_expand)) +
      labs(x = x_lab, y = y_lab, title = title) +
      theme_fig +
      theme(
        axis.text.y = element_text(
          size = if (compact) 6 else 6.5,
          lineheight = 0.92,
          margin = margin(r = if (compact) 1 else 2)
        ),
        axis.title.y = element_text(
          size = if (compact) 6 else 7.5,
          margin = margin(r = 3)
        ),
        axis.title.x = element_text(
          size = if (compact) 6 else 7.5,
          lineheight = 0.95
        ),
        plot.title = element_text(
          size = if (compact) 6.5 else 7.5,
          hjust = 0.5, lineheight = 1.05, margin = margin(b = 2)
        ),
        plot.margin = if (is.null(panel_margin)) {
          margin(2, if (compact) 4 else 6, 1, if (is.null(y_lab)) 1 else 4)
        } else {
          panel_margin
        }
      )
    right_pad <- if (is.null(sig_label)) 0.10 else if (compact) 0.08 else 0.24
    x_expand <- if (type == "change") {
      expansion(mult = c(if (compact) 0.02 else 0.06, right_pad))
    } else {
      expansion(mult = c(if (compact) 0.01 else 0.02, right_pad))
    }
  }

  x_layers <- scale_x_outcome(type, x_expand)
  if (inherits(x_layers, "Scale")) {
    base <- base + x_layers
  } else {
    for (ly in x_layers) base <- base + ly
  }

  if (!show_y) {
    if (identical(y_mode, "stack")) {
      base <- base + theme(
        axis.text.y = element_text(color = NA, size = 6.5),
        axis.ticks.y = element_blank()
      )
    } else {
      base <- base + theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    }
  }

  if (identical(y_mode, "stack")) {
    return(add_study2_brackets(base, dat, focal, sig_by_time, discrete = FALSE))
  }
  if (is.null(sig_label)) {
    return(base)
  }
  add_study2_brackets(
    base, dat, focal, c(T1 = sig_label),
    discrete = TRUE, y_order = y_order,
    right_margin = if (compact) 8 else 26,
    left_margin = if (!is.null(y_lab)) 5 else if (compact) 1 else NULL,
    inside_label = compact
  )
}

build_panel <- function(dat, title, x_lab, type, colors, focal,
                        y_scale, show_y = TRUE, pop = FALSE,
                        sig_by_time = NULL) {
  build_mean_ci_panel(
    dat, title, x_lab, type, colors, focal,
    y_mode = "stack", y_scale = y_scale, show_y = show_y, pop = pop,
    sig_by_time = sig_by_time
  )
}

# Legend order follows the point stack, bottom to top. Study 2 is the
# unlabeled default; the pilot is tagged, and only for arms that have one.
make_legend <- function(dat, colors, focal, legend_nrow = 1,
                        order_conds = NULL, pop = FALSE) {
  if (is.null(order_conds)) {
    order_conds <- c("Control", focal, setdiff(other_levels, focal))
  }
  present <- dat |>
    distinct(condition_lab, study) |>
    mutate(
      condition_lab = as.character(condition_lab),
      study = as.character(study)
    )
  legend_df <- present |>
    mutate(ord = match(condition_lab, order_conds)) |>
    arrange(ord, desc(study)) |>
    mutate(
      series = if_else(
        study == "Study 1",
        paste0(condition_lab, " (Pilot)"),
        condition_lab
      ),
      shape = if_else(study == "Study 1", 25, 16),
      color = unname(colors[condition_lab]),
      heavy = condition_lab %in% c(focal, "Control")
    )
  legend_df$series <- factor(legend_df$series, levels = legend_df$series)
  legend_df$point_size <- if (pop) {
    ifelse(
      legend_df$condition_lab %in% c(focal, "Control") & legend_df$study == "Study 2",
      2,
      1.35
    )
  } else {
    dplyr::case_when(
      legend_df$study == "Study 1" ~ 1.7,
      legend_df$heavy ~ 2.15,
      TRUE ~ 1.3
    )
  }

  legend_plot <- ggplot(
    legend_df,
    aes(x = 1, y = series, color = series, fill = series, shape = series, size = series)
  ) +
    geom_point(stroke = 0.15) +
    scale_color_manual(values = setNames(legend_df$color, legend_df$series), name = NULL) +
    scale_fill_manual(values = setNames(legend_df$color, legend_df$series), name = NULL) +
    scale_shape_manual(values = setNames(legend_df$shape, legend_df$series), name = NULL) +
    scale_size_manual(values = setNames(legend_df$point_size, legend_df$series), name = NULL) +
    guides(
      color = guide_legend(nrow = legend_nrow, byrow = TRUE),
      fill = guide_legend(nrow = legend_nrow, byrow = TRUE),
      shape = guide_legend(nrow = legend_nrow, byrow = TRUE),
      size = guide_legend(nrow = legend_nrow, byrow = TRUE)
    ) +
    theme_fig +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 6),
      legend.key.size = unit(0.28, "cm"),
      legend.key.width = unit(0.28, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0.08, "cm")
    )

  cowplot::get_legend(legend_plot)
}

assemble_wave_column <- function(belief_dat, behaviour_dat, belief_title,
                                 behaviour_title, behaviour_x, behaviour_type,
                                 colors, focal, pop = FALSE, order_conds = NULL,
                                 sig_belief = NULL, sig_behaviour = NULL) {
  y_scale <- y_scale_for(belief_dat)
  belief_panel <- build_panel(
    belief_dat, belief_title, "Belief change (post \u2212 pre)", "change",
    colors, focal, y_scale,
    pop = pop, sig_by_time = sig_belief
  ) +
    labs(tag = "A") + tag_style
  behaviour_panel <- build_panel(
    behaviour_dat, behaviour_title, behaviour_x, behaviour_type,
    colors, focal, y_scale,
    pop = pop, sig_by_time = sig_behaviour
  ) +
    labs(tag = "B") + tag_style
  # Seven legend entries do not fit on one row at single-column width.
  legend_in <- if (pop) 0.72 else 0.58
  panel_in <- 4.15
  fig <- plot_grid(
    belief_panel, behaviour_panel,
    make_legend(belief_dat, colors, focal, legend_nrow = 2, order_conds = order_conds, pop = pop),
    ncol = 1, align = "v", axis = "lr",
    rel_heights = c(1, 1, legend_in / (panel_in / 2))
  )
  list(fig = fig, height = panel_in + legend_in)
}

assemble_fig <- function(belief_dat, behaviour_dat, belief_title, behaviour_title,
                         behaviour_x, behaviour_type, colors, focal,
                         legend_rel = LEGEND_REL, pop = FALSE, order_conds = NULL,
                         sig_belief = NULL, sig_behaviour = NULL) {
  y_scale <- y_scale_for(belief_dat)
  belief_panel <- build_panel(
    belief_dat, belief_title, "Belief change (post \u2212 pre)", "change",
    colors, focal, y_scale,
    pop = pop, sig_by_time = sig_belief
  ) +
    labs(tag = "A") + tag_style
  behaviour_panel <- build_panel(
    behaviour_dat, behaviour_title, behaviour_x, behaviour_type,
    colors, focal, y_scale,
    pop = pop, sig_by_time = sig_behaviour
  ) +
    labs(tag = "B") + tag_style

  plot_grid(
    plot_grid(belief_panel, behaviour_panel, nrow = 1, align = "h"),
    make_legend(belief_dat, colors, focal, order_conds = order_conds, pop = pop),
    ncol = 1, rel_heights = c(1, legend_rel)
  )
}

# Two-wave figures keep the pilot-small height. A single wave is shorter,
# but the legend strip stays the same absolute height: scaling it with the
# panel drops the key onto the axis titles.
LEGEND_IN <- FIGURE_HEIGHT * LEGEND_REL / (1 + LEGEND_REL)

figure_layout <- function(dat) {
  if (length(unique(dat$timepoint)) > 1) {
    return(list(height = FIGURE_HEIGHT, legend_rel = LEGEND_REL))
  }
  y_scale <- y_scale_for(dat)
  panel_in <- (FIGURE_HEIGHT - LEGEND_IN) * (diff(y_scale$limits) / TWO_WAVE_Y_SPAN)
  # Two-line titles need a little more than the one-line meat/charity titles.
  panel_in <- panel_in + 0.28
  height <- panel_in + LEGEND_IN
  list(height = height, legend_rel = LEGEND_IN / panel_in)
}

# =============================================================================
# SINGLE-COLUMN STACK
# =============================================================================
# Political violence and deceiving others are one wave and one study, so the
# timepoint label is dropped and the condition names sit on the y-axis.
# Belief is above behaviour. There is no legend: the axis labels carry it.

build_column_panel <- function(dat, title, x_lab, type, colors, focal, pop = FALSE,
                               sig_label = NULL, show_y = TRUE, compact = FALSE,
                               y_expand = 0.4, panel_margin = NULL, y_lab = NULL) {
  build_mean_ci_panel(
    dat, title, x_lab, type, colors, focal,
    y_mode = "condition", show_y = show_y, pop = pop, sig_label = sig_label,
    compact = compact, y_expand = y_expand, panel_margin = panel_margin,
    y_lab = y_lab
  )
}

assemble_column <- function(belief_dat, behaviour_dat, belief_title, behaviour_title,
                            behaviour_x, behaviour_type, colors, focal, pop = FALSE,
                            sig_belief = NULL, sig_behaviour = NULL) {
  belief_panel <- build_column_panel(
    belief_dat, belief_title, "Belief change (post \u2212 pre)", "change",
    colors, focal,
    pop = pop, sig_label = sig_belief
  ) +
    labs(tag = "A") + tag_style
  behaviour_panel <- build_column_panel(
    behaviour_dat, behaviour_title, behaviour_x, behaviour_type,
    colors, focal,
    pop = pop, sig_label = sig_behaviour
  ) +
    labs(tag = "B") + tag_style
  plot_grid(
    belief_panel, behaviour_panel,
    ncol = 1, align = "v", axis = "lr"
  )
}

# Belief and behaviour on one row. Condition names are drawn once, on the
# belief panel, because the two panels share the same order.
assemble_column_row <- function(belief_dat, behaviour_dat, belief_title, behaviour_title,
                                behaviour_x, behaviour_type, colors, focal, pop = FALSE,
                                sig_belief = NULL, sig_behaviour = NULL,
                                y_expand = 0.4, panel_margin = NULL) {
  # Panel letters are drawn by plot_grid after the panels are aligned.
  # A ggplot tag sits under the top padding cowplot adds to the shorter
  # title, so the letter on the one-line behaviour title lands lower than
  # the letter on the three-line belief title.
  belief_panel <- build_column_panel(
    belief_dat, belief_title, "Belief change (post \u2212 pre)", "change",
    colors, focal,
    pop = pop, sig_label = sig_belief, show_y = TRUE, compact = TRUE,
    y_expand = y_expand, panel_margin = panel_margin, y_lab = "Condition"
  )
  behaviour_panel <- build_column_panel(
    behaviour_dat, behaviour_title, behaviour_x, behaviour_type,
    colors, focal,
    pop = pop, sig_label = sig_behaviour, show_y = FALSE, compact = TRUE,
    y_expand = y_expand, panel_margin = panel_margin
  )
  # Widths cover the fixed pieces (labels, titles, margins). The same amount
  # is then added for each panel, so the plotting regions match and the
  # condition names sit outside that shared width.
  fixed_in <- function(plot) {
    widths <- ggplotGrob(plot)$widths
    keep <- !grid::unitType(widths) %in% c("null", "npc")
    if (!any(keep)) {
      return(0)
    }
    sum(grid::convertWidth(widths[keep], "in", valueOnly = TRUE))
  }
  # The behaviour panel's fixed width is a little high once drawn, which
  # leaves its plotting region wider than the belief panel. The nudge
  # puts those two regions on the same width.
  f1 <- fixed_in(belief_panel)
  f2 <- max(fixed_in(behaviour_panel) - 0.055, 0)
  # The saved figure is one column wide. Split whatever is left after the
  # fixed pieces equally, so the two plotting regions come out the same width.
  panel_in <- max((FIGURE_WIDTH_COLUMN - f1 - f2) / 2, 0.4)
  plot_grid(
    belief_panel, behaviour_panel,
    nrow = 1, align = "h", axis = "tb",
    rel_widths = c(f1 + panel_in, f2 + panel_in),
    labels = c("A", "B"),
    label_size = 9, label_fontfamily = "Helvetica", label_fontface = "bold",
    label_x = 0.01, label_y = 1, hjust = 0, vjust = 1.25
  )
}

# =============================================================================
# OUTCOMES
# =============================================================================

s1_t2_belief <- s1_t2 |>
  label_study1() |>
  mutate(
    meat_wrong_change_t2 = meat_wrong_t2 - meat_wrong_pre,
    charity_wrong_change_t2 = charity_wrong_t2 - charity_wrong_pre
  )

bind_outcome <- function(waves, focal) {
  bind_rows(waves) |>
    place_points(focal)
}

wave <- function(dat, outcome, type, timepoint, study, focal) {
  summarise_wave(dat, outcome, type, timepoint, study, focal)
}

specs <- list(
  list(
    key = "meat",
    focal = "Meat",
    file = "fig-meat-pilot-small.png",
    column_file = "fig-meat-column.png",
    column_waves = TRUE,
    belief_title = "\"Eating meat is morally wrong.\"",
    behaviour_title = "Dietary Choice",
    behaviour_x = "% choosing plant-based burger",
    behaviour_type = "binary",
    belief = function(focal) {
      bind_outcome(list(
        wave(label_study2(s2_t1), "belief_meat_change", "change", "T1", "Study 2", focal),
        wave(label_study2(s2_t2), "belief_meat_change_t2", "change", "T2", "Study 2", focal),
        wave(label_study1(s1_t1), "meat_wrong_change", "change", "T1", "Study 1", focal),
        wave(s1_t2_belief, "meat_wrong_change_t2", "change", "T2", "Study 1", focal)
      ), focal)
    },
    behaviour = function(focal) {
      bind_outcome(list(
        wave(label_study2(s2_t1), "voucher_is_veg", "binary", "T1", "Study 2", focal),
        wave(label_study2(s2_t2), "voucher_is_veg_t2", "binary", "T2", "Study 2", focal),
        wave(label_study1(s1_t1), "voucher_is_veg", "binary", "T1", "Study 1", focal),
        wave(label_study1(s1_t2), "voucher_is_veg_t2", "binary", "T2", "Study 1", focal)
      ), focal)
    }
  ),
  list(
    key = "charity",
    focal = "Charity",
    file = "fig-charity-pilot-small.png",
    column_file = "fig-charity-column.png",
    column_waves = TRUE,
    belief_title = "\"Not giving to charity is morally wrong.\"",
    behaviour_title = "Donation",
    behaviour_x = "Donation to AMF (cents)",
    behaviour_type = "cents",
    belief = function(focal) {
      bind_outcome(list(
        wave(label_study2(s2_t1), "belief_charity_change", "change", "T1", "Study 2", focal),
        wave(label_study2(s2_t2), "belief_charity_change_t2", "change", "T2", "Study 2", focal),
        wave(label_study1(s1_t1), "charity_wrong_change", "change", "T1", "Study 1", focal),
        wave(s1_t2_belief, "charity_wrong_change_t2", "change", "T2", "Study 1", focal)
      ), focal)
    },
    behaviour = function(focal) {
      bind_outcome(list(
        wave(label_study2(s2_t1), "donation_cents", "cents", "T1", "Study 2", focal),
        wave(label_study2(s2_t2), "donation_cents_t2", "cents", "T2", "Study 2", focal),
        wave(label_study1(s1_t1), "charity_donation_cents", "cents", "T1", "Study 1", focal),
        wave(label_study1(s1_t2), "charity_donation_cents_t2", "cents", "T2", "Study 1", focal)
      ), focal)
    }
  ),
  list(
    key = "polviol",
    focal = "Political Violence",
    file = "fig-polviol-small.png",
    column_file = "fig-polviol-column.png",
    belief_title = "\"In the US today, using violence to advance\npolitical goals is morally wrong.\"",
    column_belief_title = "\"In the US today, using\nviolence to advance political\ngoals is morally wrong.\"",
    row_belief_title = "\"In the US today, using\nviolence to advance political\ngoals is morally wrong.\"",
    behaviour_title = "Hypothetical Political\nViolence Participation",
    behaviour_x = "Would join crowd (0\u2013100)",
    behaviour_type = "points",
    belief = function(focal) {
      bind_outcome(list(
        wave(label_study2(s2_t1), "belief_polviol_change", "change", "T1", "Study 2", focal)
      ), focal)
    },
    behaviour = function(focal) {
      bind_outcome(list(
        wave(label_study2(s2_t1), "pv_participation", "points", "T1", "Study 2", focal)
      ), focal)
    }
  ),
  list(
    key = "lying",
    focal = "Lying",
    file = "fig-lying-small.png",
    column_file = "fig-lying-column.png",
    belief_title = "\"Deceiving others for your own\nfinancial gain is morally wrong.\"",
    column_belief_title = "\"Deceiving others for your\nown financial gain is\nmorally wrong.\"",
    row_belief_title = "\"Deceiving others for your\nown financial gain is\nmorally wrong.\"",
    behaviour_title = "Coin Flip Report",
    behaviour_x = "% falsely reporting heads",
    behaviour_type = "binary",
    belief = function(focal) {
      bind_outcome(list(
        wave(label_study2(s2_t1), "belief_lying_change", "change", "T1", "Study 2", focal)
      ), focal)
    },
    behaviour = function(focal) {
      bind_outcome(list(
        wave(label_study2(s2_t1), "coin_heads", "binary", "T1", "Study 2", focal)
      ), focal)
    }
  )
)

# =============================================================================
# FIXED COLORS, EXAGGERATED FOCAL ARM
# =============================================================================
# Two orders. "adj" puts the focal condition immediately above control.
# "order" uses the study-design order on every figure. Colors never move.

# Pop figures are 15% shorter than the first versions of this layout.
POP_HEIGHT_SCALE <- 0.85

pop_orders <- list(
  adj = function(focal) c("Control", focal, setdiff(design_order, c("Control", focal))),
  order = function(focal) design_order
)

pop_wide_layout <- function(dat) {
  lay <- figure_layout(dat)
  if (length(unique(dat$timepoint)) > 1) {
    return(list(height = 3.05, legend_rel = 0.20))
  }
  legend_in <- 0.42
  height <- lay$height + 0.18
  list(height = height, legend_rel = legend_in / (height - legend_in))
}

stem_of <- function(filename) sub("\\.png$", "", filename)

# Study 2 focal-vs-control labels for the adj figures. Same model as the
# main analyses: outcome ~ condition * centered pretreatment belief + order.
sig_plan <- list(
  meat = list(
    treat = "meat",
    belief = list(
      T1 = list(s2_t1, "belief_meat_change", "belief_meat_pre"),
      T2 = list(s2_t2, "belief_meat_change_t2", "belief_meat_pre")
    ),
    behaviour = list(
      T1 = list(s2_t1, "voucher_is_veg", "belief_meat_pre"),
      T2 = list(s2_t2, "voucher_is_veg_t2", "belief_meat_pre")
    )
  ),
  charity = list(
    treat = "charity",
    belief = list(
      T1 = list(s2_t1, "belief_charity_change", "belief_charity_pre"),
      T2 = list(s2_t2, "belief_charity_change_t2", "belief_charity_pre")
    ),
    behaviour = list(
      T1 = list(s2_t1, "donation_cents", "belief_charity_pre"),
      T2 = list(s2_t2, "donation_cents_t2", "belief_charity_pre")
    )
  ),
  polviol = list(
    treat = "polviol",
    belief = list(
      T1 = list(s2_t1, "belief_polviol_change", "belief_polviol_pre")
    ),
    behaviour = list(
      T1 = list(s2_t1, "pv_participation", "belief_polviol_pre")
    )
  ),
  lying = list(
    treat = "lying",
    belief = list(
      T1 = list(s2_t1, "belief_lying_change", "belief_lying_pre")
    ),
    behaviour = list(
      T1 = list(s2_t1, "coin_heads", "belief_lying_pre")
    )
  )
)

sig_labels_for <- function(key) {
  plan <- sig_plan[[key]]
  one <- function(waves) {
    labs <- vapply(names(waves), function(tp) {
      spec <- waves[[tp]]
      p <- contrast_p(spec[[1]], spec[[2]], spec[[3]], plan$treat)
      lab <- fmt_bracket_sig(p)
      cat(sprintf("  %s %s vs control: p = %.4g  %s\n", key, tp, p, lab))
      lab
    }, character(1))
    names(labs) <- names(waves)
    labs
  }
  cat(sprintf("%s, belief\n", key))
  belief <- one(plan$belief)
  cat(sprintf("%s, behaviour\n", key))
  behaviour <- one(plan$behaviour)
  list(belief = belief, behaviour = behaviour)
}

pop_sigs <- lapply(names(sig_plan), sig_labels_for)
names(pop_sigs) <- names(sig_plan)

# Uses paper_pdf_device() from helpers.R; trim_bottom_in is local (legend strip).
save_paper_fig <- function(file, plot, width, height, trim_bottom_in = 0) {
  path <- file.path(output_dir, file)
  ggsave(
    path,
    plot,
    width = width, height = height,
    dpi = FIGURE_DPI, bg = "white", units = "in",
    device = paper_pdf_device()
  )
  if (trim_bottom_in > 0) trim_pdf_bottom(path, trim_bottom_in)
  cat(sprintf(
    "Saved: output/figures/%s (%.2f x %.2f in)\n",
    file, width, height - trim_bottom_in
  ))
}

# The legend strip is taller than the key, and that extra space is split
# above and below it. Raise the page bottom to drop the empty band under
# the key. The plot and the legend stay where they are.
trim_pdf_bottom <- function(path, inches) {
  py <- tempfile(fileext = ".py")
  writeLines(c(
    "import sys",
    "from pypdf import PdfReader, PdfWriter",
    "path, pts = sys.argv[1], float(sys.argv[2])",
    "reader = PdfReader(path)",
    "writer = PdfWriter()",
    "page = reader.pages[0]",
    "box = page.mediabox",
    "left, bottom = float(box.left), float(box.bottom) + pts",
    "top, right = float(box.top), float(box.right)",
    "page.mediabox.lower_left = (left, bottom)",
    "page.mediabox.upper_right = (right, top)",
    "page.cropbox.lower_left = (left, bottom)",
    "page.cropbox.upper_right = (right, top)",
    "writer.add_page(page)",
    "with open(path, 'wb') as out:",
    "    writer.write(out)"
  ), py)
  # Prefer python3 (macOS/Linux); fall back to python (common on Windows).
  py_bin <- if (nzchar(Sys.which("python3"))) {
    "python3"
  } else if (nzchar(Sys.which("python"))) {
    "python"
  } else {
    NA_character_
  }
  status <- if (is.na(py_bin)) {
    1L
  } else {
    system2(py_bin, c(py, path, inches * 72))
  }
  if (!identical(status, 0L)) {
    warning(
      "Could not trim ", path,
      " (needs Python with pypdf). Leaving the untrimmed PDF.",
      call. = FALSE
    )
  }
}

# Paper figures only (exploratory variants live under extra_analyses/ if present).
if (!isTRUE(getOption("amp.figures.variants_only"))) {
  spec_for <- function(key) {
    specs[[which(vapply(specs, `[[`, character(1), "key") == key)]]
  }

  paper_keys <- getOption("amp.paper.keys", c("meat", "charity", "lying", "polviol"))

  for (key in intersect(c("meat", "charity"), paper_keys)) {
    spec <- spec_for(key)
    cond_order <- pop_orders$adj(spec$focal)
    belief_pop <- place_points(spec$belief(spec$focal), spec$focal, cond_order)
    behaviour_pop <- place_points(spec$behaviour(spec$focal), spec$focal, cond_order)
    wide <- pop_wide_layout(belief_pop)
    fig <- assemble_fig(
      belief_pop, behaviour_pop,
      spec$belief_title, spec$behaviour_title,
      spec$behaviour_x, spec$behaviour_type,
      design_colors, spec$focal,
      legend_rel = wide$legend_rel, pop = TRUE, order_conds = cond_order,
      sig_belief = pop_sigs[[key]]$belief,
      sig_behaviour = pop_sigs[[key]]$behaviour
    )
    height <- wide$height * POP_HEIGHT_SCALE
    save_paper_fig(
      sprintf("fig-%s.pdf", key), fig, FIGURE_WIDTH, height,
      trim_bottom_in = 0.13
    )
  }

  for (key in intersect(c("lying", "polviol"), paper_keys)) {
    spec <- spec_for(key)
    cond_order <- pop_orders$adj(spec$focal)
    belief_pop <- place_points(spec$belief(spec$focal), spec$focal, cond_order)
    behaviour_pop <- place_points(spec$behaviour(spec$focal), spec$focal, cond_order)
    fig <- assemble_column_row(
      belief_pop, behaviour_pop,
      spec$row_belief_title, spec$behaviour_title,
      spec$behaviour_x, spec$behaviour_type,
      design_colors, spec$focal,
      pop = TRUE,
      sig_belief = unname(pop_sigs[[key]]$belief[["T1"]]),
      sig_behaviour = unname(pop_sigs[[key]]$behaviour[["T1"]])
    )
    save_paper_fig(
      sprintf("fig-%s.pdf", key), fig, FIGURE_WIDTH_COLUMN, 2.15
    )
  }
}
