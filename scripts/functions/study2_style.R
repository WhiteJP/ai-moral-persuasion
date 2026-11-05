# =============================================================================
# Shared Study 2 display labels and figure defaults
# =============================================================================
# One place for condition names/colors so console tables, TeX, and figures
# stay consistent. Canonical short label for the lying arm is "Lying"
# (matches main-text figures and SI tables).

CONDITION_KEYS <- c("control", "meat", "charity", "lying", "polviol")

condition_labels <- c(
  control = "Control",
  meat    = "Meat",
  charity = "Charity",
  lying   = "Lying",
  polviol = "Political Violence"
)

# Subset used by follow-up analyses (control / meat / charity only).
condition_labels_fu <- condition_labels[c("control", "meat", "charity")]

colors_condition <- c(
  "Control"            = "#9B9B9B",
  "Meat"               = "#E07A5F",
  "Charity"            = "#3D405B",
  "Lying"              = "#81B29A",
  "Political Violence" = "#F2CC8F"
)

colors_condition_fu <- colors_condition[c("Control", "Meat", "Charity")]

# Corrective / antisocial-arm figures (two-arm palette).
colors_arm_antisocial <- c(
  "Lying"              = "#E69F00",
  "Political Violence" = "#CC79A7"
)

FIGURE_DPI <- 300L
# PNAS double-column width (~17.8–18 cm).
FIGURE_WIDTH_DOUBLE <- 7.08
# PNAS single-column width (~8.7 cm).
FIGURE_WIDTH_SINGLE <- 3.43

theme_study2_fig <- function(base_size = 9, base_family = "Helvetica") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.line          = ggplot2::element_line(color = "grey30", linewidth = 0.4),
      axis.ticks         = ggplot2::element_line(color = "grey30", linewidth = 0.3),
      axis.text          = ggplot2::element_text(color = "grey20"),
      axis.title         = ggplot2::element_text(color = "grey10", face = "bold", size = 8),
      plot.title         = ggplot2::element_text(
        face = "bold", size = 8, hjust = 0.5,
        margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle      = ggplot2::element_text(color = "grey40", size = 7),
      legend.position    = "bottom",
      legend.title       = ggplot2::element_text(face = "bold", size = 8),
      strip.text         = ggplot2::element_text(face = "bold", size = 8),
      plot.margin        = ggplot2::margin(3, 5, 3, 3)
    )
}
