# =============================================================================
# 05 · Combined Figure (Figure 1)
# =============================================================================
# Row 1: Main effects (A: belief change, B: donation, C: voucher)
# Row 2: Participant reasoning categories (D: meat, E: charity)
# Row 3: AI persuasion strategy distribution (F)
#
# Reads NLP classification CSVs directly (no RDS intermediaries) and computes
# consensus columns where needed.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(patchwork)
library(ggtext)
library(ggdist)
library(cowplot)
library(here)

source(here("scripts", "functions", "helpers.R"))

# =============================================================================
# DATA LOADING
# =============================================================================

d1 <- readRDS(here("data", "d1.rds"))

# --- NLP: Meat reasoning classifications ------------------------------------

df_4ns <- read_csv(here("data", "classifications_4ns.csv"), show_col_types = FALSE) |>
  dplyr::select(id, natural_consensus, normal_consensus, necessary_consensus, nice_consensus) |>
  rename(
    PROLIFIC_PID                = id,
    meat_4n_natural_consensus   = natural_consensus,
    meat_4n_normal_consensus    = normal_consensus,
    meat_4n_necessary_consensus = necessary_consensus,
    meat_4n_nice_consensus      = nice_consensus
  )

df_other <- read_csv(here("data", "classifications_meat_other.csv"), show_col_types = FALSE) |>
  dplyr::select(id, religious_consensus, amoral_consensus, conditional_consensus) |>
  rename(PROLIFIC_PID = id)

d_meat <- d1 |>
  filter(condition == "Meat") |>
  left_join(df_4ns, by = "PROLIFIC_PID") |>
  left_join(df_other, by = "PROLIFIC_PID")

d_meat_low <- d_meat |> filter(meat_wrong_pre <= 50)

# --- NLP: Charity reasoning classifications ---------------------------------

df_charity_reason <- read_csv(here("data", "classifications_charity_reasoning.csv"),
                              show_col_types = FALSE) |>
  dplyr::select(id,
         financial_constraints_consensus, supererogatory_consensus,
         skepticism_consensus, relativism_consensus,
         property_rights_consensus, systemic_consensus,
         limited_scope_consensus) |>
  rename_with(~ paste0("charity_reason_", .), -id) |>
  rename(PROLIFIC_PID = id)

d_charity <- d1 |>
  filter(condition == "Charity") |>
  left_join(df_charity_reason, by = "PROLIFIC_PID")

d_charity_low <- d_charity |> filter(charity_wrong_pre <= 50)

# --- NLP: Persuasion strategy classifications -------------------------------

d_strat <- read_csv(here("data", "classifications_persuasion_strategies.csv"),
                    show_col_types = FALSE)

# =============================================================================
# SHARED STYLE
# =============================================================================

output_dir <- here("output", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

FIGURE_WIDTH  <- 7.08
FIGURE_HEIGHT <- 7.5
FIGURE_DPI    <- 300

colors_condition <- c(
  "Meat Conversation"    = "#E07A5F",
  "Charity Conversation" = "#3D405B"
)

theme_fig <- theme_minimal(base_size = 9, base_family = "Helvetica") +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    axis.line          = element_line(color = "grey30", linewidth = 0.4),
    axis.ticks         = element_line(color = "grey30", linewidth = 0.3),
    axis.text          = element_text(color = "grey20"),
    axis.title         = element_text(color = "grey10", face = "bold", size = 8),
    plot.title         = element_text(face = "bold", size = 8, hjust = 0.5,
                                     margin = margin(b = 2)),
    plot.subtitle      = element_text(color = "grey40", size = 7),
    legend.position    = "none",
    legend.title       = element_text(face = "bold", size = 8),
    strip.text         = element_text(face = "bold"),
    plot.margin        = margin(3, 5, 3, 3)
  )

# =============================================================================
# PANEL A: Belief Change
# =============================================================================

belief_data <- d1 |>
  mutate(condition = ifelse(condition == "Meat",
                            "Meat Conversation", "Charity Conversation")) |>
  dplyr::select(condition, meat_wrong_change, charity_wrong_change) |>
  pivot_longer(c(meat_wrong_change, charity_wrong_change),
               names_to = "belief_type", values_to = "change") |>
  mutate(
    belief_type = ifelse(
      belief_type == "meat_wrong_change",
      "Eating Meat\nis Morally\nWrong",
      "Not Giving to\nCharity is\nMorally Wrong"
    ),
    belief_type = factor(belief_type, levels = c(
      "Eating Meat\nis Morally\nWrong",
      "Not Giving to\nCharity is\nMorally Wrong"
    ))
  )

belief_summary <- belief_data |>
  group_by(condition, belief_type) |>
  summarise(
    mean     = mean(change, na.rm = TRUE),
    se       = sd(change, na.rm = TRUE) / sqrt(n()),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se,
    .groups  = "drop"
  )

max_y_meat <- max(belief_summary$ci_upper[
  belief_summary$belief_type == "Eating Meat\nis Morally\nWrong"])
max_y_charity <- max(belief_summary$ci_upper[
  belief_summary$belief_type == "Not Giving to\nCharity is\nMorally Wrong"])

panel_a <- ggplot(belief_summary, aes(x = belief_type, y = mean, color = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.5),
                width = 0.12, linewidth = 0.5) +
  annotate("segment", x = 0.875, xend = 0.875,
           y = max_y_meat + 3, yend = max_y_meat + 4,
           color = "black", linewidth = 0.3) +
  annotate("segment", x = 0.875, xend = 1.125,
           y = max_y_meat + 4, yend = max_y_meat + 4,
           color = "black", linewidth = 0.3) +
  annotate("segment", x = 1.125, xend = 1.125,
           y = max_y_meat + 3, yend = max_y_meat + 4,
           color = "black", linewidth = 0.3) +
  annotate("text", x = 1, y = max_y_meat + 4.8,
           label = "***", size = 3, color = "black") +
  annotate("segment", x = 1.875, xend = 1.875,
           y = max_y_charity + 3, yend = max_y_charity + 4,
           color = "black", linewidth = 0.3) +
  annotate("segment", x = 1.875, xend = 2.125,
           y = max_y_charity + 4, yend = max_y_charity + 4,
           color = "black", linewidth = 0.3) +
  annotate("segment", x = 2.125, xend = 2.125,
           y = max_y_charity + 3, yend = max_y_charity + 4,
           color = "black", linewidth = 0.3) +
  annotate("text", x = 2, y = max_y_charity + 4.8,
           label = "***", size = 3, color = "black") +
  scale_color_manual(values = colors_condition, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.15))) +
  labs(x = NULL, y = "Belief Change\n(0\u2013100 scale)",
       title = "Moral Belief Change") +
  theme_fig +
  theme(axis.text.x = element_text(size = 7.5, lineheight = 0.9))

# =============================================================================
# PANEL B: Donation to AMF
# =============================================================================

donation_summary <- d1 |>
  mutate(
    condition = ifelse(condition == "Meat",
                       "Meat Conversation", "Charity Conversation"),
    condition_short = ifelse(condition == "Meat Conversation",
                             "Meat\nconv.", "Charity\nconv."),
    condition_short = factor(condition_short,
                             levels = c("Meat\nconv.", "Charity\nconv."))
  ) |>
  group_by(condition, condition_short) |>
  summarise(
    mean     = mean(charity_donation_cents, na.rm = TRUE),
    se       = sd(charity_donation_cents, na.rm = TRUE) / sqrt(n()),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se,
    .groups  = "drop"
  )

max_x_donation <- max(donation_summary$ci_upper)

panel_b <- ggplot(donation_summary,
                  aes(x = mean, y = condition_short, color = condition)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 height = 0.12, linewidth = 0.5) +
  annotate("segment",
           x = max_x_donation + 0.8, xend = max_x_donation + 1.5,
           y = 1, yend = 1, color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_donation + 1.5, xend = max_x_donation + 1.5,
           y = 1, yend = 2, color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_donation + 0.8, xend = max_x_donation + 1.5,
           y = 2, yend = 2, color = "black", linewidth = 0.3) +
  annotate("text",
           x = max_x_donation + 2.5, y = 1.5,
           label = "*", size = 3, color = "black", angle = 90) +
  scale_color_manual(values = colors_condition, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)),
                     limits = c(0, NA),
                     labels = function(x) paste0(x, "\u00A2")) +
  coord_cartesian(clip = "off") +
  labs(x = "Donation to AMF (cents)", y = NULL,
       title = "Charitable Behavior") +
  theme_fig +
  theme(axis.text.y = element_text(size = 8.5))

# =============================================================================
# PANEL C: Voucher Choice
# =============================================================================

voucher_summary <- d1 |>
  mutate(
    condition = ifelse(condition == "Meat",
                       "Meat Conversation", "Charity Conversation"),
    condition_short = ifelse(condition == "Meat Conversation",
                             "Meat\nconv.", "Charity\nconv."),
    condition_short = factor(condition_short,
                             levels = c("Meat\nconv.", "Charity\nconv."))
  ) |>
  group_by(condition, condition_short) |>
  summarise(
    n        = n(),
    n_veg    = sum(voucher_is_veg, na.rm = TRUE),
    prop     = mean(voucher_is_veg, na.rm = TRUE) * 100,
    se       = sqrt(prop * (100 - prop) / n),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se,
    .groups  = "drop"
  )

max_x_voucher <- max(voucher_summary$ci_upper)

panel_c <- ggplot(voucher_summary,
                  aes(x = prop, y = condition_short, fill = condition)) +
  geom_col(width = 0.5, alpha = 0.9) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 height = 0.12, linewidth = 0.5, color = "grey30") +
  annotate("segment",
           x = max_x_voucher + 4, xend = max_x_voucher + 7,
           y = 1, yend = 1, color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_voucher + 7, xend = max_x_voucher + 7,
           y = 1, yend = 2, color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_voucher + 4, xend = max_x_voucher + 7,
           y = 2, yend = 2, color = "black", linewidth = 0.3) +
  annotate("text",
           x = max_x_voucher + 11, y = 1.5,
           label = "***", size = 3, color = "black", angle = 90) +
  scale_fill_manual(values = colors_condition, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3)),
                     limits = c(0, NA),
                     labels = function(x) paste0(x, "%")) +
  coord_cartesian(clip = "off") +
  labs(x = "% Choosing Veg Burger", y = NULL,
       title = "Dietary Behaviour") +
  theme_fig +
  theme(axis.text.y = element_text(size = 8.5))

# =============================================================================
# PANEL D: Meat Pro-Meat Reasoning (skeptics only, pre <= 50)
# =============================================================================

MEAT_PRO <- list(
  "meat_4n_natural_consensus"   = c("Natural",     "Evolved to eat meat"),
  "meat_4n_necessary_consensus" = c("Necessary",   "Required for nutrition"),
  "amoral_consensus"            = c("Amoral",       "Not a moral issue"),
  "conditional_consensus"       = c("Conditional",  "OK if ethically sourced"),
  "meat_4n_normal_consensus"    = c("Normal",       "Socially acceptable"),
  "meat_4n_nice_consensus"      = c("Nice",         "Tastes good"),
  "religious_consensus"         = c("Religious",    "God permits it")
)

make_reasoning_panel <- function(data, categories, panel_title, subtitle) {
  counts <- purrr::map_dfr(names(categories), function(var) {
    info  <- categories[[var]]
    label <- info[1]
    desc  <- info[2]
    count <- if (var %in% names(data)) sum(data[[var]] == TRUE, na.rm = TRUE) else 0
    pct   <- round(100 * count / nrow(data), 1)
    html_label <- paste0(
      "<b>", label, "</b><br>",
      "<span style='font-size:8pt;color:grey50;'>", desc, "</span>"
    )
    tibble(
      FullLabel = html_label,
      Count     = count,
      Pct       = pct,
      Label     = paste0(pct, "%")
    )
  }) |>
    arrange(desc(Count)) |>
    mutate(FullLabel = factor(FullLabel, levels = rev(FullLabel)))

  ggplot(counts, aes(x = Pct, y = FullLabel)) +
    geom_col(fill = "skyblue", color = "black", width = 1) +
    geom_text(aes(label = Label), hjust = -0.15, size = 2.3, fontface = "bold") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25)), limits = c(0, NA)) +
    labs(title = panel_title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_bw(base_size = 9) +
    theme(
      plot.title         = element_text(face = "bold", size = 8, hjust = 0.4,
                                        margin = margin(b = 1)),
      plot.subtitle      = element_text(color = "grey30", size = 7.5,
                                        hjust = 0.4, margin = margin(b = 3)),
      panel.grid         = element_blank(),
      panel.border       = element_blank(),
      axis.line.y        = element_blank(),
      axis.line.x        = element_blank(),
      axis.ticks.y       = element_blank(),
      axis.ticks.x       = element_blank(),
      axis.text.y        = element_markdown(size = 7, lineheight = 1.1),
      axis.text.x        = element_blank(),
      plot.margin        = margin(3, 8, 3, 3)
    )
}

panel_d <- make_reasoning_panel(
  d_meat_low, MEAT_PRO,
  "Participant Reasons for Meat-eating",
  paste0("Pre score \u2264 50, N = ", nrow(d_meat_low))
) +
  theme(plot.title.position = "plot")

# =============================================================================
# PANEL E: Charity Anti-Obligation Reasoning (skeptics only, pre <= 50)
# =============================================================================

CHARITY_ANTI <- list(
  "charity_reason_financial_constraints_consensus" = c("Financial",      "Can't afford to give"),
  "charity_reason_supererogatory_consensus"        = c("Supererogatory", "Nice but not required"),
  "charity_reason_skepticism_consensus"            = c("Skepticism",     "Charity doesn't work"),
  "charity_reason_relativism_consensus"            = c("Relativism",     "No objective duty"),
  "charity_reason_property_rights_consensus"       = c("Property Rights","It's my money"),
  "charity_reason_systemic_consensus"              = c("Systemic",       "Need structural change"),
  "charity_reason_limited_scope_consensus"         = c("Limited Scope",  "Obligations are local")
)

panel_e <- make_reasoning_panel(
  d_charity_low, CHARITY_ANTI,
  "Participant Reasons Against Charitable Obligation",
  paste0("Pre score \u2264 50, N = ", nrow(d_charity_low))
) +
  theme(plot.title.position = "plot",
        plot.subtitle = element_text(color = "grey30", size = 7.5,
                                     margin = margin(b = 3)))

# =============================================================================
# PANEL F: AI Persuasion Strategy Distribution
# =============================================================================

strategy_labels <- c(
  evidence_facts      = "Evidence & Facts",
  logical_consistency = "Logical Consistency",
  moral_principles    = "Moral Principles",
  emotional_appeal    = "Emotional Appeal",
  analogies_reframing = "Analogies & Reframing"
)

strategy_descriptions <- c(
  evidence_facts      = "Cites data, statistics, or factual claims",
  logical_consistency = "Demands consistency across analogous scenarios",
  moral_principles    = "Invokes explicit moral duties or obligations",
  emotional_appeal    = "Appeals to empathy, compassion, or emotion",
  analogies_reframing = "Uses analogies or reframes the issue"
)

strategies   <- names(strategy_labels)
models       <- c("claude", "gpt", "deepseek")
rating_to_num <- c(none = 0, low = 1, moderate = 2, high = 3)

for (s in strategies) {
  model_cols <- paste0(s, "_", models)
  d_strat[[paste0(s, "_mean")]] <- d_strat |>
    dplyr::select(all_of(model_cols)) |>
    mutate(across(everything(), ~ rating_to_num[.])) |>
    rowMeans(na.rm = TRUE)
}

strat_long <- d_strat |>
  pivot_longer(cols = all_of(paste0(strategies, "_mean")),
               names_to = "strategy", values_to = "rating") |>
  mutate(strategy = str_remove(strategy, "_mean")) |>
  filter(!is.na(rating)) |>
  dplyr::select(id, condition, strategy, rating)

strategy_order <- strat_long |>
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

strat_long <- strat_long |>
  mutate(
    strategy  = factor(strategy, levels = strategy_order),
    condition = ifelse(condition == "Charity",
                       "Charity Conversation", "Meat Conversation")
  )

panel_f <- strat_long |>
  ggplot(aes(x = rating, y = strategy, fill = condition)) +
  stat_slab(
    density = "histogram",
    breaks = seq(-1/6, 3 + 1/6, by = 1/3),
    height = 0.7,
    justification = 0.5,
    position = position_dodge(width = 0.7),
    alpha = 0.7,
    normalize = "panels"
  ) +
  stat_pointinterval(
    aes(colour = condition),
    point_interval = "mean_qi",
    .width = 0.5,
    position = position_dodge(width = 0.7),
    interval_color = NA,
    point_size = 1.8,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = colors_condition, guide = "none") +
  scale_colour_manual(values = colors_condition, guide = "none") +
  scale_y_discrete(labels = strat_label_map) +
  scale_x_continuous(
    breaks = 0:3,
    labels = c("Not\nused", "Limited\nuse", "Used with\nemphasis", "Used\nextensively")
  ) +
  labs(x = "Strategy Rating", y = NULL,
       title = "AI Persuasive Strategy Use") +
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

# =============================================================================
# ASSEMBLE COMBINED FIGURE
# =============================================================================

tag_style <- theme(plot.tag = element_text(face = "bold", size = 10))

panel_a_tagged <- panel_a + labs(tag = "A") + tag_style
panel_b_tagged <- panel_b + labs(tag = "B") + tag_style
panel_c_tagged <- panel_c + labs(tag = "C") + tag_style

row1 <- plot_grid(panel_a_tagged, panel_b_tagged, panel_c_tagged,
                  nrow = 1, rel_widths = c(1, 1, 1), align = "none")

panel_d_tagged <- panel_d + labs(tag = "D") + tag_style
panel_e_tagged <- panel_e + labs(tag = "E") + tag_style

row2 <- plot_grid(panel_d_tagged, panel_e_tagged,
                  nrow = 1, rel_widths = c(1, 1), align = "none")

panel_f_tagged <- panel_f + labs(tag = "F") + tag_style

legend_plot <- get_legend(
  panel_a +
    guides(color = guide_legend(direction = "vertical")) +
    theme(legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.4, "cm"))
)

row3 <- plot_grid(panel_f_tagged, legend_plot,
                  nrow = 1, rel_widths = c(3, 1), align = "none")

combined <- plot_grid(row1, row2, row3,
                      ncol = 1, rel_heights = c(1, 1, 1))

save_plot(file.path(output_dir, "fig-combined.png"),
         combined, base_width = FIGURE_WIDTH, base_height = FIGURE_HEIGHT,
         dpi = FIGURE_DPI, bg = "white")

cat("Saved: output/figures/fig-combined.png\n")
