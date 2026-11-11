# =============================================================================
# 05 · Figures
# =============================================================================
# Builds four publication figures from cleaned survey data and NLP codings:
# belief change (T1 vs T2), behavioral outcomes (donation + voucher by time),
# participant reasoning themes (meat vs charity skeptics), and AI strategy use.
# Exports to output/study_1/figures/: fig-belief-change.png, fig-behavior.png,
# fig-participant-reasoning.png, fig-ai-strategies.png, plus a PDF of each.
#

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggtext)
library(ggdist)
library(cowplot)
library(here)

source(here("scripts", "functions", "helpers.R"))

# =============================================================================
# DATA LOADING
# =============================================================================

d1 <- readRDS(here("data", "study_1", "d1.rds"))
d  <- readRDS(here("data", "study_1", "d.rds"))   # initial + 1-month follow-up

# --- NLP: Meat reasoning classifications ------------------------------------

df_4ns <- read_csv(here("data", "study_1", "classifications_4ns.csv"), show_col_types = FALSE) |>
  dplyr::select(id, natural_consensus, normal_consensus, necessary_consensus, nice_consensus) |>
  rename(
    participant_id                = id,
    meat_4n_natural_consensus   = natural_consensus,
    meat_4n_normal_consensus    = normal_consensus,
    meat_4n_necessary_consensus = necessary_consensus,
    meat_4n_nice_consensus      = nice_consensus
  )

df_other <- read_csv(here("data", "study_1", "classifications_meat_other.csv"), show_col_types = FALSE) |>
  dplyr::select(id, religious_consensus, amoral_consensus, conditional_consensus) |>
  rename(participant_id = id)

d_meat <- d1 |>
  filter(condition == "Meat") |>
  left_join(df_4ns, by = "participant_id") |>
  left_join(df_other, by = "participant_id")

d_meat_low <- d_meat |> filter(meat_wrong_pre <= 50)

# --- NLP: Charity reasoning classifications ---------------------------------

df_charity_reason <- read_csv(here("data", "study_1", "classifications_charity_reasoning.csv"),
                              show_col_types = FALSE) |>
  dplyr::select(id,
         financial_constraints_consensus, supererogatory_consensus,
         skepticism_consensus, relativism_consensus,
         property_rights_consensus, systemic_consensus,
         limited_scope_consensus) |>
  rename_with(~ paste0("charity_reason_", .), -id) |>
  rename(participant_id = id)

d_charity <- d1 |>
  filter(condition == "Charity") |>
  left_join(df_charity_reason, by = "participant_id")

d_charity_low <- d_charity |> filter(charity_wrong_pre <= 50)

# --- NLP: Persuasion strategy classifications -------------------------------

d_strat <- read_csv(here("data", "study_1", "classifications_persuasion_strategies.csv"),
                    show_col_types = FALSE)

# =============================================================================
# SHARED STYLE
# =============================================================================

output_dir <- here("output", "study_1", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
use_quartz_text_metrics()

FIGURE_WIDTH  <- 7.08
FIGURE_HEIGHT <- 7.5
FIGURE_DPI    <- 300
# Width for the two-column behavior and belief PNGs (slightly narrower than full page)
FIGURE_WIDTH_BEHAVIOR_BELIEF <- 6

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
  
# T1 / T2 wording reused wherever a plot contrasts immediate post vs 1-month follow-up
TP_T1 <- "Immediately\npost-treatment"
TP_T2 <- "1-month\nfollow-up"
timepoint_levels <- c(TP_T2, TP_T1) # factor order → T2 bottom, T1 top on horizontal-dot plots

# =============================================================================
# Meat skeptics — reasons offered for meat-eating (pre-treatment moral score ≤ 50)
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
# Charity skeptics — reasons against a duty to donate (pre score ≤ 50)
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
# AI persuasion strategies — rating distributions by experimental arm
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
# Subplot labels (A, B, …) for cowplot composites
# =============================================================================

tag_style <- paper_tag_theme()

# =============================================================================
# Behavior: charitable donation + veg-voucher choice, both at T1 and T2
# =============================================================================
# Horizontal layout: mean donation (cents) or mean voucher choice (% veg) on x-axis;
# two rows for timepoints; Meat vs Charity arm dodged within each row. Both panels use
# point estimates ± 95% CI. Bracket annotations mark treatment-vs-control contrasts from
# the preregistered main models
# (scripts/study_1/04_main_analyses.R; values mirrored in annotations below).

# Timepoint rows: factor order puts T1 at y = 2 (top) and T2 at y = 1 (bottom);
# bracket coordinates follow those row indices.

# --- Donation across timepoints ---------------------------------------------

donation_t1 <- d1 |>
  mutate(condition = ifelse(condition == "Meat",
                            "Meat Conversation", "Charity Conversation"),
         timepoint = TP_T1) |>
  dplyr::select(condition, timepoint, value = charity_donation_cents)

donation_t2 <- d |>
  mutate(condition = ifelse(condition == "Meat",
                            "Meat Conversation", "Charity Conversation"),
         timepoint = TP_T2) |>
  dplyr::select(condition, timepoint, value = charity_donation_cents_t2)

n_donation_t1 <- sum(!is.na(donation_t1$value))
n_donation_t2 <- sum(!is.na(donation_t2$value))

donation_t12_summary <- bind_rows(donation_t1, donation_t2) |>
  mutate(timepoint = factor(timepoint, levels = timepoint_levels)) |>
  group_by(condition, timepoint) |>
  summarise(
    mean     = mean(value, na.rm = TRUE),
    se       = sd(value, na.rm = TRUE) / sqrt(n()),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se,
    .groups  = "drop"
  )

max_x_donation_t12 <- max(donation_t12_summary$ci_upper)

behavior_dodge <- 0.5
behavior_offset <- behavior_dodge / 4

y_t1 <- 2
y_t2 <- 1

donation_y_labels <- setNames(
  c(paste0(TP_T2, "\n(N = ", n_donation_t2, ")"),
    paste0(TP_T1, "\n(N = ", n_donation_t1, ")")),
  c(TP_T2, TP_T1)
)

panel_donation_t12 <- ggplot(donation_t12_summary,
                             aes(x = mean, y = timepoint, colour = condition)) +
  geom_point(size = 2.5,
             position = position_dodge(width = behavior_dodge)) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 position = position_dodge(width = behavior_dodge),
                 height = 0.16, linewidth = 0.5) +
  annotate("segment",
           x = max_x_donation_t12 + 0.8, xend = max_x_donation_t12 + 1.5,
           y = y_t1 - behavior_offset, yend = y_t1 - behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_donation_t12 + 1.5, xend = max_x_donation_t12 + 1.5,
           y = y_t1 - behavior_offset, yend = y_t1 + behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_donation_t12 + 0.8, xend = max_x_donation_t12 + 1.5,
           y = y_t1 + behavior_offset, yend = y_t1 + behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("text",
           x = max_x_donation_t12 + 2.5, y = y_t1,
           label = "*", size = 3, color = "black", angle = 90) +
  annotate("segment",
           x = max_x_donation_t12 + 0.8, xend = max_x_donation_t12 + 1.5,
           y = y_t2 - behavior_offset, yend = y_t2 - behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_donation_t12 + 1.5, xend = max_x_donation_t12 + 1.5,
           y = y_t2 - behavior_offset, yend = y_t2 + behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_donation_t12 + 0.8, xend = max_x_donation_t12 + 1.5,
           y = y_t2 + behavior_offset, yend = y_t2 + behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("text",
           x = max_x_donation_t12 + 2.7, y = y_t2,
           label = "n.s.", size = 2.4, color = "black", angle = -90) +
  scale_colour_manual(values = colors_condition, name = NULL) +
  scale_y_discrete(labels = donation_y_labels) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)),
                     limits = c(0, NA),
                     labels = function(x) paste0(x, "\u00A2")) +
  coord_cartesian(clip = "off") +
  labs(x = "Donation to AMF (cents)", y = NULL,
       title = "Donation") +
  theme_fig +
  theme(
    axis.text.y = element_text(size = 8, lineheight = 0.95),
    panel.grid.major.y = element_blank()
  )

# --- Voucher across timepoints ----------------------------------------------

voucher_t1 <- d1 |>
  mutate(condition = ifelse(condition == "Meat",
                            "Meat Conversation", "Charity Conversation"),
         timepoint = TP_T1) |>
  dplyr::select(condition, timepoint, value = voucher_is_veg)

voucher_t2 <- d |>
  mutate(condition = ifelse(condition == "Meat",
                            "Meat Conversation", "Charity Conversation"),
         timepoint = TP_T2) |>
  dplyr::select(condition, timepoint, value = voucher_is_veg_t2)

n_voucher_t1 <- sum(!is.na(voucher_t1$value))
n_voucher_t2 <- sum(!is.na(voucher_t2$value))

voucher_t12_summary <- bind_rows(voucher_t1, voucher_t2) |>
  mutate(timepoint = factor(timepoint, levels = timepoint_levels)) |>
  group_by(condition, timepoint) |>
  summarise(
    n        = n(),
    n_veg    = sum(value, na.rm = TRUE),
    prop     = mean(value, na.rm = TRUE) * 100,
    se       = sqrt(prop * (100 - prop) / n),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se,
    .groups  = "drop"
  )

max_x_voucher_t12 <- max(voucher_t12_summary$ci_upper)

voucher_y_labels <- setNames(
  c(paste0(TP_T2, "\n(N = ", n_voucher_t2, ")"),
    paste0(TP_T1, "\n(N = ", n_voucher_t1, ")")),
  c(TP_T2, TP_T1)
)

panel_voucher_t12 <- ggplot(voucher_t12_summary,
                            aes(x = prop, y = timepoint, colour = condition)) +
  geom_point(size = 2.5,
             position = position_dodge(width = behavior_dodge)) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 position = position_dodge(width = behavior_dodge),
                 height = 0.16, linewidth = 0.5) +
  annotate("segment",
           x = max_x_voucher_t12 + 4, xend = max_x_voucher_t12 + 7,
           y = y_t1 - behavior_offset, yend = y_t1 - behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_voucher_t12 + 7, xend = max_x_voucher_t12 + 7,
           y = y_t1 - behavior_offset, yend = y_t1 + behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_voucher_t12 + 4, xend = max_x_voucher_t12 + 7,
           y = y_t1 + behavior_offset, yend = y_t1 + behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("text",
           x = max_x_voucher_t12 + 11, y = y_t1,
           label = "***", size = 3, color = "black", angle = 90) +
  annotate("segment",
           x = max_x_voucher_t12 + 4, xend = max_x_voucher_t12 + 7,
           y = y_t2 - behavior_offset, yend = y_t2 - behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_voucher_t12 + 7, xend = max_x_voucher_t12 + 7,
           y = y_t2 - behavior_offset, yend = y_t2 + behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("segment",
           x = max_x_voucher_t12 + 4, xend = max_x_voucher_t12 + 7,
           y = y_t2 + behavior_offset, yend = y_t2 + behavior_offset,
           color = "black", linewidth = 0.3) +
  annotate("text",
           x = max_x_voucher_t12 + 11, y = y_t2,
           label = "*", size = 3, color = "black", angle = 90) +
  scale_colour_manual(values = colors_condition, name = NULL) +
  scale_y_discrete(labels = voucher_y_labels) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3)),
                     limits = c(0, NA),
                     labels = function(x) paste0(x, "%")) +
  coord_cartesian(clip = "off") +
  labs(x = "% choosing plant-based burger", y = NULL,
       title = "Dietary Choice") +
  theme_fig +
  theme(
    axis.text.y = element_text(size = 8, lineheight = 0.95),
    panel.grid.major.y = element_blank()
  )

# =============================================================================
# Belief change: meat vs charity wrongness scales, T1 vs T2
# =============================================================================
# Each subplot is one Likert-derived change score (post minus pre within wave);
# x = timepoint; points are arm means ± 95% CI. Labels above brackets match
# the corresponding main-analysis p-values at each wave.

BELIEF_LABELS <- c(
  meat    = "Eating Meat\nis Morally\nWrong",
  charity = "Not Giving to\nCharity is\nMorally Wrong"
)

belief_t1_long <- d1 |>
  mutate(condition = ifelse(condition == "Meat",
                            "Meat Conversation", "Charity Conversation"),
         timepoint = TP_T1) |>
  dplyr::select(condition, timepoint,
                meat    = meat_wrong_change,
                charity = charity_wrong_change)

belief_t2_long <- d |>
  mutate(condition = ifelse(condition == "Meat",
                            "Meat Conversation", "Charity Conversation"),
         timepoint = TP_T2,
         meat_wrong_change_t2    = meat_wrong_t2    - meat_wrong_pre,
         charity_wrong_change_t2 = charity_wrong_t2 - charity_wrong_pre) |>
  dplyr::select(condition, timepoint,
                meat    = meat_wrong_change_t2,
                charity = charity_wrong_change_t2)

belief_t12 <- bind_rows(belief_t1_long, belief_t2_long) |>
  pivot_longer(c(meat, charity),
               names_to = "belief_var", values_to = "change") |>
  mutate(
    belief_type = factor(BELIEF_LABELS[belief_var],
                         levels = unname(BELIEF_LABELS)),
    timepoint   = factor(timepoint, levels = c(TP_T1, TP_T2))
  )

belief_t12_summary <- belief_t12 |>
  group_by(condition, belief_type, timepoint, belief_var) |>
  summarise(
    mean     = mean(change, na.rm = TRUE),
    se       = sd(change, na.rm = TRUE) / sqrt(n()),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se,
    .groups  = "drop"
  )

# One ggplot per moral statement; bracket geometry uses discrete x positions 1 = T1, 2 = T2.

belief_sig_alt <- belief_t12_summary |>
  group_by(belief_type, timepoint, belief_var) |>
  summarise(top = max(ci_upper), .groups = "drop") |>
  mutate(
    label    = case_when(
      timepoint == TP_T1 ~ "***",
      timepoint == TP_T2 ~ "n.s."
    ),
    x_center = ifelse(timepoint == TP_T1, 1, 2)
  )

n_belief_t1 <- belief_t1_long |>
  filter(!is.na(meat), !is.na(charity)) |>
  nrow()
n_belief_t2 <- belief_t2_long |>
  filter(!is.na(meat), !is.na(charity)) |>
  nrow()

TP_LABELS_SHORT <- c(
  setNames(paste0("Immediately\npost-treatment\n(N = ", n_belief_t1, ")"), TP_T1),
  setNames(paste0("1-month\nfollow-up\n(N = ", n_belief_t2, ")"),          TP_T2)
)

# Identical y-limits across the two belief subplots for direct comparison
belief_y_pad_top    <- 7   # headroom for sig brackets/labels
belief_y_pad_bottom <- 2
belief_y_lim <- c(
  min(c(belief_t12_summary$ci_lower, 0)) - belief_y_pad_bottom,
  max(belief_t12_summary$ci_upper) + belief_y_pad_top
)

build_belief_alt_panel <- function(measure_var, panel_title) {
  data_sub <- belief_t12_summary |> filter(belief_var == measure_var)
  sig_sub  <- belief_sig_alt    |> filter(belief_var == measure_var)

  y_lab <- if (measure_var == "meat") {
    "Belief Change\n(0-100 scale)"
  } else {
    NULL
  }

  ggplot(data_sub, aes(x = timepoint, y = mean, color = condition)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.4) +
    geom_point(position = position_dodge(width = 0.5), size = 2.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  position = position_dodge(width = 0.5),
                  width = 0.12, linewidth = 0.5) +
    geom_segment(data = sig_sub,
                 aes(x = x_center - 0.125, xend = x_center - 0.125,
                     y = top + 3, yend = top + 4),
                 color = "black", linewidth = 0.3, inherit.aes = FALSE) +
    geom_segment(data = sig_sub,
                 aes(x = x_center - 0.125, xend = x_center + 0.125,
                     y = top + 4, yend = top + 4),
                 color = "black", linewidth = 0.3, inherit.aes = FALSE) +
    geom_segment(data = sig_sub,
                 aes(x = x_center + 0.125, xend = x_center + 0.125,
                     y = top + 3, yend = top + 4),
                 color = "black", linewidth = 0.3, inherit.aes = FALSE) +
    geom_text(data = sig_sub,
              aes(x = x_center, y = top + 5.2, label = label),
              size = 3, color = "black", inherit.aes = FALSE) +
    scale_color_manual(values = colors_condition, name = NULL) +
    scale_x_discrete(labels = TP_LABELS_SHORT) +
    coord_cartesian(ylim = belief_y_lim, clip = "off") +
    labs(x = NULL, y = y_lab, title = panel_title) +
    theme_fig +
    theme(
      axis.text.x = element_text(size = 7.5, lineheight = 0.9),
      plot.title  = element_text(face = "bold", size = 8, hjust = 0.5,
                                 lineheight = 0.95, margin = margin(b = 4))
    )
}

panel_belief_alt_meat    <- build_belief_alt_panel(
  "meat",    "\u201cEating meat is morally wrong\u201d"
)
panel_belief_alt_charity <- build_belief_alt_panel(
  "charity", "\u201cNot giving to charity is morally wrong\u201d"
)

# =============================================================================
# Participant reasoning composite (meat themes | charity themes)
# =============================================================================

panel_d_standalone <- panel_d + labs(tag = "A") + tag_style
panel_e_standalone <- panel_e + labs(tag = "B") + tag_style

fig_reasoning <- plot_grid(panel_d_standalone, panel_e_standalone,
                           nrow = 1, rel_widths = c(1, 1), align = "none")

# =============================================================================
# Strategy densities beside a Meat / Charity legend (get_legend with colors_condition)
# =============================================================================

legend_plot <- get_legend(
  panel_donation_t12 +
    guides(color = guide_legend(direction = "vertical")) +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm")
    )
)

fig_strategies <- plot_grid(panel_f, legend_plot,
                            nrow = 1, rel_widths = c(3, 1), align = "none")

# =============================================================================
# Behavior composite: donation and voucher side by side, shared condition legend
# =============================================================================

panel_donation_tagged <- panel_donation_t12 + labs(tag = "A") + tag_style +
  theme(legend.position = "none")
panel_voucher_tagged  <- panel_voucher_t12 + labs(tag = "B") + tag_style +
  theme(legend.position = "none")

legend_behavior <- get_legend(
  panel_donation_t12 +
    guides(colour = guide_legend(direction = "horizontal", nrow = 1)) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      legend.margin = margin(t = 4, b = 0)
    )
)

plots_behavior_row <- plot_grid(panel_donation_tagged, panel_voucher_tagged,
                                nrow = 1, rel_widths = c(1, 1), align = "none")

fig_behavior <- plot_grid(plots_behavior_row, legend_behavior,
                          ncol = 1, rel_heights = c(1, 0.07))

# =============================================================================
# Belief composite: both moral statements + shared horizontal legend below
# =============================================================================

panel_belief_alt_meat_tagged <- panel_belief_alt_meat +
  labs(tag = "A") + tag_style +
  theme(legend.position = "none")

panel_belief_alt_charity_tagged <- panel_belief_alt_charity +
  labs(tag = "B") + tag_style +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y  = element_blank())

legend_belief <- get_legend(
  panel_belief_alt_meat +
    guides(color = guide_legend(direction = "horizontal", nrow = 1)) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      legend.margin = margin(t = 4, b = 0)
    )
)

panels_belief_row <- plot_grid(
  panel_belief_alt_meat_tagged,
  panel_belief_alt_charity_tagged,
  nrow = 1,
  rel_widths = c(1.15, 1),
  align = "h",
  axis = "tb"
)

fig_belief_change <- plot_grid(
  panels_belief_row,
  legend_belief,
  ncol = 1,
  rel_heights = c(1, 0.07)
)

# =============================================================================
# Write PNGs
# =============================================================================

save_plot(file.path(output_dir, "fig-belief-change.png"),
          fig_belief_change,
          base_width = FIGURE_WIDTH_BEHAVIOR_BELIEF, base_height = FIGURE_HEIGHT / 3,
          dpi = FIGURE_DPI, bg = "white")
cat("Saved: output/study_1/figures/fig-belief-change.png\n")

save_plot(file.path(output_dir, "fig-behavior.png"),
          fig_behavior,
          base_width = FIGURE_WIDTH_BEHAVIOR_BELIEF, base_height = FIGURE_HEIGHT / 3,
          dpi = FIGURE_DPI, bg = "white")
cat("Saved: output/study_1/figures/fig-behavior.png\n")

save_plot(file.path(output_dir, "fig-participant-reasoning.png"),
          fig_reasoning,
          base_width = FIGURE_WIDTH, base_height = FIGURE_HEIGHT / 3,
          dpi = FIGURE_DPI, bg = "white")
cat("Saved: output/study_1/figures/fig-participant-reasoning.png\n")

save_plot(file.path(output_dir, "fig-ai-strategies.png"),
          fig_strategies,
          base_width = FIGURE_WIDTH, base_height = FIGURE_HEIGHT / 3,
          dpi = FIGURE_DPI, bg = "white")
cat("Saved: output/study_1/figures/fig-ai-strategies.png\n")

# =============================================================================
# Paper PDFs
# =============================================================================

paper_figs <- list(
  list(fig_belief_change, "fig-belief-change.pdf", FIGURE_WIDTH_BEHAVIOR_BELIEF),
  list(fig_behavior, "fig-behavior.pdf", FIGURE_WIDTH_BEHAVIOR_BELIEF),
  list(fig_reasoning, "fig-participant-reasoning.pdf", FIGURE_WIDTH),
  list(fig_strategies, "fig-ai-strategies.pdf", FIGURE_WIDTH)
)
for (f in paper_figs) {
  save_paper_pdf(
    f[[1]], f[[2]], width = f[[3]], height = FIGURE_HEIGHT / 3,
    output_dir = output_dir, png = FALSE
  )
}
