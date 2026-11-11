# =============================================================================
# 07 · Direct-lie coding of lying-condition conversations
# =============================================================================
# Three LLMs (Claude Opus 5.5, GPT-5.5, Grok 4.7) coded each lying-condition
# conversation on mentions_direct, contrasts_direct, and clears_direct.
# A flag is yes when at least two of the three models say yes. The flags
# stay separate: a conversation can both set a direct lie apart and argue
# that it is not morally wrong.
#
# The main-text groups, in order:
#   mentions no
#     -> never mentioned a direct lie
#   mentions yes and clears yes
#     -> argued the direct lie is not morally wrong
#        (includes conversations that also set it apart)
#   mentions yes and contrasts yes and clears not yes
#     -> set the direct lie apart and did not clear it
#   mentions yes and neither stance yes
#     -> mentioned, with no majority stance (left out of the two rates)
#
# Prints the shares and heads rates reported in the lying paragraph, plus
# the share of conversations on which all three models agreed.

library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(here)

source(here("scripts", "functions", "helpers.R"))

options(width = max(getOption("width"), 200L))

rating_dir <- here("data", "study_2", "text_ratings")
rater_files <- c(
  claude = "direct_lie_moves_claude-opus-55.csv",
  gpt    = "direct_lie_moves_gpt-55.csv",
  grok   = "direct_lie_moves_grok-47.csv"
)

flag_cols <- c("mentions_direct", "contrasts_direct", "clears_direct")

read_rater <- function(path, name) {
  if (!file.exists(path)) stop("Missing direct-lie ratings: ", path)
  d <- read_csv(path, show_col_types = FALSE)
  if (!all(c("id", flag_cols) %in% names(d))) {
    stop("Unexpected columns in ", path)
  }
  d <- d |>
    transmute(
      id = as.character(id),
      across(all_of(flag_cols), ~ tolower(trimws(as.character(.x))))
    ) |>
    distinct(id, .keep_all = TRUE)
  n_dup <- nrow(read_csv(path, show_col_types = FALSE)) - nrow(d)
  if (n_dup > 0L) {
    cat(sprintf("  Deduped %d extra row(s) in %s\n", n_dup, basename(path)))
  }
  names(d)[names(d) != "id"] <- paste0(names(d)[names(d) != "id"], "_", name)
  d
}

majority_label <- function(...) {
  vals <- c(...)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0L) return(NA_character_)
  tab <- sort(table(vals), decreasing = TRUE)
  if (as.integer(tab[1]) < 2L) return("no_majority")
  names(tab)[1]
}

section("DIRECT-LIE CODING (lying conversations)")

raters <- imap(rater_files, function(file, name) {
  read_rater(file.path(rating_dir, file), name)
})

coded <- reduce(raters, full_join, by = "id")
if (any(is.na(coded$id)) || nrow(coded) == 0L) {
  stop("Failed to merge direct-lie rater files.")
}

# Require every lying conversation to have all three raters.
missing_any <- coded |>
  filter(if_any(starts_with("mentions_direct_"), is.na) |
           if_any(starts_with("contrasts_direct_"), is.na) |
           if_any(starts_with("clears_direct_"), is.na))
if (nrow(missing_any) > 0L) {
  stop(
    sprintf(
      "%d conversation(s) missing a rater flag. First ids: %s",
      nrow(missing_any),
      paste(head(missing_any$id, 5), collapse = ", ")
    )
  )
}

flag_majority <- function(data, flag) {
  cols <- paste0(flag, "_", names(rater_files))
  pmap_chr(data[cols], function(...) majority_label(...))
}

coded <- coded |>
  mutate(
    mentions = flag_majority(coded, "mentions_direct"),
    contrasts = flag_majority(coded, "contrasts_direct"),
    clears = flag_majority(coded, "clears_direct"),
    group = case_when(
      mentions == "no" ~ "never",
      mentions == "yes" & clears == "yes" ~ "cleared",
      mentions == "yes" & contrasts == "yes" & clears != "yes" ~ "set_apart",
      mentions == "yes" ~ "mentioned_neither",
      TRUE ~ "unassigned"
    )
  )

n <- nrow(coded)
cat(sprintf("Lying conversations coded: %d\n", n))
if (any(coded$group == "unassigned")) {
  stop(
    sum(coded$group == "unassigned"),
    " conversation(s) did not fall into a direct-lie group."
  )
}

cat("\nMajority on each flag:\n")
print(as.data.frame(count(coded, mentions, contrasts, clears)))

mentioned <- coded |> filter(mentions == "yes")
cat(sprintf(
  "\nNever mentioned a direct lie: %d / %d (%.1f%%)\n",
  sum(coded$group == "never"), n, 100 * mean(coded$group == "never")
))
cat(sprintf("Mentioned a direct lie: %d\n", nrow(mentioned)))
cat(sprintf(
  "  cleared it:                         %d (%.1f%% of those who mentioned it)\n",
  sum(mentioned$group == "cleared"),
  100 * mean(mentioned$group == "cleared")
))
cat(sprintf(
  "  set it apart and did not clear it:  %d (%.1f%% of those who mentioned it)\n",
  sum(mentioned$group == "set_apart"),
  100 * mean(mentioned$group == "set_apart")
))
cat(sprintf(
  "  mentioned it, no majority stance:   %d\n",
  sum(mentioned$group == "mentioned_neither")
))

# Heads rates by majority category. Rating file id matches participant_id.
d1 <- readRDS(here("data", "study_2", "study2_d1.rds"))
if (!"participant_id" %in% names(d1)) {
  stop("study2_d1.rds is missing participant_id; cannot join direct-lie codes.")
}
if (!"coin_heads" %in% names(d1)) {
  d1 <- d1 |>
    mutate(coin_heads = as.integer(lying_DV == 1))
}
lying <- d1 |>
  filter(condition == "lying") |>
  transmute(
    id = as.character(participant_id),
    coin_heads = as.numeric(coin_heads)
  )

joined <- coded |>
  left_join(lying, by = "id")
if (any(is.na(joined$coin_heads))) {
  n_miss <- sum(is.na(joined$coin_heads))
  warning(sprintf("%d coded conversations missing coin_heads.", n_miss))
}

group_order <- c("never", "cleared", "set_apart", "mentioned_neither")
heads_by_group <- joined |>
  group_by(group) |>
  summarise(
    n = n(),
    heads_rate = mean(coin_heads, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group = factor(group, levels = group_order)) |>
  arrange(group)

cat("\nCoin-flip heads rate by group:\n")
print(as.data.frame(
  heads_by_group |> mutate(heads_pct = round(100 * heads_rate, 1))
))

cleared_heads <- heads_by_group$heads_rate[heads_by_group$group == "cleared"]
set_apart_heads <- heads_by_group$heads_rate[heads_by_group$group == "set_apart"]
cat(sprintf(
  "\nPaper comparison: cleared heads = %.1f%%; set apart and did not clear heads = %.1f%%\n",
  100 * cleared_heads, 100 * set_apart_heads
))

# Flag-level agreement
section("DIRECT-LIE RATER AGREEMENT")
agree_rows <- map_dfr(flag_cols, function(flag) {
  cols <- paste0(flag, "_", names(rater_files))
  mat <- as.matrix(coded[, cols])
  all3 <- mean(mat[, 1] == mat[, 2] & mat[, 1] == mat[, 3])
  tibble(flag = flag, pct_all3 = 100 * all3)
})
print(as.data.frame(agree_rows |> mutate(across(where(is.numeric), ~ round(.x, 1)))))
