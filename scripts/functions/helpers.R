# =============================================================================
# Shared helper functions
# =============================================================================

# --- Demographic recoding ----------------------------------------------------

edu_labels <- c(
  "< High School", "High School", "Some College",
  "Associate's Degree", "Bachelor's Degree", "Graduate Degree",
  "Prefer not to say"
)

income_labels <- c(
  "<$10K", "$10-20K", "$20-30K", "$30-40K", "$40-50K",
  "$50-60K", "$60-70K", "$70-80K", "$80-90K", "$90-100K",
  "$100-120K", "$120-150K", "$150-200K", "$200-250K",
  "$250-350K", "$350-500K", ">$500K",
  "Don't know", "Prefer not to say"
)

#' Convert ordinal income codes to approximate dollar midpoints
income_numeric <- function(income) {
  L1 <- 350000
  L2 <- 500000

  n1 <- sum(income == 16, na.rm = TRUE)
  n2 <- sum(income == 17, na.rm = TRUE)

  alpha <- log((n1 + n2) / n2) / log(L2 / L1)

  if (is.infinite(alpha) || alpha <= 1) {
    warning("Pareto alpha <= 1 or undefined. Using fallback 1.3 * L2.")
    top_mean <- 1.3 * L2
  } else {
    top_mean <- L2 * alpha / (alpha - 1)
  }

  mids <- c(
    "1"  =  5000, "2"  = 15000, "3"  = 25000, "4"  = 35000, "5"  = 45000,
    "6"  = 55000, "7"  = 65000, "8"  = 75000, "9"  = 85000, "10" = 95000,
    "11" = 110000, "12" = 135000, "13" = 175000, "14" = 225000, "15" = 300000,
    "16" = 425000, "17" = top_mean
  )

  out <- mids[as.character(income)]
  out[income %in% c(18, 19)] <- NA_real_
  as.numeric(out)
}

race_codes <- c(
  "1" = "white", "2" = "black", "3" = "native_american",
  "4" = "asian", "5" = "pacific_islander", "6" = "other",
  "7" = "prefer_not_to_say"
)

#' One-hot encode a multi-select Qualtrics column
multi_hot_df <- function(x, codes, prefix, na_zero = TRUE) {
  s <- as.character(x)
  s <- gsub("[^0-9]", "", s)
  s[is.na(x)] <- NA_character_

  mat <- sapply(names(codes), function(dig) stringr::str_detect(s, stringr::fixed(dig)))
  if (is.null(dim(mat))) mat <- cbind(mat)
  if (na_zero) mat[is.na(mat)] <- FALSE
  storage.mode(mat) <- "integer"

  colnames(mat) <- paste0(prefix, make.names(unname(codes)))
  tibble::as_tibble(mat)
}

# --- Statistical helpers -----------------------------------------------------

#' Winsorize at an upper percentile
winsorize_upper <- function(x, p = 0.95) {
  pmin(x, quantile(x, p, na.rm = TRUE))
}

#' Pooled SD of two groups
pooled_sd <- function(x, y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  n1 <- length(x)
  n2 <- length(y)
  if (n1 < 2 || n2 < 2) return(NA_real_)
  s1 <- sd(x)
  s2 <- sd(y)
  sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
}

#' Cohen's d with NA handling (uses pooled SD of outcome)
safe_d <- function(x, y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  effsize::cohen.d(x, y)$estimate
}

#' Cohen's d using a pre-specified pooled SD (e.g. from full baseline sample)
#' d = (mean_treat - mean_control) / sd_pooled
safe_d_with_sd <- function(treat_vals, control_vals, sd_pooled) {
  m_treat  <- mean(treat_vals, na.rm = TRUE)
  m_control <- mean(control_vals, na.rm = TRUE)
  if (is.na(sd_pooled) || sd_pooled == 0) return(NA_real_)
  (m_treat - m_control) / sd_pooled
}

# --- NLP consensus helper ----------------------------------------------------

#' Majority vote across LLM raters
#'
#' @param votes Logical vector of TRUE/FALSE/NA from each model.
#' @return Single logical, or NA if no valid votes.
compute_consensus <- function(votes) {
  valid_votes <- votes[!is.na(votes)]
  n_valid <- length(valid_votes)
  if (n_valid == 0) return(NA)
  if (n_valid == 1) return(valid_votes[1])
  sum(valid_votes) > n_valid / 2
}

# --- Printing helpers --------------------------------------------------------

section <- function(text, char = "=", width = 70) {
  cat("\n", strrep(char, width), "\n", text, "\n",
      strrep(char, width), "\n", sep = "")
}
