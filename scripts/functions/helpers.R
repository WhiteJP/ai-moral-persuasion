# =============================================================================
# Shared helper functions
# =============================================================================

# --- p-values and significance display ---------------------------------------

fmt_p <- function(p) {
  ifelse(is.na(p), "NA", ifelse(p < 0.001, "<.001", sprintf("%.3f", p)))
}

fmt_sig <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ ".",
    TRUE      ~ ""
  )
}

#' Stars for figure brackets. Non-significant cells use `ns` (default "n.s.").
fmt_bracket_sig <- function(p, ns = "n.s.") {
  dplyr::case_when(
    is.na(p)  ~ ns,
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ns
  )
}

#' LaTeX p-cell. Leading zero stripped (.042). Bold when p < .05 if `bold`.
fmt_p_tex <- function(p, bold = TRUE) {
  cell <- ifelse(
    is.na(p), "---",
    ifelse(
      p < 0.001, "<.001",
      ifelse(p > 0.999, "1.00", sub("^0", "", sprintf("%.3f", p)))
    )
  )
  if (isTRUE(bold)) {
    ifelse(!is.na(p) & p < 0.05, paste0("\\textbf{", cell, "}"), cell)
  } else {
    cell
  }
}

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

#' Within-arm pooled SD of two groups.
#' Used for the Study 1 donation standardizer: each arm is the other's
#' control, and pooling within arms keeps the treatment's mean shift out of
#' the denominator. Cohen's d itself is the model coefficient divided by a
#' frozen standardizer; see scripts/study_1/04_main_analyses.R and
#' study2_d_sd() in scripts/functions/study2_te_helpers.R.
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

# --- Hause chatbot transcripts -----------------------------------------------

#' Stitch Qualtrics-split Hause chatbot JSON into one row per participant.
#'
#' Chat transcripts are stored double-encoded. Qualtrics splits a long JSON
#' string across chathistory columns, leaving a stray quote on each side of
#' the cut. Pass the columns in order; trailing all-NA columns are ignored.
#'
#' Returns a tibble (same length as the inputs) with the parsed object widened
#' into columns. The turn list is in `messages`.
#'
#' From the effective-giving and god-persuasion analyses.
conv_from_hause_chatbot <- function(...) {
  requireNamespace("jsonlite")
  requireNamespace("tibble")
  requireNamespace("tidyr")
  all_vecs <- list(...)

  if (length(unique(vapply(all_vecs, length, integer(1)))) != 1) {
    stop("All input vectors must have the same length")
  }

  na_matrix <- sapply(all_vecs, is.na)
  nrows <- nrow(na_matrix)
  ncols <- ncol(na_matrix)

  json_strings <- character(nrows)

  for (i in seq_len(nrows)) {
    nas <- na_matrix[i, ]
    if (all(nas)) next

    # Whole string is in the first column.
    if (!nas[1] && all(nas[2:ncols])) {
      json_strings[i] <- all_vecs[[1]][i]
    } else {
      # Last filled column, or the last column if every column is filled.
      last_col <- if (all(!nas)) ncols else which(nas)[1] - 1
      for (j in seq_len(last_col)) {
        s <- all_vecs[[j]][i]
        nchars <- nchar(s)
        piece <- if (j == 1) {
          substr(s, 1, nchars - 1)
        } else if (j < last_col) {
          substr(s, 2, nchars - 1)
        } else {
          substr(s, 2, nchars)
        }
        json_strings[i] <- paste0(json_strings[i], piece)
      }
    }
  }

  json_parsed <- lapply(seq_along(json_strings), function(i) {
    s <- json_strings[[i]]
    tryCatch(
      if (!nzchar(s)) NULL else jsonlite::fromJSON(jsonlite::fromJSON(s)),
      error = function(e) {
        message(sprintf("Error parsing chat JSON in row %d: %s", i, e$message))
        NULL
      }
    )
  })

  out <- tibble::tibble(json_parsed)
  tidyr::unnest_wider(out, json_parsed)
}

# --- Paper outputs -----------------------------------------------------------

# Seed, simulation count, and smoke-test switch from mediation_helpers.R,
# without replacing condition labels or outcome tables already in the session.
import_mediation_settings <- function() {
  if (exists("mediation_n_sims", envir = .GlobalEnv, inherits = FALSE) &&
      exists("MEDIATION_SEED", envir = .GlobalEnv, inherits = FALSE)) {
    return(invisible(TRUE))
  }
  src <- new.env(parent = baseenv())
  sys.source(
    here::here("scripts", "functions", "mediation_helpers.R"),
    envir = src
  )
  keep <- c(
    "MEDIATION_SEED", "MEDIATION_N_SIMS_FULL", "MEDIATION_N_SIMS_FAST",
    "mediation_bootstrap_enabled", "mediation_n_sims",
    "mediation_interval_phrase", "mediation_sims_line",
    "announce_mediation_settings"
  )
  for (nm in keep) assign(nm, get(nm, envir = src), envir = .GlobalEnv)
  invisible(TRUE)
}

# Quartz writes the PDF with the system Arial, so the text metrics match the
# drawn glyphs. Cairo measures Arial against the PostScript font database
# and the spacing comes out uneven, so it is only the non-Mac fallback.
paper_pdf_device <- function() {
  if (isTRUE(capabilities("aqua"))) {
    function(filename, ...) grDevices::quartz(file = filename, type = "pdf", ...)
  } else {
    grDevices::cairo_pdf
  }
}

use_quartz_text_metrics <- function() {
  if (!isTRUE(capabilities("aqua"))) return(invisible(FALSE))
  grDevices::quartzFonts(Arial = grDevices::quartzFont(c(
    "Arial", "Arial Bold", "Arial Italic", "Arial Bold Italic"
  )))
  if (requireNamespace("cowplot", quietly = TRUE)) {
    cowplot::set_null_device(function(width, height) {
      grDevices::quartz(
        file = tempfile(fileext = ".pdf"), type = "pdf",
        width = width, height = height
      )
      grDevices::dev.control("enable")
    })
  }
  # Leave a Quartz PDF device current so text measured outside cowplot,
  # such as ggplotGrob() axis widths, also uses Arial.
  grDevices::quartz(file = tempfile(fileext = ".pdf"), type = "pdf")
  invisible(TRUE)
}

#' Save a paper figure as PDF under output/. A PNG preview stays next to it.
save_paper_pdf <- function(plot, filename, width, height, output_dir,
                           dpi = 300, png = TRUE) {
  stopifnot(grepl("\\.pdf$", filename))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(output_dir, filename)
  ggplot2::ggsave(
    dest, plot,
    width = width, height = height, units = "in", bg = "white",
    device = paper_pdf_device()
  )
  if (png) {
    ggplot2::ggsave(
      file.path(output_dir, sub("\\.pdf$", ".png", filename)), plot,
      width = width, height = height, units = "in", dpi = dpi, bg = "white"
    )
  }
  cat(sprintf("Saved: %s\n", dest))
  invisible(dest)
}

#' PNAS panel label: bold uppercase sans-serif at the top-left corner of the
#' panel, outside the plotting region.
paper_tag_theme <- function(size = 9) {
  ggplot2::theme(
    plot.tag = ggplot2::element_text(
      face = "bold", size = size, family = "Helvetica",
      hjust = 0, vjust = 1, margin = ggplot2::margin(0, 0, 0, 0)
    ),
    plot.tag.location = "plot",
    plot.tag.position = c(0, 1)
  )
}

#' Matching settings for cowplot::plot_grid(labels = ...).
PAPER_LABEL_ARGS <- list(
  label_size = 9, label_fontfamily = "Helvetica", label_fontface = "bold",
  label_x = 0, label_y = 1, hjust = 0, vjust = 1
)

#' Apply the SI typesetting conventions to a generated table.
#'
#' US spelling, a math minus with the digits left in text (so bold and the
#' table font apply), {,} as the thousands separator inside any leftover
#' $N = ...$ math, and the SI table wrappers defined in the paper
#' preambles. Portrait tables use the SITable environment. Landscape tables
#' are a captioned minipage between \SILandscapeStyle and \SILandscapeFooter,
#' because a landscape table float does not rotate in the PNAS SI class.
paper_table_tex <- function(tex) {
  tex <- paste(tex, collapse = "\n")
  tex <- gsub("Behaviour", "Behavior", tex, fixed = TRUE)
  tex <- gsub("behaviour", "behavior", tex, fixed = TRUE)
  tex <- gsub("(^|[ \\[(&]|\\\\textbf\\{|\\\\shortstack\\{)-([0-9]+(\\.[0-9]+)?)",
              "\\1$-$\\2", tex, perl = TRUE)
  tex <- gsub("(\\$N = [0-9]+),([0-9]{3}\\$)", "\\1{,}\\2", tex, perl = TRUE)
  if (grepl("\\begin{landscape}", tex, fixed = TRUE)) {
    tex <- sub(
      "\\begin{table}[!htbp]",
      paste(
        "\\SILandscapeStyle",
        "\\nolinenumbers",
        "\\vspace*{0.5em}",
        "\\noindent\\begin{minipage}{\\linewidth}",
        sep = "\n"
      ),
      tex, fixed = TRUE
    )
    tex <- sub("\\caption{", "\\captionof{table}{", tex, fixed = TRUE)
    tex <- sub(
      "\\end{table}",
      paste("\\end{minipage}", "\\SILandscapeFooter", sep = "\n"),
      tex, fixed = TRUE
    )
  } else {
    tex <- sub("\\begin{table}[!htbp]", "\\begin{SITable}", tex, fixed = TRUE)
    tex <- sub("\\end{table}", "\\end{SITable}", tex, fixed = TRUE)
  }
  tex
}

#' Write a generated table under output/.
write_paper_table <- function(tex, path) {
  writeLines(paper_table_tex(tex), path)
  invisible(path)
}

# --- Printing helpers --------------------------------------------------------

# Result block inside a step. The rule is only as wide as the title, so it
# sits under the step banner from main.R rather than matching it.
section <- function(text, char = "=", width = 70) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  rule_n <- min(as.integer(width), max(12L, max(nchar(lines))))
  cat("\n", text, "\n", strrep(char, rule_n), "\n", sep = "")
}

# A note inside a section: no rule.
subsection <- function(text) {
  cat("\n", text, "\n", sep = "")
}
