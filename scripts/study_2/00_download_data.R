# =============================================================================
# 00 · Download Study 2 data from OSF
# =============================================================================
# Downloads survey files from OSF node rwn89 / study_2/data into data/study_2/,
# and LLM text-rating CSVs from study_2/text_ratings into
# data/study_2/text_ratings/. Skips files that already exist locally.
#
# Text ratings may not yet be on OSF. If that folder is missing but the local
# rating files are already present, the download step continues with a warning.

source(here::here("scripts", "functions", "download_data.R"))

download_osf_data(
  "rwn89",
  osf_path = "study_2/data",
  dest_dir = here::here("data", "study_2")
)

ratings_dir <- here::here("data", "study_2", "text_ratings")
required_ratings <- c(
  "strategy_ratings.csv",
  "classifications_meat.csv",
  "classifications_charity.csv",
  "classifications_lying.csv",
  "classifications_polviol.csv",
  "direct_lie_moves_claude-opus-55.csv",
  "direct_lie_moves_gpt-55.csv",
  "direct_lie_moves_grok-47.csv"
)

tryCatch(
  download_osf_data(
    "rwn89",
    osf_path = "study_2/text_ratings",
    dest_dir = ratings_dir
  ),
  error = function(e) {
    present <- file.exists(file.path(ratings_dir, required_ratings))
    if (all(present)) {
      message(
        "OSF study_2/text_ratings unavailable (", conditionMessage(e),
        "). Using local files in ", ratings_dir, "."
      )
    } else {
      stop(
        "Could not download study_2/text_ratings from OSF, and local files are incomplete.\n",
        "Missing: ", paste(required_ratings[!present], collapse = ", "), "\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  }
)
