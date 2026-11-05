# =============================================================================
# Optional regeneration of Study 2 LLM text ratings
# =============================================================================
# Sourced from main.R only when rerun_text_analysis is TRUE.
# Each Python script calls OpenRouter and writes under output/study_2/text_analysis/.
# --write-data copies the paper CSVs into data/study_2/text_ratings/.
# Requires Python 3, the packages in scripts/text_analysis/*/requirements.txt,
# and OPENROUTER_API_KEY.

py <- Sys.which("python3")
if (!nzchar(py)) py <- Sys.which("python")
if (!nzchar(py)) {
  stop("python3 is required when rerun_text_analysis is TRUE.", call. = FALSE)
}
if (!nzchar(Sys.getenv("OPENROUTER_API_KEY"))) {
  stop(
    "Set OPENROUTER_API_KEY before regenerating text ratings.",
    call. = FALSE
  )
}

run_py <- function(rel, args) {
  script <- here::here("scripts", "text_analysis", rel)
  message("--- ", rel, " ---")
  status <- system2(py, c(script, args))
  if (!is.null(status) && status != 0) {
    stop("Text rating script failed: ", rel, call. = FALSE)
  }
}

run_py(file.path("reasons", "classify_meat.py"), c("--run", "--write-data"))
run_py(file.path("reasons", "classify_charity.py"), c("--run", "--write-data"))
run_py(file.path("reasons", "classify_lying.py"), c("--run", "--write-data"))
run_py(file.path("reasons", "classify_polviol.py"), c("--run", "--write-data"))
run_py(file.path("arguments", "classify_persuasion_strategies.py"), "--run")
run_py(
  file.path("arguments", "write_strategy_ratings.py"),
  c("--run", "--write-data")
)
run_py(
  file.path("arguments", "classify_direct_lie_moves.py"),
  c("--run", "--write-data")
)
