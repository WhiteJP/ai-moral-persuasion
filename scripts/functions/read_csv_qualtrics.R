#' Read a Qualtrics CSV export
#'
#' Reads the data rows (skipping the 2 metadata rows after the header),
#' using the first row as column names. Optionally drops common PII /
#' distribution columns.
#'
#' @param path Path to the Qualtrics CSV file.
#' @param remove_extra_cols Logical; drop Status, IPAddress, etc.
#' @param ... Additional arguments passed to readr::read_csv().
#' @return A tibble.
read_csv_qualtrics <- function(path, remove_extra_cols = TRUE, ...) {
  if (!file.exists(path)) {
    stop("The specified file cannot be found: ", path)
  }

  cols_to_remove <- if (remove_extra_cols) {
    c("Status", "IPAddress", "RecipientLastName", "RecipientFirstName",
      "RecipientEmail", "ExternalReference", "LocationLatitude",
      "LocationLongitude", "DistributionChannel")
  } else {
    character(0)
  }

  readr::read_csv(
    path,
    skip = 3,
    col_names = strsplit(readLines(path, n = 1), ",")[[1]],
    col_select = -tidyr::all_of(cols_to_remove),
    ...
  )
}
