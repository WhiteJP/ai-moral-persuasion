#' Download data files from an OSF node
#'
#' Retrieves files from the specified OSF node and saves them into dest_dir.
#' If the OSF node contains subfolders, lists their contents recursively so
#' that all files end up flat in dest_dir. Skips files that already exist
#' unless overwrite = TRUE.
#'
#' @param osf_node_id Character string, the 5-character OSF node identifier.
#' @param osf_path Optional subfolder path within the OSF node (e.g. "study_1/data").
#'   If NULL, downloads all top-level files and recurses into folders.
#' @param dest_dir Local directory to save downloaded files.
#' @param overwrite If TRUE, re-download even if files already exist.
download_osf_data <- function(osf_node_id,
                              osf_path = NULL,
                              dest_dir = here::here("data"),
                              overwrite = FALSE) {
  fs::dir_create(dest_dir)

  message("Retrieving OSF node: ", osf_node_id)
  node <- osfr::osf_retrieve_node(osf_node_id)

  if (!is.null(osf_path)) {
    all_files <- osfr::osf_ls_files(node, path = osf_path, n_max = Inf, type = "file")
  } else {
    all_files <- osfr::osf_ls_files(node, n_max = Inf, type = "file")
  }

  if (nrow(all_files) == 0L) {
    stop("No files found on OSF node '", osf_node_id, "'.")
  }

  existing <- fs::path(dest_dir, all_files$name)
  exists_mask <- fs::file_exists(existing)

  if (any(exists_mask) && !overwrite) {
    message("Skipping already-downloaded files: ",
            paste(all_files$name[exists_mask], collapse = ", "))
    all_files <- all_files[!exists_mask, , drop = FALSE]
  }

  if (nrow(all_files) == 0L) {
    message("All files already present in: ", dest_dir)
    return(invisible(NULL))
  }

  message("Downloading ", nrow(all_files), " file(s) to: ", dest_dir)
  downloaded <- osfr::osf_download(
    all_files,
    path = dest_dir,
    conflicts = if (overwrite) "overwrite" else "skip",
    progress = TRUE
  )

  message("Done! Data saved in: ", dest_dir)
  invisible(downloaded)
}
