
#' Extract information from an Argo path
#'
#' @param path A path relative to the root directory of
#'   [argo_mirror()] or [argo_cache_dir()].
#'
#' @return A [tibble::tibble()] with columns `file`, `file_float`,
#'   `file_type`, `file_cycle`, `file_data_mode`, and `file_modifier`.
#' @export
#'
#' @examples
#' with_argo_example_cache({
#'   argo_path_info(list.files(argo_cache_dir(), recursive = TRUE))
#' })
#'
#'
argo_path_info <- function(path) {
  path <- as_argo_path(path)
  filename <- basename(path)

  extract_prof <- stringr::str_match(
    filename,
    "^(B|S)?(R|D)([0-9]+)_([0-9]+)(D)?\\.nc$"
  )

  extract_non_prof <- stringr::str_match(
    filename,
    "^([0-9]+)_(B|S)?(R|D)?(traj|prof|tech|meta)\\.nc"
  )

  is_prof <- is.na(extract_non_prof[, 1])
  is_prof[is.na(extract_prof[, 1]) & is.na(extract_non_prof[, 1])] <- NA

  file <- stringr::str_remove(path, "^dac/")
  file_float <- ifelse(is_prof, extract_prof[, 4], extract_non_prof[, 2])
  file_type <- ifelse(is_prof, "prof", extract_non_prof[, 5])
  file_cycle <- ifelse(is_prof, as.integer(extract_prof[, 5]), NA_integer_)
  file_data_mode <- ifelse(is_prof, extract_prof[, 3], extract_non_prof[, 4])
  file_modifier <- ifelse(is_prof, extract_prof[, 2], extract_non_prof[, 3])

  tibble::tibble(file, file_float, file_type, file_cycle, file_data_mode, file_modifier)
}

#' @rdname argo_path_info
#' @export
as_argo_path <- function(path) {
  if (is.data.frame(path) && ("file" %in% names(path))) {
    path <- path[["file"]]
    path <- paste0(rep_len("dac/", length(path)), path)
  }

  if (!is.character(path)) {
    abort(
      "`path` must be a character vector or data frame with a character vector column named 'file'."
    )
  }

  path
}
