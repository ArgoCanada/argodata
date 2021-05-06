
#' Extract information from an Argo path
#'
#' @param path A path relative to the root directory of
#'   [argo_mirror()] or [argo_cache_dir()]. This value can also
#'   be a data.frame with a `file` column (e.g., a global index as
#'   returned by [argo_global_meta()] and others).
#' @inheritParams argo_filter_radius
#'
#' @return A [tibble::tibble()] with columns `file`, `file_float`,
#'   `file_type`, `file_cycle`, `file_data_mode`, and `file_modifier`.
#' @export
#'
#' @examples
#' argo_path_info("dac/nmdis/2901633/profiles/R2901633_052.nc")
#'
#' with_argo_example_cache({
#'   argo_extract_path_info(argo_global_meta())
#' })
#'
argo_path_info <- function(path) {
  # this is the most common case, and in the case of a profile
  # index, saves a str_replace() on 1.3 million rows
  if (is.data.frame(path) && ("file" %in% names(path))) {
    file <- path[["file"]]
  } else {
    file <- NULL
  }

  path <- as_argo_path(path)
  filename <- basename(path)

  extract_prof <- stringr::str_match(
    filename,
    "^(B|S)?(R|D)([0-9]+)_([0-9]+)(D)?(_aux)?\\.nc$"
  )

  extract_non_prof <- stringr::str_match(
    filename,
    "^([0-9]+)_(B|S)?(R|D)?(traj|prof|tech|meta)(_aux)?\\.nc"
  )

  is_prof <- is.na(extract_non_prof[, 1])
  is_prof[is.na(extract_prof[, 1]) & is.na(extract_non_prof[, 1])] <- NA

  if (is.null(file)) {
    # stringr::str_replace() / stringr::str_remove() crash R when doing this
    # with all three profile indexes rbinded together for an unknown reason
    file <- gsub("^dac/", "", path)
  }
  file_float <- ifelse(is_prof, extract_prof[, 4], extract_non_prof[, 2])
  file_type <- ifelse(is_prof, "prof", extract_non_prof[, 5])
  file_cycle <- ifelse(is_prof, as.integer(extract_prof[, 5]), NA_integer_)
  file_data_mode <- ifelse(is_prof, extract_prof[, 3], extract_non_prof[, 4])
  file_modifier <- ifelse(is_prof, extract_prof[, 2], extract_non_prof[, 3])
  file_descending <- ifelse(is_prof, !is.na(extract_prof[, 6]), NA)

  tibble::tibble(
    file, file_float, file_type,
    file_cycle, file_data_mode,
    file_modifier, file_descending
  )
}

#' @rdname argo_path_info
#' @export
argo_extract_path_info <- function(tbl) {
  argo_assert_columns(tbl, "file")

  info <- argo_path_info(tbl$file)
  info$file <- NULL

  out <- vctrs::vec_cbind(tbl, info, .name_repair = "check_unique")
  out_names <- insert_vector(names(tbl), names(info), 1 + match("file", names(tbl)))
  out[out_names]
}

#' @rdname argo_path_info
#' @export
as_argo_path <- function(path) {
  if (is.data.frame(path) && ("file" %in% names(path))) {
    path <- path[["file"]]
    path <- paste0(rep_len("dac/", length(path)), path)
  } else if (inherits(path, "argoFloats") && identical(path@metadata$type, "index")) {
    path <- as_argo_path(path@data$index)
  } else if (inherits(path, "argoFloats") && identical(path@metadata$type, "profiles")) {
    path <- fs::path_abs(path.expand(pr@data$file))
  } else if (inherits(path, "argoFloats") && identical(path@metadata$type, "argos")) {
    path <- vapply(path@data$argos, "[[", "filename", FUN.VALUE = character(1))
    path <- fs::path_abs(path.expand(path))
  }

  if (!is.character(path)) {
    abort(
      "`path` must be a character vector or data frame with a character vector column named 'file'."
    )
  }

  as.character(path)
}

#' @rdname argo_path_info
#' @export
as_argo_path_aux <- function(path) {
  path <- as_argo_path(path)
  is_aux <- grepl(".+?_aux.nc$", path)
  aux_path <- gsub("dac/", "aux/", path)
  aux_path <- gsub("\\.nc$", "_aux.nc", aux_path)
  path[!is_aux] <- aux_path
  path
}
