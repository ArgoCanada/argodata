
#' Load Argo float meta
#'
#' @inheritParams argo_prof_levels
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_meta
#'
#' @examples
#' with_argo_example_cache({
#'   argo_meta_config("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' meta_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_meta.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_meta_config(meta_file)
#'
argo_meta_config <- function(path, download = NULL, quiet = FALSE) {
  tbl <- argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_config,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet
  )

  tbl$config_parameter_name <- stringr::str_trim(tbl$config_parameter_name)
  tbl
}

#' @rdname argo_meta
#' @export
argo_read_meta_config <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_meta_read_config_param
  )
}

assert_argo_meta_file <- function(path) {
  is_traj_file <- stringr::str_detect(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/\\1_meta\\.nc$"
  )

  if (any(!is_traj_file)) {
    bad_files <- path[!is_traj_file]
    paths <- if (length(bad_files) != 1) "paths" else "path"
    bad_files_label <- paste0(
      "'", utils::head(bad_files, 20), "'",
      collapse = "\n"
    )

    abort(
      glue(
        "Found { length(bad_files) } invalid Argo meta { paths }:\n{ bad_files_label}"
      )
    )
  }

  invisible(path)
}
