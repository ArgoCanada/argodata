
#' Load Argo profiles
#'
#' @inheritParams argo_download
#' @inheritParams argo_nc_prof_read_levels
#' @param file A previously downloaded Argo profile NetCDF file.
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_prof
#'
#' @examples
#' with_argo_example_cache({
#'   argo_prof_levels("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_prof_prof("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_prof_levels(prof_file)
#' argo_read_prof_prof(prof_file)
#'
argo_prof_levels <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_levels,
    path = path,
    vars = vars,
    meta = c("CYCLE_NUMBER", "JULD"),
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_prof
#' @export
argo_prof_prof <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_prof,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )
}


#' @rdname argo_prof
#' @export
argo_read_prof_levels <- function(file, vars = NULL, meta = NULL) {
  nc <- ncdf4::nc_open(file, suppress_dimvals = TRUE)
  on.exit(ncdf4::nc_close(nc))
  argo_nc_prof_read_levels(nc, vars = vars, meta = meta)
}

#' @rdname argo_prof
#' @export
argo_read_prof_prof <- function(file, vars = NULL) {
  nc <- ncdf4::nc_open(file, suppress_dimvals = TRUE)
  on.exit(ncdf4::nc_close(nc))
  argo_nc_prof_read_prof(nc, vars = vars)
}

assert_argo_prof_file <- function(path) {
  is_prof_file <- stringr::str_detect(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/profiles/(R|D)\\1_[0-9]+D?\\.nc$"
  )

  if (any(!is_prof_file)) {
    bad_files <- path[!is_prof_file]
    paths <- if (length(bad_files) != 1) "paths" else "path"
    bad_files_label <- paste0(
      "'", utils::head(bad_files, 20), "'",
      collapse = "\n"
    )

    abort(
      glue(
        "Found { length(bad_files) } invalid Argo profile { paths }:\n{ bad_files_label}"
      )
    )
  }

  invisible(path)
}
