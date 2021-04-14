
#' Load Argo float meta
#'
#' @inheritParams argo_prof_levels
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_tech
#'
#' @examples
#' with_argo_example_cache({
#'   argo_tech_tech_param("dac/csio/2900313/2900313_tech.nc")
#' })
#'
argo_tech_tech_param <- function(path, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_tech_file,
    argo_read_tech_tech_param,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' Read Argo float technical parameters
#'
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()]
#' @export
#' @rdname argo_read_meta
#'
#' @examples
#' tech_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_tech.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_tech_tech_param(tech_file)
#'
argo_read_tech_tech_param <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_TECH_PARAM",
    quiet = quiet
  )
}

assert_argo_tech_file <- function(path) {
  argo_assert_path_type(path, "^dac/[a-z]+/([0-9a-zA-Z]+)/\\1_tech\\.nc$", "tech")
}
