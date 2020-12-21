
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
argo_tech_tech_param <- function(path, download = NULL, quiet = FALSE) {
  tbl <- argo_read_many(
    assert_argo_tech_file,
    argo_read_tech_tech_param,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet
  )

  is_char <- vapply(tbl, is.character, logical(1))
  tbl[is_char] <- lapply(tbl[is_char], stringr::str_trim)
  tbl
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
argo_read_tech_tech_param <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_tech_read_tech_param
  )
}

assert_argo_tech_file <- function(path) {
  argo_assert_path_type(path, "^dac/[a-z]+/([0-9a-zA-Z]+)/\\1_tech\\.nc$", "meta")
}
