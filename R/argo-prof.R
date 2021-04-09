
#' Load Argo profiles
#'
#' @inheritParams argo_vars
#' @inheritParams argo_read_vars
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
#' with_argo_example_cache({
#'   argo_prof_calib("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_prof_param("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_prof_history("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
argo_prof_levels <- function(path, vars = NULL, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_levels,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_prof
#' @export
argo_prof_prof <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_prof,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_prof
#' @export
argo_prof_calib <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_calib,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_prof
#' @export
argo_prof_param <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_param,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_prof
#' @export
argo_prof_history <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_history,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
}


#' Read Argo profiles
#'
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()]
#' @export
#' @rdname argo_read_prof
#'
#' @examples
#' prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_prof_levels(prof_file)
#' argo_read_prof_prof(prof_file)
#' argo_read_prof_calib(prof_file)
#' argo_read_prof_param(prof_file)
#' argo_read_prof_history(prof_file)
#'
argo_read_prof_levels <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = c("N_LEVELS", "N_PROF"),
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_prof <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_PROF",
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_calib <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = c("N_PARAM", "N_CALIB", "N_PROF"),
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_param <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file, dims = c("N_PARAM", "N_PROF"),
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_history <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = c("N_PROF", "N_HISTORY"),
    vars = vars,
    quiet = quiet
  )
}

assert_argo_prof_file <- function(path) {
  argo_assert_path_type(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/((profiles/(B|S)?(R|D)\\1_[0-9]+D?\\.nc)|(\\1_[A-Z]*prof\\.nc))$",
    "profile"
  )
}
