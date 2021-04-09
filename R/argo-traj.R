
#' Load Argo trajectories
#'
#' Trajectory files contain (1) location measurements and (2)
#' detailed information about each cycle. Use [argo_traj_measurement()]
#' output containing one row per location measurement; use
#' [argo_traj_cycle()] for output containing one row per
#' cycle. Finally, use [argo_traj_history()] to view quality
#' control that has been applied.
#'
#' @inheritParams argo_prof_levels
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_traj
#'
#' @examples
#' with_argo_example_cache({
#'   argo_traj_measurement("dac/csio/2900313/2900313_Rtraj.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_traj_cycle("dac/csio/2900313/2900313_Rtraj.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_traj_param("dac/csio/2900313/2900313_Rtraj.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_traj_history("dac/csio/2900313/2900313_Rtraj.nc")
#' })
#'
argo_traj_measurement <- function(path, vars = NULL, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_traj_file,
    argo_read_traj_measurement,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_traj
#' @export
argo_traj_cycle <- function(path, vars = NULL, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_traj_file,
    argo_read_traj_cycle,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_traj
#' @export
argo_traj_param <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_traj_file,
    argo_read_traj_param,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_traj
#' @export
argo_traj_history <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_traj_file,
    argo_read_traj_history,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
}

#' Read Argo trajectories
#'
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_read_traj
#'
#' @examples
#' traj_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_traj_measurement(traj_file)
#' argo_read_traj_cycle(traj_file)
#' argo_read_traj_param(traj_file)
#' argo_read_traj_history(traj_file)
#'
argo_read_traj_measurement <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_MEASUREMENT",
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_traj
#' @export
argo_read_traj_cycle <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_CYCLE",
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_traj
#' @export
argo_read_traj_param <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_PARAM",
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_traj
#' @export
argo_read_traj_history <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_HISTORY",
    vars = vars,
    quiet = quiet
  )
}

assert_argo_traj_file <- function(path) {
  argo_assert_path_type(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/\\1_B?(R|D)traj\\.nc$",
    "trajectory"
  )
}
