
#' Load Argo trajectories
#'
#' Use `argo_traj_*()` functions to extract information from Argo trajectory
#' NetCDF files. Use [`argo_read_traj_*()`][argo_read_traj_measurement]
#' to extract information from a single previously-downloaded NetCDF file.
#'
#' @inheritParams argo_prof_levels
#'
#' @return A [tibble::tibble()] with
#'   - `argo_traj_measurement()`: one row per file per measurement.
#'   - `argo_traj_cycle()`: one row per file per cycle.
#'   - `argo_traj_param()`: one row per file per parameter.
#'   - `argo_traj_history()`: one row per file per history entry.
#'
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
  argo_read_many(
    assert_argo_traj_file,
    argo_read_traj_param,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_traj
#' @export
argo_traj_history <- function(path, vars = NULL, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_traj_file,
    argo_read_traj_history,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' Read Argo trajectories
#'
#' Use `argo_read_traj_*()` functions to extract trajectory information from a
#' previously-downloaded Argo NetCDF file.
#'
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()] with
#'   - `argo_read_traj_measurement()`: one per measurement.
#'   - `argo_read_traj_cycle()`: one per cycle.
#'   - `argo_read_traj_param()`: one per parameter.
#'   - `argo_read_traj_history()`: one row per history entry.
#'
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
