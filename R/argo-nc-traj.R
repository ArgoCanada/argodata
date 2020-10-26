
#' Read Argo trajectory NetCDF files
#'
#' @inheritParams argo_nc_prof_read_levels
#'
#' @return A [tibble::tibble()] containing columns "float" and
#'   columns listed in `vars`.
#' @export
#' @rdname argo_nc_traj
#'
#' @examples
#' nc_traj_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
#'   package = "argodata"
#' )
#' nc_traj <- ncdf4::nc_open(nc_traj_file)
#'
#' argo_nc_traj_vars_meas(nc_traj)
#' argo_nc_traj_read_meas(nc_traj)
#'
#' ncdf4::nc_close(nc_traj)
#'
argo_nc_traj_read_meas <- function(nc, vars = NULL) {
  nc_vars <- argo_nc_traj_vars_meas(nc)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)
  n <- nc$dim$N_MEASUREMENT$len

  values <- argo_nc_values(nc, vars)
  values <- argo_string_to_chars_tbl(values)

  argo_nc_new_tibble(nc, values, nrow = n)
}

#' @rdname argo_nc_traj
#' @export
argo_nc_traj_read_cycle <- function(nc, vars = NULL) {
  nc_vars <- argo_nc_traj_vars_cycle(nc)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)
  n <- nc$dim$N_CYCLE$len

  values <- argo_nc_values(nc, vars)
  values <- argo_string_to_chars_tbl(values)

  argo_nc_new_tibble(nc, values, nrow = n)
}

#' @rdname argo_nc_traj
#' @export
argo_nc_traj_vars_meas <- function(nc) {
  argo_nc_vars_by_dimension(nc, 1, "N_MEASUREMENT")
}

#' @rdname argo_nc_traj
#' @export
argo_nc_traj_vars_cycle <- function(nc) {
  argo_nc_vars_by_dimension(nc, 1, "N_CYCLE")
}
