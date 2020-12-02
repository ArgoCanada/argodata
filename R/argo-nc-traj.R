
#' Read Argo trajectory NetCDF files
#'
#' @inheritParams argo_nc_read_vars
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
#' argo_nc_traj_vars_cycle(nc_traj)
#' argo_nc_traj_vars_history(nc_traj)
#'
#' argo_nc_traj_read_meas(nc_traj)
#' argo_nc_traj_read_cycle(nc_traj)
#' argo_nc_traj_read_history(nc_traj)
#'
#' ncdf4::nc_close(nc_traj)
#'
argo_nc_traj_read_meas <- function(nc, vars = NULL) {
  nc_vars <- argo_nc_traj_vars_meas(nc)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)
  n <- nc$dim$N_MEASUREMENT$len

  if (is.null(n)) {
    abort(glue("'{ nc$filename }' is missing dimension 'N_MEASUREMENT'"))
  }

  values <- argo_nc_values(nc, vars)
  values <- argo_string_to_chars_tbl(values)

  tibble::new_tibble(values, nrow = n)
}

#' @rdname argo_nc_traj
#' @export
argo_nc_traj_read_cycle <- function(nc, vars = NULL) {
  nc_vars <- argo_nc_traj_vars_cycle(nc)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)
  n <- nc$dim$N_CYCLE$len

  if (is.null(n)) {
    abort(glue("'{ nc$filename }' is missing dimension 'N_CYCLE'"))
  }

  values <- argo_nc_values(nc, vars)
  values <- argo_string_to_chars_tbl(values)

  tibble::new_tibble(values, nrow = n)
}

#' @rdname argo_nc_traj
#' @export
argo_nc_traj_read_history <- function(nc, vars = NULL) {
  nc_vars_reg <- argo_nc_vars_by_dimension(nc, 1, "N_HISTORY")
  nc_vars_string <- argo_nc_vars_by_dimension(nc, 2, "N_HISTORY")
  nc_vars <- c(nc_vars_reg, nc_vars_string)

  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)
  n <- nc$dim$N_HISTORY$len

  if (is.null(n)) {
    abort(glue("'{ nc$filename }' is missing dimension 'N_HISTORY'"))
  }

  # regular values don't have a string dimension, so need
  # argo_string_to_chars_tbl()
  values <- argo_nc_values(nc, intersect(vars, nc_vars_reg))
  values <- argo_string_to_chars_tbl(values)

  # string values are fixed-width, so need whitespace trimmed
  values_string <- argo_nc_values(nc, intersect(vars, nc_vars_string))
  values_string <- lapply(values_string, stringr::str_trim)

  tibble::new_tibble(c(values, values_string), nrow = n)
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

#' @rdname argo_nc_traj
#' @export
argo_nc_traj_vars_history <- function(nc) {
  c(
    argo_nc_vars_by_dimension(nc, 1, "N_HISTORY"),
    # for these variables, the first dimension is a string dimension
    argo_nc_vars_by_dimension(nc, 2, "N_HISTORY")
  )
}
