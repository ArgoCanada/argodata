
#' Read Argo meta NetCDF files
#'
#' @inheritParams argo_vars
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_nc_meta
#'
#' @examples
#' nc_meta_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_meta.nc",
#'   package = "argodata"
#' )
#' nc_meta <- ncdf4::nc_open(nc_meta_file)
#'
#' argo_nc_meta_read_config_param(nc_meta)
#'
#' ncdf4::nc_close(nc_meta)
#'
argo_nc_meta_read_config_param <- function(nc) {
  argo_nc_assert_dimensions(nc, c("N_MISSIONS", "N_CONFIG_PARAM"))

  n_missions <- nc$dim$N_MISSIONS$len
  n <- nc$dim$N_CONFIG_PARAM$len

  mission_number <- argo_nc_values(nc, "CONFIG_MISSION_NUMBER")
  mission_number <- lapply(mission_number, vctrs::vec_rep, n)
  names <- argo_nc_values(nc, "CONFIG_PARAMETER_NAME")
  values <- argo_nc_values(nc, "CONFIG_PARAMETER_VALUE")

  argo_nc_new_tibble(nc, mission_number, names, values, nrow = n_missions * n)
}

#' @rdname argo_nc_meta
#' @export
argo_nc_meta_read_missions <- function(nc) {
  argo_nc_assert_dimensions(nc, "N_MISSIONS")
  mission_number <- argo_nc_values(nc, "CONFIG_MISSION_NUMBER")
  comment <- argo_nc_values(nc, "CONFIG_MISSION_COMMENT")
  argo_nc_new_tibble(nc, mission_number, comment, nrow = nc$dim$N_MISSIONS$len)
}

#' @rdname argo_nc_meta
#' @export
argo_nc_meta_read_trans_system <- function(nc) {
  argo_nc_assert_dimensions(nc, "N_TRANS_SYSTEM")
  values <- argo_nc_values(nc, c("TRANS_SYSTEM", "TRANS_SYSTEM_ID", "TRANS_FREQUENCY"))
  argo_nc_new_tibble(nc, values, nrow = nc$dim$N_TRANS_SYSTEM$len)
}

#' @rdname argo_nc_meta
#' @export
argo_nc_meta_read_positioning_system <- function(nc) {
  argo_nc_assert_dimensions(nc, "N_POSITIONING_SYSTEM")
  values <- argo_nc_values(nc, "POSITIONING_SYSTEM")
  argo_nc_new_tibble(nc, values, nrow = nc$dim$N_POSITIONING_SYSTEM$len)
}

#' @rdname argo_nc_meta
#' @export
argo_nc_meta_read_launch_config_param <- function(nc) {
  argo_nc_assert_dimensions(nc, "N_LAUNCH_CONFIG_PARAM")
  values <- argo_nc_values(nc, c("LAUNCH_CONFIG_PARAMETER_NAME", "LAUNCH_CONFIG_PARAMETER_VALUE"))
  argo_nc_new_tibble(nc, values, nrow = nc$dim$N_LAUNCH_CONFIG_PARAM$len)
}

#' @rdname argo_nc_meta
#' @export
argo_nc_meta_read_sensor <- function(nc) {
  argo_nc_assert_dimensions(nc, "N_SENSOR")
  values <- argo_nc_values(
    nc,
    c("SENSOR", "SENSOR_MAKER", "SENSOR_MODEL", "SENSOR_SERIAL_NO")
  )
  argo_nc_new_tibble(nc, values, nrow = nc$dim$N_SENSOR$len)
}

#' @rdname argo_nc_meta
#' @export
argo_nc_meta_read_param <- function(nc) {
  argo_nc_assert_dimensions(nc, "N_PARAM")
  values <- argo_nc_values(
    nc,
    c(
      "PARAMETER",
      "PARAMETER_SENSOR",
      "PARAMETER_UNITS",
      "PARAMETER_ACCURACY",
      "PARAMETER_RESOLUTION",
      "PREDEPLOYMENT_CALIB_EQUATION",
      "PREDEPLOYMENT_CALIB_COEFFICIENT",
      "PREDEPLOYMENT_CALIB_COMMENT"
    )
  )
  argo_nc_new_tibble(nc, values, nrow = nc$dim$N_PARAM$len)
}
