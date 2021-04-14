
#' Load Argo float meta
#'
#' Use `argo_meta_*()` functions to extract information from Argo meta
#' NetCDF files. Use [`argo_read_meta_*()`][argo_read_meta_config_param]
#' to extract information from a single previously-downloaded NetCDF file.
#' Using [argo_info()] on meta files is also useful for extracting general
#' float information.
#'
#' @inheritParams argo_prof_levels
#'
#' @return A [tibble::tibble()] with
#'   - `argo_meta_config_param()`: one row per file per configuration parameter and
#'     columns `file`, `n_config_param`, `n_missions`, `config_parameter_value`,
#'     and `config_parameter_name`.
#'   - `argo_meta_missions()`: one row per file per mission and columns `file`,
#'     `n_missions`, `config_mission_number`, and `config_mission_comment`.
#'   - `argo_meta_trans_system()`: one row per file per transmission system
#'     and columns `file`, `n_trans_system`, `trans_system`, `trans_system_id`,
#'     and `trans_frequency`.
#'   - `argo_meta_positioning_system()`: one row per file per positioning
#'     system and columns `file`, `n_positioning_system`, and
#'     `positioning_system`.
#'   - `argo_meta_launch_config_param()`: one row per file per launch
#'     configuration parameter and columns `file`, `n_launch_config_param`,
#'     `launch_config_parameter_name`, and `launch_config_parameter_value`.
#'   - `argo_meta_sensor()`: one row per file per sensor and columns
#'     `file`, `n_sensor`, `sensor`, `sensor_maker`, `sensor_model`,
#'     and `sensor_serial_no`.
#'   - `argo_meta_param()`: one row per file per parameter and columns `file`,
#'     `n_param`, `parameter` `parameter_sensor`, `parameter_units`,
#'     `parameter_resolution`, `predeployment_calib_equation`,
#'     `predeployment_calib_coefficient`, and `predeployment_calib_comment`.
#'
#' @export
#' @rdname argo_meta
#'
#' @examples
#' with_argo_example_cache({
#'   argo_meta_config_param("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_missions("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_trans_system("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_positioning_system("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_launch_config_param("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_sensor("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_param("dac/csio/2900313/2900313_meta.nc")
#' })
#'
argo_meta_config_param <- function(path, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_config_param,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_meta
#' @export
argo_meta_missions <- function(path, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_missions,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_meta
#' @export
argo_meta_trans_system <- function(path, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_trans_system,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_meta
#' @export
argo_meta_positioning_system <- function(path, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_positioning_system,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_meta
#' @export
argo_meta_launch_config_param <- function(path, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_launch_config_param,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_meta
#' @export
argo_meta_sensor <- function(path, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_sensor,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_meta
#' @export
argo_meta_param <- function(path, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_param,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}


#' Read Argo float meta
#'
#' Use `argo_read_meta_*()` functions to extract meta information from a
#' previously-downloaded Argo NetCDF file.
#'
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()] with
#'   - `argo_meta_config_param()`: one row per configuration parameter and
#'     columns `n_config_param`, `N_MISSIONS`, `CONFIG_PARAMETER_VALUE`,
#'     and `CONFIG_PARAMETER_NAME`.
#'   - `argo_meta_missions()`: one row per mission and columns
#'     `N_MISSIONS`, `CONFIG_MISSION_NUMBER`, and `CONFIG_MISSION_COMMENT`.
#'   - `argo_meta_trans_system()`: one row per transmission system
#'     and columns `N_TRANS_SYSTEM`, `TRANS_SYSTEM`, `TRANS_SYSTEM_ID`,
#'     and `TRANS_FREQUENCY`.
#'   - `argo_meta_positioning_system()`: one row per positioning
#'     system and columns `N_POSITIONING_SYSTEM`, and
#'     `POSITIONING_SYSTEM`.
#'   - `argo_meta_launch_config_param()`: one row per launch
#'     configuration parameter and columns `N_LAUNCH_CONFIG_PARAM`,
#'     `LAUNCH_CONFIG_PARAMETER_NAME`, and `LAUNCH_CONFIG_PARAMETER_VALUE`.
#'   - `argo_meta_sensor()`: one row per sensor and columns
#'     `N_SENSOR`, `SENSOR`, `SENSOR_MAKER`, `SENSOR_MODEL`,
#'     and `sensor_serial_no`.
#'   - `argo_meta_param()`: one row per parameter and columns
#'     `N_PARAM`, `PARAMETER` `PARAMETER_SENSOR`, `PARAMETER_UNITS`,
#'     `PARAMETER_RESOLUTION`, `PREDEPLOYMENT_CALIB_EQUATION`,
#'     `PREDEPLOYMENT_CALIB_COEFFICIENT`, and `PREDEPLOYMENT_CALIB_COMMENT`.
#'
#' @export
#' @rdname argo_read_meta
#'
#' @examples
#' meta_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_meta.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_meta_config_param(meta_file)
#' argo_read_meta_missions(meta_file)
#' argo_read_meta_trans_system(meta_file)
#' argo_read_meta_positioning_system(meta_file)
#' argo_read_meta_launch_config_param(meta_file)
#' argo_read_meta_sensor(meta_file)
#' argo_read_meta_param(meta_file)
#'
argo_read_meta_config_param <- function(file, vars = NULL, quiet = FALSE) {
  # Non-standard in that argodata generally does not combine multiple
  # tables into one read function, but doing that here would be terrible
  # for usability. This is still much faster than the previous method.
  params <- argo_nc_read_simple(
    file,
    dims = "N_CONFIG_PARAM",
    quiet = quiet
  )

  values <- argo_nc_read_simple(
    file,
    dims = c("N_CONFIG_PARAM", "N_MISSIONS"),
    quiet = quiet
  )

  if (is.null(params) || is.null(values)) {
    NULL
  } else {
    vctrs::vec_cbind(
      values,
      # could also use vec_rep(params[-1], max(values$n_missions))
      params[rep_len(seq_len(nrow(params)), nrow(values)), -1],
    )
  }
}

#' @rdname argo_read_meta
#' @export
argo_read_meta_missions <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_MISSIONS",
    quiet = quiet
  )
}

#' @rdname argo_read_meta
#' @export
argo_read_meta_trans_system <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_TRANS_SYSTEM",
    quiet = quiet
  )
}

#' @rdname argo_read_meta
#' @export
argo_read_meta_positioning_system <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_POSITIONING_SYSTEM",
    quiet = quiet
  )
}

#' @rdname argo_read_meta
#' @export
argo_read_meta_launch_config_param <- function(file, vars = NULL, quiet = TRUE) {
  argo_nc_read_simple(
    file,
    dims = "N_LAUNCH_CONFIG_PARAM",
    quiet = quiet
  )
}

#' @rdname argo_read_meta
#' @export
argo_read_meta_sensor <- function(file, vars = NULL, quiet = quiet) {
  argo_nc_read_simple(
    file,
    dims = "N_SENSOR",
    quiet = quiet
  )
}

#' @rdname argo_read_meta
#' @export
argo_read_meta_param <- function(file, vars = NULL, quiet = quiet) {
  argo_nc_read_simple(
    file,
    dims = "N_PARAM",
    quiet = quiet
  )
}

assert_argo_meta_file <- function(path) {
  argo_assert_path_type(path, "^dac/[a-z]+/([0-9a-zA-Z]+)/\\1_meta\\.nc$", "meta")
}
