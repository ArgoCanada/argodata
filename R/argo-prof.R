
#' Load Argo profiles
#'
#' @inheritParams argo_vars
#' @inheritParams argo_nc_prof_read_levels
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_prof
#'
#' @section Level Variables:
#'
#' Some of these variables may be missing depending on the sensors
#' that are available for a given float.
#'
#' - `pres` (decibar, float): Sea water pressure, equals 0 at sea-level
#' - `pres_qc` (char): quality flag
#' - `pres_adjusted` (decibar, float): Sea water pressure, equals 0 at
#'    sea-level
#' - `pres_adjusted_qc` (char): quality flag
#' - `pres_adjusted_error` (decibar, float): Contains the error on the
#'    adjusted values as determined by the delayed mode QC process
#' - `temp` (degree_Celsius, float): Sea temperature in-situ ITS-90 scale
#' - `temp_qc` (char): quality flag
#' - `temp_adjusted` (degree_Celsius, float): Sea temperature in-situ
#'    ITS-90 scale
#' - `temp_adjusted_qc` (char): quality flag
#' - `temp_adjusted_error` (degree_Celsius, float): Contains the error on
#'    the adjusted values as determined by the delayed mode QC process
#' - `psal` (psu, float): Practical salinity
#' - `psal_qc` (char): quality flag
#' - `psal_adjusted` (psu, float): Practical salinity
#' - `psal_adjusted_qc` (char): quality flag
#' - `psal_adjusted_error` (psu, float): Contains the error on the
#'    adjusted values as determined by the delayed mode QC process
#'
#' @section Profile Variables:
#'
#' - `cycle_number` (int): Float cycle number
#' - `direction` (char): Direction of the station profiles
#' - `data_mode` (char): Delayed mode or real time data
#' - `date` (days since 1950-01-01 00:00:00 UTC, double): Julian day
#'    (UTC) of the station relative to REFERENCE_DATE_TIME
#' - `date_qc` (char): Quality on date and time
#' - `date_location` (days since 1950-01-01 00:00:00 UTC, double): Julian
#'    day (UTC) of the location relative to REFERENCE_DATE_TIME
#' - `latitude` (degree_north, double): Latitude of the station, best
#'    estimate
#' - `longitude` (degree_east, double): Longitude of the station, best
#'    estimate
#' - `position_qc` (char): Quality on position (latitude and longitude)
#' - `profile_pres_qc` (char): Global quality flag of PRES profile
#' - `profile_temp_qc` (char): Global quality flag of TEMP profile
#' - `profile_psal_qc` (char): Global quality flag of PSAL profile
#' - `config_mission_number` (int): Unique number denoting the missions
#'    performed by the float
#'
#' @section History variables:
#'
#' - `history_start_pres` (decibar, float): Start pressure action applied
#'    on
#' - `history_stop_pres` (decibar, float): Stop pressure action applied
#'    on
#' - `history_previous_value` (float): Parameter/Flag previous value
#'    before action
#' - `history_institution` (char): Institution which performed action
#' - `history_step` (char): Step in data processing
#' - `history_software` (char): Name of software which performed action
#' - `history_software_release` (char): Version/release of software which
#'    performed action
#' - `history_reference` (char): Reference of database
#' - `history_date` (char): Date the history record was created
#' - `history_action` (char): Action performed on data
#' - `history_parameter` (char): Station parameter action is performed on
#' - `history_qctest` (char): Documentation of tests performed, tests
#'    failed (in hex form)
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
#'   argo_prof_history("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
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
argo_prof_calib <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_calib,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_prof
#' @export
argo_prof_history <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_history,
    path = path,
    vars = vars,
    meta = c("CYCLE_NUMBER", "JULD"),
    download = download,
    quiet = quiet
  )
}


#' Read Argo profiles
#'
#' @inheritParams argo_read_vars
#' @inheritParams argo_nc_prof_read_levels
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
#' argo_read_prof_history(prof_file)
#'
argo_read_prof_levels <- function(file, vars = NULL, meta = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_prof_read_levels,
    vars = vars, meta = meta
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_prof <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_prof_read_prof,
    vars = vars
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_calib <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_prof_read_calib,
    vars = vars
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_history <- function(file, vars = NULL, meta = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_prof_read_history,
    vars = vars,
    meta = meta
  )
}

assert_argo_prof_file <- function(path) {
  argo_assert_path_type(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/((profiles/(B|S)?(R|D)\\1_[0-9]+D?\\.nc)|(\\1_[A-Z]*prof\\.nc))$",
    "profile"
  )
}
