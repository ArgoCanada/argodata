
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
#' @section Measurement Variables:
#'
#' - `date` (days since 1950-01-01 00:00:00 UTC, double): Julian day
#'    (UTC) of each measurement relative to REFERENCE_DATE_TIME
#' - `date_status` (char): Status of the date and time
#' - `date_qc` (char): Quality on date and time
#' - `date_adjusted` (days since 1950-01-01 00:00:00 UTC, double):
#'    Adjusted julian day (UTC) of each measurement relative to
#'    REFERENCE_DATE_TIME
#' - `date_adjusted_status` (char): Status of the JULD_ADJUSTED date
#' - `date_adjusted_qc` (char): Quality on adjusted date and time
#' - `latitude` (degree_north, double): Latitude of each location
#' - `longitude` (degree_east, double): Longitude of each location
#' - `position_accuracy` (char): Estimated accuracy in latitude and
#'    longitude
#' - `position_qc` (char): Quality on position
#' - `cycle_number` (int): Float cycle number of the measurement
#' - `cycle_number_adjusted` (int): Adjusted float cycle number of the
#'    measurement
#' - `measurement_code` (int): Flag referring to a measurement event in
#'    the cycle
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
#' - `axes_error_ellipse_major` (meters, float): Major axis of error
#'    ellipse from positioning system
#' - `axes_error_ellipse_minor` (meters, float): Minor axis of error
#'    ellipse from positioning system
#' - `axes_error_ellipse_angle` (Degrees (from North when heading East),
#'    float): Angle of error ellipse from positioning system
#' - `satellite_name` (char): Satellite name from positioning system
#'
#' @section Cycle Variables:
#'
#' - `date_descent_start` (days since 1950-01-01 00:00:00 UTC, double):
#'    Descent start date of the cycle
#' - `date_descent_start_status` (char): Status of descent start date of
#'    the cycle
#' - `date_first_stabilization` (days since 1950-01-01 00:00:00 UTC,
#'    double): Time when a float first becomes water-neutral
#' - `date_first_stabilization_status` (char): Status of time when a
#'    float first becomes water-neutral
#' - `date_descent_end` (days since 1950-01-01 00:00:00 UTC, double):
#'    Descent end date of the cycle
#' - `date_descent_end_status` (char): Status of descent end date of the
#'    cycle
#' - `date_park_start` (days since 1950-01-01 00:00:00 UTC, double):
#'    Drift start date of the cycle
#' - `date_park_start_status` (char): Status of drift start date of the
#'    cycle
#' - `date_park_end` (days since 1950-01-01 00:00:00 UTC, double): Drift
#'    end date of the cycle
#' - `date_park_end_status` (char): Status of drift end date of the cycle
#' - `date_deep_descent_end` (days since 1950-01-01 00:00:00 UTC,
#'    double): Deep descent end date of the cycle
#' - `date_deep_descent_end_status` (char): Status of deep descent end
#'    date of the cycle
#' - `date_deep_park_start` (days since 1950-01-01 00:00:00 UTC, double):
#'    Deep park start date of the cycle
#' - `date_deep_park_start_status` (char): Status of deep park start date
#'    of the cycle
#' - `date_ascent_start` (days since 1950-01-01 00:00:00 UTC, double):
#'    Start date of the ascent to the surface
#' - `date_ascent_start_status` (char): Status of start date of the
#'    ascent to the surface
#' - `date_deep_ascent_start` (days since 1950-01-01 00:00:00 UTC,
#'    double): Deep ascent start date of the cycle
#' - `date_deep_ascent_start_status` (char): Status of deep ascent start
#'    date of the cycle
#' - `date_ascent_end` (days since 1950-01-01 00:00:00 UTC, double): End
#'    date of ascent to the surface
#' - `date_ascent_end_status` (char): Status of end date of ascent to the
#'    surface
#' - `date_transmission_start` (days since 1950-01-01 00:00:00 UTC,
#'    double): Start date of transmission
#' - `date_transmission_start_status` (char): Status of start date of
#'    transmission
#' - `date_first_message` (days since 1950-01-01 00:00:00 UTC, double):
#'    Date of earliest float message received
#' - `date_first_message_status` (char): Status of date of earliest float
#'    message received
#' - `date_first_location` (days since 1950-01-01 00:00:00 UTC, double):
#'    Date of earliest location
#' - `date_first_location_status` (char): Status of date of earliest
#'    location
#' - `date_last_location` (days since 1950-01-01 00:00:00 UTC, double):
#'    Date of latest location
#' - `date_last_location_status` (char): Status of date of latest
#'    location
#' - `date_last_message` (days since 1950-01-01 00:00:00 UTC, double):
#'    Date of latest float message received
#' - `date_last_message_status` (char): Status of date of latest float
#'    message received
#' - `date_transmission_end` (days since 1950-01-01 00:00:00 UTC,
#'    double): Transmission end date
#' - `date_transmission_end_status` (char): Status of transmission end
#'    date
#' - `clock_offset` (days, double): Time of float clock drift
#' - `grounded` (days, char): Did the profiler touch the ground for that
#'    cycle?
#' - `representative_park_pressure` (decibar, float): Best pressure value
#'    during park phase
#' - `representative_park_pressure_status` (char): Status of best
#'    pressure value during park phase
#' - `config_mission_number` (int): Unique number denoting the missions
#'    performed by the float
#' - `cycle_number_index` (int): Cycle number that corresponds to the
#'    current index
#' - `cycle_number_index_adjusted` (int): Adjusted cycle number that
#'    corresponds to the current index
#' - `data_mode` (char): Delayed mode or real time data
#'
#' @section History Variables:
#'
#' - `history_previous_value` (float): Parameter/Flag previous value
#'    before action
#' - `history_index_dimension` (char): Name of dimension to which
#'    HISTORY_START_INDEX and HISTORY_STOP_INDEX correspond
#' - `history_start_index` (int): Start index action applied on
#' - `history_stop_index` (int): Stop index action applied on
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
#'   argo_traj_measurement("dac/csio/2900313/2900313_Rtraj.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_traj_cycle("dac/csio/2900313/2900313_Rtraj.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_traj_history("dac/csio/2900313/2900313_Rtraj.nc")
#' })
#'
argo_traj_measurement <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
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
argo_traj_cycle <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
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
argo_traj_history <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  argo_read_many(
    assert_argo_traj_file,
    argo_read_traj_history,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )
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
#' argo_read_traj_history(traj_file)
#'
argo_read_traj_measurement <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_traj_read_measurement,
    vars = vars
  )
}

#' @rdname argo_read_traj
#' @export
argo_read_traj_cycle <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_traj_read_cycle,
    vars = vars
  )
}

#' @rdname argo_read_traj
#' @export
argo_read_traj_history <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_traj_read_history,
    vars = vars
  )
}

assert_argo_traj_file <- function(path) {
  argo_assert_path_type(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/\\1_B?(R|D)traj\\.nc$",
    "trajectory"
  )
}
