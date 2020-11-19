
#' Select rows of Argo tables
#'
#' These functions are intended to be applied to [argo_global_meta()] and
#' other global index tables in the `argo_global_*()` family.
#'
#' @param tbl A data.frame, ideally derived from [argo_global_meta()]
#'   and family. The column conventions used by the global indexes is assumed
#'   (e.g., columns `latitude` and `longitude` exist).
#' @param latitude,longitude A location.
#' @param radius_km A radius from the point described by `latitude` and
#'   `longitude`.
#' @param latitude_max,latitude_min,longitude_max,longitude_min A rectangle
#'   describing the desired bounds. Rectangle where `longitude_min` is greater
#'   than `longitude_max` are interpreted as wrapping across the international
#'   date line.
#' @param date_min,date_max,date_update_min,date_update_max A range of
#'   datetimes. Users are responsible for setting the timezone for these
#'   objects and are encouraged to used UTC.
#' @param parameters A case-insensitive vector of parameters such as those
#'   contained in the [argo_global_bio_prof()] `parameters` column.
#' @param parameter_data_mode,data_mode A case-insensitive vector of data modes
#'   (realtime or delayed) or abbreviations of data modes (R for realtime, D
#'   for delayed).
#' @param float A float identifier.
#'
#' @rdname argo_filter
#' @return `tbl` with rows that match the search criteria.
#' @export
#'
argo_filter_radius <- function(tbl, latitude, longitude, radius_km) {
  abort("Not implemented")
}

#' @rdname argo_filter
#' @export
argo_filter_rect <- function(tbl, latitude_max, latitude_min, longitude_max, longitude_min) {
  abort("Not implemented")
}

#' @rdname argo_filter
#' @export
argo_filter_date <- function(tbl, date_min, date_max = Sys.time()) {
  argo_assert_columns(tbl, "date")

  date_min <- as.POSIXct(date_min, tz = "UTC")
  date_max <- as.POSIXct(date_max, tz = "UTC")

  # called for the error message when incompatible lengths are passed
  vctrs::vec_size_common(
    tbl = tbl,
    date_min = date_min,
    date_max = date_max
  )

  tbl_match <- (tbl$date >= date_min) &
    (tbl$date <= date_max)

  tbl[!is.na(tbl_match) & tbl_match, , drop = FALSE]
}

#' @rdname argo_filter
#' @export
argo_filter_updated <- function(tbl, date_update_min, date_update_max = Sys.time()) {
  argo_assert_columns(tbl, "date_update")

  date_update_min <- as.POSIXct(date_update_min, tz = "UTC")
  date_update_max <- as.POSIXct(date_update_max, tz = "UTC")

  # called for the error message when incompatible lengths are passed
  vctrs::vec_size_common(
    tbl = tbl,
    date_update_min = date_update_min,
    date_update_max = date_update_max
  )

  tbl_match <- (tbl$date_update >= date_update_min) &
    (tbl$date_update <= date_update_max)

  tbl[!is.na(tbl_match) & tbl_match, , drop = FALSE]
}

#' @rdname argo_filter
#' @export
argo_filter_parameters <- function(tbl, parameters, parameter_data_mode = NULL) {
  abort("Not implemented")
}

#' @rdname argo_filter
#' @export
argo_filter_float <- function(tbl, float) {
  abort("Not implemented")
}

#' @rdname argo_filter
#' @export
argo_filter_data_mode <- function(tbl, data_mode) {
  abort("Not implemented")
}
