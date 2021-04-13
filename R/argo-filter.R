
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
#'   describing the desired bounds. A rectangle where `longitude_min` is greater
#'   than `longitude_max` are interpreted as wrapping across the international
#'   date line.
#' @param date_min,date_max,date_update_min,date_update_max A range of
#'   datetimes. Users are responsible for setting the timezone for these
#'   objects and are encouraged to used UTC.
#' @param data_mode One of "realtime" or "delayed".
#' @param direction One of "ascending" or "descending"
#' @param float A float identifier.
#'
#' @rdname argo_filter
#' @return `tbl` with rows that match the search criteria.
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' \dontrun{
#' argo_global_prof() %>%
#'   # within 500 km of Halifax, Nova Scotia
#'   argo_filter_radius(45, -64, 500)
#' }
#'
#' with_argo_example_cache({
#'   argo_global_traj() %>%
#'     argo_filter_rect(40, 60, -64, -54)
#' })
#'
#' with_argo_example_cache({
#'   argo_global_traj() %>%
#'     argo_filter_updated("2020-01-01 00:00") %>%
#'     select(date_update, everything())
#' })
#'
#' with_argo_example_cache({
#'   argo_global_traj() %>%
#'     argo_filter_float(c("13857", "15851"))
#' })
#'
#' with_argo_example_cache({
#'   argo_global_traj() %>%
#'     argo_filter_data_mode("delayed")
#' })
#'
#'
argo_filter_radius <- function(tbl, latitude, longitude, radius_km) {
  latitude <- vec_sanitize(latitude, double(), 1)
  longitude <- vec_sanitize(longitude, double(), 1)
  radius_km <- vec_sanitize(radius_km, double(), 1)

  xy <- list(x = longitude, y = latitude)

  if (tbl_has_latlon_or_rect(tbl) == "latlon") {
    filter_latlon_radius(tbl, xy, radius_km)
  } else {
    filter_rect_radius(tbl, xy, radius_km)
  }
}

#' @rdname argo_filter
#' @export
argo_filter_rect <- function(tbl, latitude_min, latitude_max, longitude_min, longitude_max) {
  latitude_min <- vec_sanitize(latitude_min, double(), 1)
  latitude_max <- vec_sanitize(latitude_max, double(), 1)
  longitude_min <- vec_sanitize(longitude_min, double(), 1)
  longitude_max <- vec_sanitize(longitude_max, double(), 1)

  r_query <- list(
    xmin = longitude_min, xmax = longitude_max,
    ymin = latitude_min, ymax = latitude_max
  )

  if (tbl_has_latlon_or_rect(tbl) == "rect") {
    filter_rect_rect(tbl, r_query)
  } else {
    filter_latlon_rect(tbl, r_query)
  }
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

  argo_do_filter(
    tbl,
    tbl$date >= date_min,
    tbl$date <= date_max
  )
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

  argo_do_filter(
    tbl,
    tbl$date_update >= date_update_min,
    tbl$date_update <= date_update_max
  )
}

#' @rdname argo_filter
#' @export
argo_filter_float <- function(tbl, float) {
  argo_assert_columns(tbl, "file")

  bad_float <- !stringr::str_detect(float, "^[0-9]+$")
  if (any(bad_float)) {
    values <- if (sum(bad_float) != 1) "values" else "value"
    are <- if (sum(bad_float) != 1) "are" else "is"
    bad_float_lab <- glue::glue_collapse(paste0("'", float, "'"), sep = ", ", last = " and ")
    abort(
      glue(
        paste0(
          "`float` must be a numeric identifier. ",
          "The following { values } { are } not valid:\n{ bad_float_lab }"
        )
      )
    )
  }

  if (length(float) == 0) {
    return(tbl[integer(0), , drop = FALSE])
  }


  file_regex <- paste0("[^0-9](", paste0(float, collapse = "|"), ")[^0-9][^/]*$")
  argo_do_filter(tbl, stringr::str_detect(tbl$file, file_regex))
}

#' @rdname argo_filter
#' @export
argo_filter_data_mode <- function(tbl, data_mode) {
  argo_assert_columns(tbl, "file")

  data_mode_choices <- c("R", "D", "r", "d", "realtime", "delayed")
  if (!isTRUE(data_mode %in% data_mode_choices)) {
    choices <- glue::glue_collapse(paste0("'", data_mode_choices, "'"), sep = ", ", last = " or ")
    abort(
      glue(
        "`data_mode` must be one of { choices }"
      )
    )
  }

  data_mode <- toupper(substr(data_mode, 1, 1))

  regex_prof <- paste0(data_mode, "[0-9]+[^/]+$")
  regex_non_prof <- paste0("[0-9]+_(B|S)?", data_mode, "(traj|prof|tech|meta)\\.nc$")

  file_regex <- paste0("(", regex_prof, ")|(", regex_non_prof, ")")
  argo_do_filter(tbl, stringr::str_detect(tbl$file, file_regex))
}

#' @rdname argo_filter
#' @export
argo_filter_direction <- function(tbl, direction) {
  argo_assert_columns(tbl, "file")

  if (!isTRUE(direction %in% c("ascending", "descending"))) {
    abort("`direction` must be one of 'ascending' or 'descending'")
  }

  is_descending <- stringr::str_detect(tbl$file, "[0-9]{3,4}D\\.nc")
  if (direction == "descending") {
    argo_do_filter(tbl, is_descending)
  } else {
    argo_do_filter(tbl, !is_descending)
  }
}

filter_latlon_radius <- function(tbl, xy, radius_km) {
  tbl_longitude <- normalize_lng(tbl$longitude)
  tbl_latitude <- normalize_lat(tbl$latitude)

  dist <- geodist_lnglat(
    tbl_longitude, tbl_latitude,
    xy$x, xy$y,
    R = 6371.01
  )

  argo_do_filter(tbl, dist <= radius_km)
}

filter_rect_radius <- function(tbl, xy, radius_km, n_detail = 100) {
  r_tbl <- list(
    xmin = normalize_lng(tbl$longitude_min), xmax = normalize_lng(tbl$longitude_max),
    ymin = normalize_lat(tbl$latitude_min), ymax = normalize_lat(tbl$latitude_max)
  )
  r_tbl_split <- rect_split_dateline((r_tbl))

  # approximate radius as a rectangle
  radius_deg <- radius_km / 6371.01 * 180 / pi
  r_query <- list(
    xmin = normalize_lng(xy$x - radius_deg),
    xmax = normalize_lng(xy$x + radius_deg),
    ymin = pmax(normalize_lat(xy$y - radius_deg), -90),
    ymax = pmin(normalize_lat(xy$y + radius_deg), 90)
  )
  r_query_split <- rect_split_dateline(r_query)

  intersected <- list(
    rect_intersection(r_tbl_split[[1]], r_query_split[[1]]),
    rect_intersection(r_tbl_split[[1]], r_query_split[[2]]),
    rect_intersection(r_tbl_split[[2]], r_query_split[[1]]),
    rect_intersection(r_tbl_split[[2]], r_query_split[[2]])
  )

  approximated <- lapply(intersected, rect_approx_points, n_detail = 10)
  approximate_which_intersects <- lapply(approximated, function(approx_xy) {
    dist <- geodist_lnglat(
      approx_xy$x, approx_xy$y,
      xy$x, xy$y,
      R = 6371.01
    )

    i <- which(dist <= radius_km)
    (i - 1) %/% 100 + 1
  })

  approximate_which_intersects_any <- unique(unlist(approximate_which_intersects))
  tbl[approximate_which_intersects_any, , drop = FALSE]
}

filter_latlon_rect <- function(tbl, r_query) {
  longitude <- normalize_lng(tbl$longitude)
  latitude <- normalize_lat(tbl$latitude)

  xy_tbl <- list(x = normalize_lng(tbl$longitude), y = normalize_lat(tbl$latitude))
  r_query_split <- rect_split_dateline(r_query)
  intersects <- rect_contains(r_query_split[[1]], xy_tbl) |
    rect_contains(r_query_split[[1]], xy_tbl)

  argo_do_filter(tbl, intersects)
}

filter_rect_rect <- function(tbl, r_query) {
  r_tbl <- list(
    xmin = normalize_lng(tbl$longitude_min), xmax = normalize_lng(tbl$longitude_max),
    ymin = normalize_lat(tbl$latitude_min), ymax = normalize_lat(tbl$latitude_max)
  )

  # normalize rectangles so that width < 180 degrees (a better assumption than
  # the alternative and often true for floats in the pacific)
  width_greater_than_180 <- (r_tbl$xmax - r_tbl$xmin) > 180
  xmin_temp <- r_tbl$xmin
  r_tbl$xmin[width_greater_than_180] <- r_tbl$xmax[width_greater_than_180]
  r_tbl$xmax[width_greater_than_180] <- xmin_temp[width_greater_than_180]

  r_query_split <- rect_split_dateline(r_query)
  r_tbl_split <- rect_split_dateline((r_tbl))
  intersects <- list(
    rect_intersects(r_query_split[[1]], r_tbl_split[[1]]),
    rect_intersects(r_query_split[[1]], r_tbl_split[[2]]),
    rect_intersects(r_query_split[[2]], r_tbl_split[[1]]),
    rect_intersects(r_query_split[[2]], r_tbl_split[[2]])
  )

  argo_do_filter(tbl, !!! intersects, .reduce = "|")
}

tbl_has_latlon_or_rect <- function(tbl) {
  if (!is.data.frame(tbl)) {
    abort("`tbl` must be a data.frame.")
  }

  latlon_cols <- c("latitude", "longitude")
  rect_cols <- c("latitude_min", "latitude_max", "longitude_min", "longitude_max")

  if (all(latlon_cols %in% names(tbl))) {
    "latlon"
  } else if(all(rect_cols %in% names(tbl))) {
    "rect"
  } else {
    latlon_lab <- glue::glue_collapse(paste0("'", latlon_cols, "'"), sep = ", ", last = " and ")
    rect_lab <- glue::glue_collapse(paste0("'", rect_cols, "'"), sep = ", ", last = " and ")

    abort(
      glue(
        paste0(
          "`tbl` must contain columns { latlon_lab }\n",
          "or columns { rect_lab } to filter by location."
        )
      )
    )
  }
}

argo_do_filter <- function(tbl, ..., .reduce = "&") {
  tbl_match <- Reduce(.reduce, rlang::list2(...))
  tbl[!is.na(tbl_match) & tbl_match, , drop = FALSE]
}
