
argo_assert_columns <- function(tbl, columns) {
  if (!is.data.frame(tbl)) {
    abort("`tbl` must be a data.frame.")
  }

  has_col <- columns %in% names(tbl)
  if (all(!has_col)) {
    columns_req <- if (length(columns) != 1) "columns" else "column"
    required_cols <- glue::glue_collapse(paste0("'", columns, "'"), sep = ", ", last = " and ")

    abort(glue("`tbl` must have { columns_req } { required_cols }"))
  } else if (any(!has_col)) {
    columns_req <- if (length(columns) != 1) "columns" else "column"
    required_cols <- glue::glue_collapse(paste0("'", columns, "'"), sep = ", ", last = " and ")

    columns_mis <- if (sum(!has_col) != 1) "columns" else "column"
    missing_cols <- glue::glue_collapse(paste0("'", columns[!has_col], "'"), sep = ", ", last = " and ")

    abort(
      glue(
        "`tbl` must have { columns_req } { required_cols }.\n`tbl` is missing { columns_mis } { missing_cols }"
      )
    )
  }

  invisible(tbl)
}

insert_vector <- function(x, y, pos) {
  c(x[seq_len(pos - 1)], y, x[pos - 1 + seq_len(length(x) - pos + 1)])
}

vec_sanitize <- function(x, ptype, size = NULL) {
  x_quo <- rlang::enquo(x)
  out <- tryCatch(
    vctrs::vec_cast(x, ptype),
    vctrs_error_incompatible_type = function(e) {
      abort(glue("Can't convert `{ rlang::as_label(x_quo) }` to <{ class(ptype)[1] }>"))
    }
  )

  if (!is.null(size)) {
    out <- vctrs::vec_assert(out, ptype, size = size, arg = rlang::as_label(x_quo))
  }

  out
}

# earth mean radius according to s2::s2_earth_radius_meters()
geodist_lnglat <-  function(x1, y1, x2, y2, R = 6371010) {
  geodist_rad(
    x1 * pi / 180, y1 * pi / 180,
    x2 * pi / 180, y2 * pi / 180,
    R = R
  )
}

geodist_rad <- function(long1, lat1, long2, lat2, R = 6371010) {
  delta_long <- long2 - long1
  delta_lat <- lat2 - lat1
  a <- sin(delta_lat / 2) ^ 2 + cos(lat1) * cos(lat2) * sin(delta_long / 2) ^ 2
  c <- 2 * asin(pmin(1, sqrt(a)))
  R * c
}

rect_intersection <- function(r1, r2) {
  limits <- list(
    xmin = pmax(r1$xmin, r2$xmin),
    xmax = pmin(r1$xmax, r2$xmax),
    ymin = pmax(r1$ymin, r2$ymin),
    ymax = pmin(r1$ymax, r2$ymax)
  )

  any_na <- Reduce("|", lapply(limits, is.na))
  not_valid <- any_na | !((limits$xmax >= limits$xmin) & (limits$ymax >= limits$ymin))
  limits$xmin[not_valid] <- NA_real_
  limits$xmax[not_valid] <- NA_real_
  limits$ymin[not_valid] <- NA_real_
  limits$ymax[not_valid] <- NA_real_

  limits
}

rect_intersects <- function(r1, r2) {
  limits <- list(
    xmin = pmax(r1$xmin, r2$xmin),
    xmax = pmin(r1$xmax, r2$xmax),
    ymin = pmax(r1$ymin, r2$ymin),
    ymax = pmin(r1$ymax, r2$ymax)
  )

  (limits$xmax >= limits$xmin) & (limits$ymax >= limits$ymin)
}

rect_split_dateline <- function(r) {
  # create two copies that both satisfy xmin <= xmax
  is_wrap <- r$xmax < r$xmin
  xmin1 <- ifelse(is_wrap, -180, r$xmin)
  xmin2 <- r$xmin
  xmax1 <- r$xmax
  xmax2 <- ifelse(is_wrap, 180, r$xmax)

  list(
    list(
      xmin = xmin1, xmax = xmax1,
      ymin = r$ymin, ymax = r$ymax
    ),
    list(
      xmin = xmin2, xmax = xmax2,
      ymin = r$ymin, ymax = r$ymax
    )
  )
}

normalize_lng <- function(longitude) {
  # -999.999 is occasionally used to denote missing in the profile index
  # some longitudes are greater than 180, probably so that Cartesian logic
  # can be used to query them
  longitude <- vec_sanitize({{ longitude }}, double())
  longitude[longitude == -999.999] <- NA_real_
  normalized <- ((longitude + 180) %% 360) - 180
  normalized[longitude == 180] <- 180
  normalized
}

normalize_lat <- function(latitude) {
  latitude <- vec_sanitize({{ latitude }}, double())
  latitude[latitude == -99.999] <- NA_real_
  latitude
}
