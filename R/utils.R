
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
