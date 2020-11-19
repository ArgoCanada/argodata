
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
