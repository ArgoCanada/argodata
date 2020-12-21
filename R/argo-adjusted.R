
#' Prefer adjusted values
#'
#' Many Argo tables have `_adjusted` columns that contain a more reliable value.
#' However, in many cases, these values are blank or not present. Use
#' `argo_use_adjusted()` to replace `NA` values in `cols` with the value in
#' the paired `_adjusted` column. You can use this with
#' [argo_qc_censor_if_not()] to set values in `cols` to `NA` based on the paired
#' `_qc` column prior to preferring the adjusted value.
#'
#' @param tbl tbl A data frame containing `_adjusted` or `_ADJUSTED` columns.
#' @inheritParams argo_qc_censor_if_not
#' @param adjusted_cols A paired vector of columns to `cols` containing the
#'   adjusted value to prefer over the value in `cols`.
#'
#' @return A [tibble::tibble()] with
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_prof_levels("dac/csio/2902746/profiles/BR2902746_001.nc") %>%
#'     argo_use_adjusted(doxy)
#' })
#'
argo_use_adjusted <- function(tbl, cols, adjusted_cols = argo_adjusted_cols(tbl, {{ cols }})) {
  tbl_selected <- dplyr::select(tbl, {{ cols }})
  if (ncol(tbl_selected) == 0) {
    return(tbl)
  }

  tbl_adjusted <- dplyr::select(tbl, !! adjusted_cols)
  tbl[names(tbl_selected)] <- Map(dplyr::coalesce, tbl_adjusted, tbl_selected)

  tbl
}

#' @rdname argo_use_adjusted
#' @export
argo_adjusted_cols <- function(tbl, cols) {
  tbl_selected <- dplyr::select(tbl, {{ cols }})
  cols_selected <- colnames(tbl_selected)

  # have to gamble that *_ADJUSTED and *_adjusted* aren't present in the same table
  cols_adjusted <- paste0(cols_selected, "_adjusted")
  cols_ADJUSTED <- paste0(cols_selected, "_ADJUSTED")

  matched <- ifelse(cols_adjusted %in% colnames(tbl), cols_adjusted, cols_ADJUSTED)
  matched[!(matched %in% colnames(tbl))] <- NA_character_

  if (any(is.na(matched))) {
    columns <- if (sum(is.na(matched)) != 1) "columns" else "column"
    bad_cols <- glue::glue_collapse(
      paste0("'", cols_selected[is.na(matched)], "'"),
      sep = ", ",
      last = " and "
    )

    abort(
      glue(
        paste0(
          "All columns specified in `cols` must have a paired `_adjusted` or `_ADJUSTED` column.\n",
          "Problematic { columns }: { bad_cols }"
        )
      )
    )
  }

  matched
}
