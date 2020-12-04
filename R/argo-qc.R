
#' Apply QC flags to Argo data
#'
#' @param tbl A data frame containing `_qc` or `_QC` columns.
#' @param cols Columns in `tbl`, specified using [dplyr::select()] syntax.
#' @param qc_flag One or more quality control flags. See
#'   `argo_reference_qc_flag` for long-form descriptions of integer `qc_flag`
#'   values.
#' @param qc_cols A vector of columns that contain the quality control flag
#'   values found in columns.
#'
#' @return A modified `tbl`.
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_prof_levels("dac/csio/2902746/profiles/BR2902746_001.nc") %>%
#'     argo_qc_censor_if_not(doxy, qc_flag = c(1, 2, 8))
#' })
#'
argo_qc_censor_if_not <- function(tbl, cols, qc_flag,
                                  qc_cols = argo_qc_cols(tbl, {{ cols }})) {
  tbl_selected <- dplyr::select(tbl, {{ cols }})
  if (ncol(tbl_selected) == 0) {
    return(tbl)
  }

  tbl_qc <- dplyr::select(tbl, !! qc_cols)
  filters <- lapply(tbl_qc, "%in%", qc_flag)
  filters <- lapply(filters, "!")
  na_values <- lapply(tbl_selected, "[", NA_integer_)
  tbl_censored <- Map("[<-", tbl_selected, filters, na_values)
  tbl[colnames(tbl_selected)] <- tbl_censored
  tbl
}

#' @rdname argo_qc_censor_if_not
#' @export
argo_qc_censor_if <- function(tbl, cols, qc_flag,
                              qc_cols = argo_qc_cols(tbl, {{ cols }})) {
  tbl_selected <- dplyr::select(tbl, {{ cols }})
  if (ncol(tbl_selected) == 0) {
    return(tbl)
  }

  tbl_qc <- dplyr::select(tbl, !! qc_cols)
  filters <- lapply(tbl_qc, "%in%", qc_flag)
  na_values <- lapply(tbl_selected, "[", NA_integer_)
  tbl_censored <- Map("[<-", tbl_selected, filters, na_values)
  tbl[colnames(tbl_selected)] <- tbl_censored
  tbl
}

#' @rdname argo_qc_censor_if_not
#' @export
argo_qc_keep_if_all  <- function(tbl, cols, qc_flag,
                                 qc_cols = argo_qc_cols(tbl, {{ cols }})) {
  tbl_selected <- dplyr::select(tbl, {{ cols }})
  if (ncol(tbl_selected) == 0) {
    return(tbl)
  }

  tbl_qc <- dplyr::select(tbl, !! qc_cols)
  filters <- lapply(tbl_qc, "%in%", qc_flag)
  tbl[Reduce("&", filters), , drop = FALSE]
}

#' @rdname argo_qc_censor_if_not
#' @export
argo_qc_keep_if_any  <- function(tbl, cols, qc_flag,
                                 qc_cols = argo_qc_cols(tbl, {{ cols }})) {
  tbl_selected <- dplyr::select(tbl, {{ cols }})
  if (ncol(tbl_selected) == 0) {
    return(tbl)
  }

  tbl_qc <- dplyr::select(tbl, !! qc_cols)
  filters <- lapply(tbl_qc, "%in%", qc_flag)
  tbl[Reduce("|", filters), , drop = FALSE]
}

#' @rdname argo_qc_censor_if_not
#' @export
argo_qc_discard_if_all  <- function(tbl, cols, qc_flag,
                                    qc_cols = argo_qc_cols(tbl, {{ cols }})) {
  tbl_selected <- dplyr::select(tbl, {{ cols }})
  if (ncol(tbl_selected) == 0) {
    return(tbl)
  }

  tbl_qc <- dplyr::select(tbl, !! qc_cols)
  filters <- lapply(tbl_qc, "%in%", qc_flag)
  tbl[!Reduce("&", filters), , drop = FALSE]
}

#' @rdname argo_qc_censor_if_not
#' @export
argo_qc_discard_if_any  <- function(tbl, cols, qc_flag,
                                    qc_cols = argo_qc_cols(tbl, {{ cols }})) {
  tbl_selected <- dplyr::select(tbl, {{ cols }})
  if (ncol(tbl_selected) == 0) {
    return(tbl)
  }

  tbl_qc <- dplyr::select(tbl, !! qc_cols)
  filters <- lapply(tbl_qc, "%in%", qc_flag)
  tbl[!Reduce("|", filters), , drop = FALSE]
}

#' @rdname argo_qc_censor_if_not
#' @export
argo_qc_cols <- function(tbl, cols) {
  tbl_selected <- dplyr::select(tbl, {{ cols }})
  cols_selected <- colnames(tbl_selected)

  # have to gamble that *_QC and *_qc* aren't present in the same table
  cols_qc <- paste0(cols_selected, "_qc")
  cols_QC <- paste0(cols_selected, "_QC")

  matched <- ifelse(cols_qc %in% colnames(tbl), cols_qc, cols_QC)
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
          "All columns specified in `cols` must have a paired `_qc` or `_QC` column.\n",
          "Problematic { columns }: { bad_cols }"
        )
      )
    )
  }

  matched
}
