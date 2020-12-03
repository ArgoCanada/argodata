
#' Unnest Argo QC tests
#'
#' The `history_qctest` column in [argo_prof_history()] and
#' [argo_traj_history()] is a binary flags column, allowing a single integer
#' value to represent up to 32 logical values. This function expands its
#' input such that there is one test represented by each row and can be
#' joined to [argo_reference_history_qctest][argo_reference_history_qctest]
#' on the `history_qctest` column.
#'
#' @param tbl A data frame with a `history_qctest` column.
#'
#' @return `tbl`
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_prof_history(
#'     "dac/csio/2902746/profiles/BR2902746_001.nc",
#'     vars = "history_qctest"
#'   ) %>%
#'     argo_unnest_history_qctest() %>%
#'     left_join(argo_reference_history_qctest, by = "history_qctest") %>%
#'     select(history_qctest_description, everything())
#' })
#'
argo_unnest_history_qctest <- function(tbl) {
  argo_assert_columns(tbl, "history_qctest")

  tbl$history_qctest <- lapply(
    as.integer(paste0("0x", tbl[["history_qctest"]])),
    unpack_flags,
    flags = argodata::argo_reference_history_qctest$history_qctest_flag,
    values = argodata::argo_reference_history_qctest$history_qctest
  )

  tidyr::unnest(tbl, .data$history_qctest)
}

unpack_flags <- function(flag_union,
                         flags = argodata::argo_reference_qctest$history_qctest_flag,
                         values = argodata::argo_reference_qctest$history_qctest) {
  values[bitwAnd(flag_union, flags) != 0]
}
