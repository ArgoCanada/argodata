
#' Transform Argo data to variable-long format
#'
#' Whereas the variable-wide format returned by most Argo read and load
#' functions are useful, some processing and plotting algorithms need data
#' in a variable-long form, where the variable name is assigned to a column
#' whose value is used to identify the measurement represented by each row.
#'
#' @param tbl An Argo table with paired value/qc/adjusted columns, likely
#'   from [argo_prof_levels()] or [argo_traj_measurement()].
#' @param id_cols A vector of column names used to identify each row in the
#'   output using [dplyr::select()] syntax.
#'
#' @return A [tibble::tibble()] with columns `id_cols`, `variable`, `value`,
#'   `value_qc`, `value_adjusted`, `value_adjusted_qc`, and
#'   `value_adjusted_error`.
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_prof_levels("dac/csio/2902746/profiles/BR2902746_001.nc") %>%
#'     argo_pivot_longer(id_cols = c(file, date, pres))
#' })
#'
argo_pivot_longer <- function(tbl, id_cols) {
  tbl_id <- dplyr::select(tbl, {{ id_cols }})
  n <- nrow(tbl)

  spec <- argo_pivot_longer_spec(tbl)
  n_variable <- nrow(spec)

  if (n_variable == 0) {
    return(
      vctrs::vec_cbind(
        tbl_id[integer(0), ],
        tibble::tibble(
          variable = character(),
          value = double(),
          value_qc = character(),
          value_adjusted = double(),
          value_adjusted_qc = character(),
          value_adjusted_error = double(),
        ),
        .name_repair = "check_unique"
      )
    )
  }

  # add dummy NA columns to fill in columns that don't exist but were generated
  # by the spec
  tbl["__na_dbl__"] <- NA_real_
  tbl["__na_chr__"] <- NA_character_
  spec$value[is.na(spec$value)] <- "__na_dbl__"
  spec$value_qc[is.na(spec$value_qc)] <- "__na_chr__"
  spec$value_adjusted[is.na(spec$value_adjusted)] <- "__na_dbl__"
  spec$value_adjusted_qc[is.na(spec$value_adjusted_qc)] <- "__na_chr__"
  spec$value_adjusted_error[is.na(spec$value_adjusted_error)] <- "__na_dbl__"

  spec_values <- lapply(spec[-1], function(x) lapply(x, function(col) tbl[[col]]))
  values <- tibble::tibble(
    variable = vctrs::vec_rep_each(spec$.variable, nrow(tbl)),
    !!! lapply(spec_values, unlist)
  )

  vctrs::vec_cbind(
    vctrs::vec_rep(tbl_id, n_variable),
    values,
    .name_repair = "check_unique"
  )
}

argo_pivot_longer_spec <- function(tbl) {
  cols <- names(tbl)

  adjusted <- stringr::str_match(cols, "^(.*?)_(ADJUSTED|adjusted)$")
  adjusted_qc <- stringr::str_match(cols, "^(.*?)_(ADJUSTED_QC|adjusted_qc)$")
  adjusted_error <- stringr::str_match(cols, "^(.*?)_(ADJUSTED_ERROR|adjusted_error)$")
  qc <- stringr::str_match(cols, "^(.*?)_(QC|qc)$")

  # extract list of variables based on paired suffix values
  variable <- c(adjusted[, 2], adjusted_qc[, 2], adjusted_error[, 2], qc[, 2])
  variable <- setdiff(variable, c(NA_character_, adjusted[, 1]))

  # build spec
  tibble::tibble(
    .variable = variable,
    value = ifelse(variable %in% cols, variable, NA_character_),
    value_qc = qc[match(variable, qc[, 2]), 1],
    value_adjusted = adjusted[match(variable, adjusted[, 2]), 1],
    value_adjusted_qc = adjusted_qc[match(variable, adjusted_qc[, 2]), 1],
    value_adjusted_error = adjusted_error[match(variable, adjusted_error[, 2]), 1]
  )
}
