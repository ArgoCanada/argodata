
#' Read Argo profile NetCDF files
#'
#' @param nc A handle created using [ncdf4::nc_open()].
#' @param vars A character vector of variable names.
#'   If a variable does not exist in the file, it is not included
#'   in the output. The order of `vars` in the output is not
#'   guaranteed.
#' @param meta A character vector of meta variable names for
#'   each profile. These can be found using [argo_nc_prof_vars_prof()].
#'
#' @return A [tibble::tibble()] containing both `vars` and `meta`
#'   columns.
#' @export
#' @rdname argo_nc_prof
#'
#' @examples
#' nc_prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#' nc_prof <- ncdf4::nc_open(nc_prof_file)
#'
#' argo_nc_prof_vars_levels(nc_prof)
#' argo_nc_prof_vars_prof(nc_prof)
#' argo_nc_prof_vars_history(nc_prof)
#'
#' argo_nc_prof_read_levels(nc_prof)
#' argo_nc_prof_read_prof(nc_prof)
#' argo_nc_prof_read_history(nc_prof)
#'
#' ncdf4::nc_close(nc_prof)
#'
argo_nc_prof_read_levels <- function(nc, vars = NULL, meta = NULL) {
  nc_meta <- argo_nc_prof_vars_prof(nc)
  nc_vars <- argo_nc_prof_vars_levels(nc)

  meta <- if (is.null(meta)) nc_meta else intersect(meta, nc_meta)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)

  n <- nc$dim$N_LEVELS$len
  n_prof <- nc$dim$N_PROF$len

  # assign values
  meta_values <- argo_nc_values(nc, meta)
  values <- argo_nc_values(nc, vars)

  # expand strings to characters
  meta_values <- argo_string_to_chars_tbl(meta_values)
  values <- argo_string_to_chars_tbl(values, n = n)

  # rep profile meta to match values
  meta_values <- lapply(meta_values, vctrs::vec_rep_each, n)

  argo_nc_new_tibble(nc, meta_values, values, nrow = n * n_prof)
}

#' @rdname argo_nc_prof
#' @export
argo_nc_prof_read_prof <- function(nc, vars = NULL) {
  nc_vars <- argo_nc_prof_vars_prof(nc)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)
  n <- nc$dim$N_PROF$len

  values <- argo_nc_values(nc, vars)
  values <- argo_string_to_chars_tbl(values)

  argo_nc_new_tibble(nc, values, nrow = n)
}

#' @rdname argo_nc_prof
#' @export
argo_nc_prof_read_history <- function(nc, vars = NULL, meta = NULL) {
  nc_meta <- argo_nc_prof_vars_prof(nc)
  nc_vars_reg <- argo_nc_vars_by_dimension(nc, 2, "N_HISTORY")
  nc_vars_string <- argo_nc_vars_by_dimension(nc, 3, "N_HISTORY")
  nc_vars <- c(nc_vars_reg, nc_vars_string)

  meta <- if (is.null(meta)) nc_meta else intersect(meta, nc_meta)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)

  n <- nc$dim$N_HISTORY$len
  n_prof <- nc$dim$N_PROF$len

  # When there are zero history items, reading a variable doesn't
  # work. Ideally this would have the history columns with the
  # correct types, but ncvar_get() errors and guessing the types
  # is complicated.
  if (n == 0) {
    return(tibble::tibble(float = character(0)))
  }

  # assign values
  meta_values <- argo_nc_values(nc, meta)
  values <- argo_nc_values(nc, intersect(vars, nc_vars_reg))
  values_string <- argo_nc_values(nc, intersect(vars, nc_vars_string))

  # expand strings to characters
  meta_values <- argo_string_to_chars_tbl(meta_values)
  values <- argo_string_to_chars_tbl(values, n = n_prof)
  values_string <- lapply(values_string, stringr::str_trim)

  # rep profile meta to match values (note reversed dim order from levels)
  meta_values <- lapply(meta_values, vctrs::vec_rep, n)

  argo_nc_new_tibble(nc, meta_values, values, values_string, nrow = n * n_prof)
}

#' @rdname argo_nc_prof
#' @export
argo_nc_prof_vars_levels <- function(nc) {
  argo_nc_vars_by_dimension(nc, 1, "N_LEVELS")
}

#' @rdname argo_nc_prof
#' @export
argo_nc_prof_vars_prof <- function(nc) {
  var_has_n_prof <- vapply(nc$var, function(x) {
    if (length(x$dim) != 1) return(FALSE)
    x$dim[[1]]$name == "N_PROF"
  }, logical(1))

  names(nc$var)[var_has_n_prof]
}

#' @rdname argo_nc_prof
#' @export
argo_nc_prof_vars_history <- function(nc) {
  # all history vars have N_PROF as first dimension
  c(
    argo_nc_vars_by_dimension(nc, 2, "N_HISTORY"),
    # for these variables, the first dimension is a string dimension
    argo_nc_vars_by_dimension(nc, 3, "N_HISTORY")
  )
}
