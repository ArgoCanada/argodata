
#' Read Argo profile NetCDF files
#'
#' @param nc A handle created using [ncdf4::nc_open()].
#' @param vars A character vector of variable names.
#'   If a variable does not exist in the file, it is not included
#'   in the output.
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
#' argo_nc_prof_read_levels(nc_prof)
#'
#' ncdf4::nc_close(nc_prof)
#'
argo_nc_prof_read_levels <- function(nc, vars = NULL, meta = NULL) {
  # only include variables that exist in the file
  # this is because not all profile files have all variables, so it makes it
  # possible to read in a list of files without knowing the variable names
  # ahead of time
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
