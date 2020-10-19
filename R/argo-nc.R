
#' Read and inspect Argo Profile NetCDF files
#'
#' @param nc A handle created using [ncdf4::nc_open()].
#' @param vars A character vector of variable names. These
#'   values can be found for each profile using
#'   [argo_nc_prof_list_vars()].
#'   If a variable does not exist in `nc`, a column of `NA`s
#'   is filled in its place.
#' @param meta A character vector of meta variable names for
#'   each profile. These can be found using [argo_nc_prof_list_meta()].
#' @param fill A `list()` of missing values that should be applied
#'   when `vars` contains values that are not in [argo_nc_prof_list_vars()].
#'
#' @return A [tibble::tibble()] containing both `vars` and `meta`
#'   columns.
#' @export
#'
argo_nc_prof_read <- function(nc, vars = NULL, meta = NULL, fill = list()) {
  vars <- vars %||% argo_nc_prof_list_vars(nc)
  meta <- meta %||% argo_nc_prof_list_meta(nc)

  n <- nc$dim$N_LEVELS$len
  n_prof <- nc$dim$N_PROF$len

  # create values and meta template
  meta_values <- rep(list(rep_len(NA, n_prof)), length(meta))
  names(meta_values) <- meta
  meta_types <- vapply(nc$var[meta], function(var) var$prec %||% "missing", character(1))

  values <- rep(list(rep_len(NA, n * n_prof)), length(vars))
  names(values) <- vars
  types <- vapply(nc$var[vars], function(var) var$prec %||% "missing", character(1))

  # fill values that are in `nc`
  existing_meta <- intersect(meta, argo_nc_prof_list_meta(nc))
  meta_values[existing_meta] <- lapply(
    existing_meta,
    function(x) ncdf4::ncvar_get(nc, x)
  )

  existing_vars <- intersect(vars, argo_nc_prof_list_vars(nc))
  values[existing_vars] <- lapply(
    existing_vars,
    function(x) ncdf4::ncvar_get(nc, x)
  )

  # fill values that aren't in `nc` but are in `fill`
  fill_vars <- intersect(names(fill), setdiff(vars, existing_vars))
  values[fill_vars] <- lapply(fill[fill_vars], rep_len, n * n_prof)

  fill_meta <- intersect(names(fill), setdiff(meta, existing_meta))
  meta_values[fill_meta] <- lapply(meta_values[fill_meta], rep_len, n_prof)

  # char types are in the form character(), but should be
  # character(n * n_prof)
  values[types == "char"] <- lapply(
    values[types == "char"],
    function(x) rawToChar(charToRaw(x), multiple = TRUE)
  )

  # char types are in the form character(), but should be
  # character(n_prof)
  meta_values[meta_types == "char"] <- lapply(
    meta_values[meta_types == "char"],
    function(x) rawToChar(charToRaw(x), multiple = TRUE)
  )

  # rep profile meta to match values
  meta_values <- lapply(meta_values, rep, each = n)


  # extract float info from filename if possible
  float_extract <- stringr::str_remove(
    stringr::str_extract(nc$filename, "dac/[a-z]+/[A-Za-z0-9]+"),
    "^dac/"
  )
  float <- list(float = rep(float_extract, n * n_prof))

  # remove the 'dim' attribute from values
  cols <- lapply(c(float, meta_values, values), "dim<-", NULL)
  tibble::new_tibble(cols, nrow = n * n_prof)
}

#' @rdname argo_nc_prof_read
#' @export
argo_nc_prof_list_vars <- function(nc) {
  var_has_n_levels <- vapply(nc$var, function(x) x$dim[[1]]$name == "N_LEVELS", logical(1))
  names(nc$var)[var_has_n_levels]
}

#' @rdname argo_nc_prof_read
#' @export
argo_nc_prof_list_meta <- function(nc) {
  var_has_n_prof <- vapply(nc$var, function(x) {
    if (length(x$dim) != 1) return(FALSE)
    x$dim[[1]]$name == "N_PROF"
  }, logical(1))

  names(nc$var)[var_has_n_prof]
}
