
#' Read Argo profile NetCDF files
#'
#' @inheritParams argo_nc_read_vars
#' @param meta A character vector of meta variable names for
#'   each profile. These can be found using [argo_vars()].
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

  if (is.null(n) || is.null(n_prof)) {
    abort(glue("'{ nc$filename }' is missing dimension 'N_LEVELS' or 'N_PROF'"))
  }

  # assign values
  meta_values <- c(
    list(N_PROF = nc$dim$N_PROF$vals) %||% vctrs::vec_rep(NA_integer_, n_prof),
    argo_nc_values(nc, meta)
  )
  values <- c(
    list(
      N_LEVELS = vctrs::vec_rep(nc$dim$N_LEVELS$vals, n_prof) %||%
        vctrs::vec_rep(NA_integer_, n * n_prof)
    ),
    argo_nc_values(nc, vars)
  )

  # expand strings to characters
  meta_values <- argo_string_to_chars_tbl(meta_values)
  values <- argo_string_to_chars_tbl(values, n = n)

  # rep profile meta to match values
  meta_values <- lapply(meta_values, vctrs::vec_rep_each, n)

  tibble::new_tibble(c(meta_values, values), nrow = n * n_prof)
}

#' @rdname argo_nc_prof
#' @export
argo_nc_prof_read_prof <- function(nc, vars = NULL) {
  nc_vars <- argo_nc_prof_vars_prof(nc)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)
  n <- nc$dim$N_PROF$len

  if (is.null(n)) {
    abort(glue("'{ nc$filename }' is missing dimension 'N_PROF'"))
  }

  values <- c(
    list(N_PROF = nc$dim$N_PROF$vals) %||% rep(NA_integer_, n),
    argo_nc_values(nc, vars)
  )
  values <- argo_string_to_chars_tbl(values)

  tibble::new_tibble(values, nrow = n)
}

#' @rdname argo_nc_prof
#' @export
argo_nc_prof_read_calib <- function(nc, vars = NULL) {
  nc_vars <- argo_nc_prof_vars_calib(nc)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)
  n_param <- nc$dim$N_PARAM$len
  n_calib <- nc$dim$N_CALIB$len
  n_prof <- nc$dim$N_PROF$len

  if (is.null(n_param) || is.null(n_calib) || is.null(n_prof)) {
    abort(glue("'{ nc$filename }' is missing dimension 'N_PARAM', 'N_CALIB', or 'N_PROF'"))
  }

  values <- c(
    unclass(
      expand.grid(
        N_PARAM = nc$dim$N_PARAM$vals %||% rep(NA_integer_, n_param),
        N_CALIB = nc$dim$N_CALIB$vals %||% rep(NA_integer_, n_calib),
        N_PROF = nc$dim$N_PROF$vals %||% rep(NA_integer_, n_prof)
      )
    ),
    argo_nc_values(nc, vars)
  )

  tibble::new_tibble(values, nrow = n_param * n_calib * n_prof)
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

  if (is.null(n) || is.null(n_prof)) {
    abort(glue("'{ nc$filename }' is missing dimension 'N_HISTORY' or 'N_PROF'"))
  }

  # When there are zero history items, reading a variable doesn't
  # work. Ideally this would have the history columns with the
  # correct types, but ncvar_get() errors and guessing the types
  # is complicated.
  if (n == 0) {
    return(tibble::tibble())
  }

  # assign values
  meta_values <- c(
    list(N_PROF = nc$dim$N_PROF$vals) %||% rep(NA_integer_, n_prof),
    argo_nc_values(nc, meta)
  )
  values <- c(
    list(N_HISTORY = vctrs::vec_rep_each(nc$dim$N_HISTORY$vals %||% rep(NA_integer_, n), n_prof)),
    argo_nc_values(nc, intersect(vars, nc_vars_reg))
  )
  values_string <- argo_nc_values(nc, intersect(vars, nc_vars_string))

  # expand strings to characters
  meta_values <- argo_string_to_chars_tbl(meta_values)
  values <- argo_string_to_chars_tbl(values, n = n_prof)
  values_string <- lapply(values_string, stringr::str_trim)

  # rep profile meta to match values (note reversed dim order from levels)
  meta_values <- lapply(meta_values, vctrs::vec_rep, n)

  tibble::new_tibble(c(meta_values, values, values_string), nrow = n * n_prof)
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
argo_nc_prof_vars_calib <- function(nc) {
  # all calib vars have N_PARAM, N_CALIB, and N_PROF as dimensions 2:4
  Reduce(
    intersect,
    list(
      argo_nc_vars_by_dimension(nc, 2, "N_PARAM"),
      argo_nc_vars_by_dimension(nc, 3, "N_CALIB"),
      argo_nc_vars_by_dimension(nc, 4, "N_PROF")
    )
  )
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
