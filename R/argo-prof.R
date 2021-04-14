
#' Load Argo profiles
#'
#' Use `argo_prof_*()` functions to extract information from Argo profile
#' NetCDF files. Use [`argo_read_prof_*()`][argo_read_prof_levels]
#' to extract information from a single previously-downloaded NetCDF file.
#'
#' @inheritParams argo_vars
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()] with
#'   - `argo_prof_levels()`: one row per file per profile per sampling level.
#'   - `argo_prof_prof()`: one row per file per profile.
#'   - `argo_prof_calib()`: one row per file per profile per calibration per
#'      parameter.
#'   - `argo_prof_param()`: one row per file per profile per parameter.
#'   - `argo_prof_history()`: one row per file per profile per history entry.
#'   - `argo_prof_spectra()`: one row per file per profile per sampling level
#'     per spectra value.
#'
#' @export
#' @rdname argo_prof
#'
#' @examples
#' with_argo_example_cache({
#'   argo_prof_levels("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_prof_prof("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_prof_calib("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_prof_param("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_prof_history("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_prof_spectra("dac/aoml/5906206/profiles/BD5906206_016.nc")
#' })
#'
argo_prof_levels <- function(path, vars = NULL, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_levels,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_prof
#' @export
argo_prof_prof <- function(path, vars = NULL, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_prof,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_prof
#' @export
argo_prof_calib <- function(path, vars = NULL, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_calib,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_prof
#' @export
argo_prof_param <- function(path, vars = NULL, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_param,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_prof
#' @export
argo_prof_history <- function(path, vars = NULL, download = NULL, quiet = NA) {
  argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_history,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet,
    trim = TRUE
  )
}

#' @rdname argo_prof
#' @export
argo_prof_spectra <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_spectra,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet,
    trim = TRUE
  )

  names(tbl) <- stringr::str_replace(names(tbl), "n_values[0-9]+", "n_values")
  tbl
}


#' Read Argo profiles
#'
#' Use `argo_read_prof_*()` functions to extract profile information from a
#' previously-downloaded Argo NetCDF file.
#'
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()] with
#'   - `argo_read_prof_levels()`: one row per profile per sampling level.
#'   - `argo_read_prof_prof()`: one row per profile.
#'   - `argo_read_prof_calib()`: one row per profile per calibration per parameter.
#'   - `argo_read_prof_param()`: one row per profile per parameter.
#'   - `argo_read_prof_history()`: one row per profile per history entry.
#'   - `argo_read_prof_spectra()`: one row per profile per sampling level per
#'     spectra value.
#'
#' @export
#' @rdname argo_read_prof
#'
#' @examples
#' prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_prof_levels(prof_file)
#' argo_read_prof_prof(prof_file)
#' argo_read_prof_calib(prof_file)
#' argo_read_prof_param(prof_file)
#' argo_read_prof_history(prof_file)
#'
#' bgc_file <- system.file(
#'   "cache-test/dac/aoml/5906206/profiles/BD5906206_016.nc",
#'   package = "argodata"
#' )
#' argo_read_prof_spectra(bgc_file)
#'
argo_read_prof_levels <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = c("N_LEVELS", "N_PROF"),
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_prof <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = "N_PROF",
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_calib <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = c("N_PARAM", "N_CALIB", "N_PROF"),
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_param <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file, dims = c("N_PARAM", "N_PROF"),
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_history <- function(file, vars = NULL, quiet = FALSE) {
  argo_nc_read_simple(
    file,
    dims = c("N_PROF", "N_HISTORY"),
    vars = vars,
    quiet = quiet
  )
}

#' @rdname argo_read_prof
#' @export
argo_read_prof_spectra <- function(file, vars = NULL, quiet = FALSE) {
  vars_tbl <- argo_read_vars(file)
  is_spectra <- vapply(
    vars_tbl$dim, function(x) {
      identical(x[-1], c("N_LEVELS", "N_PROF")) &&
        isTRUE(startsWith(x[1], "N_VALUES"))
    },
    logical(1)
  )
  vars_spectra <- vars_tbl$name[is_spectra]

  if (!any(is_spectra)) {
    warn_or_stop_read_error(
      glue("File '{ file }' contains zero variables along an N_VALUESXX dimension"),
      quiet = quiet
    )
    return(NULL)
  } else if (is.null(vars) && (sum(is_spectra) > 1)) {
    # I don't think this is the case in any files yet
    # nocov start
    spectra_vars <- paste0("'", vars_spectra, "'", collapse = ", ")
    warn_or_stop_read_error(
      glue(
        "File '{ file }' contains more than one variable along an N_VALUESXX dimension:\n{ spectra_vars }"
      ),
      quiet = quiet
    )
    return(NULL)
    # nocov end
  } else if (!is.null(vars) && (length(vars) != 1)) {
    warn_or_stop_read_error(
      "`vars` must be NULL or a character vector of length 1",
      quiet = quiet
    )
    return(NULL)
  } else if (!is.null(vars) && !isTRUE(vars %in% vars_spectra)) {
    warn_or_stop_read_error(
      glue(
        "File '{ file }' is missing spectra variable '{ vars }'"
      ),
      quiet = quiet
    )
    return(NULL)
  }

  if (is.null(vars)) {
    vars <- vars_spectra
  }

  vars_tbl <- vars_tbl[vars_tbl$name %in% vars, ]

  nc <- nc_open(file)
  on.exit(nc_close(nc))

  values <- nc_read_vars(
    nc,
    list(var_id = vars, var_name = vars, var_string = FALSE)
  )

  dim_values <- nc_read_dims(
    nc,
    list(dim_length = vars_tbl$size[[1]], dim_name = vars_tbl$dim[[1]])
  )

  new_tibble(c(dim_values, values), nrow = prod(vars_tbl$size[[1]]))
}

assert_argo_prof_file <- function(path) {
  argo_assert_path_type(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/((profiles/(B|S)?(R|D)\\1_[0-9]+D?\\.nc)|(\\1_[A-Z]*prof\\.nc))$",
    "profile"
  )
}
