
#' Load Argo profiles
#'
#' @inheritParams argo_vars
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()].
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
  tbl <- argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_prof,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_prof
#' @export
argo_prof_calib <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_calib,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_prof
#' @export
argo_prof_param <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_param,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_prof
#' @export
argo_prof_history <- function(path, vars = NULL, download = NULL, quiet = NA) {
  tbl <- argo_read_many(
    assert_argo_prof_file,
    argo_read_prof_history,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)
  tbl
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
    quiet = quiet
  )

  names(tbl) <- gsub("n_values[0-9]+$", "n_values", names(tbl))
  tbl
}


#' Read Argo profiles
#'
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()]
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
