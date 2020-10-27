
#' Read Argo NetCDF variable metadata
#'
#' @inheritParams argo_download
#' @param nc A handle created using [ncdf4::nc_open()].
#' @param vars A vector of variable names to include. The ordering
#'   of the variables is not guaranteed, and variables that do not
#'   exist are ignored. For `nc_read_*()` and `read_*()` variants,
#'   these are the raw variable names (all caps, without "date"
#'   substituted for "juld").
#' @param file A previously downloaded Argo NetCDF file.
#'
#' @return A [tibble::tibble()] with one row per variable and columns `name`,
#'   `longname`, `units`, `prec`, `ndims`, `size`, and `dim`. Columns
#'   `size` and `dim` can be unnested (e.g., using [tidyr::unnest()])
#'   to produce a tibble with one row per variable per dimension.
#' @export
#'
#' @examples
#' with_argo_example_cache({
#'   argo_vars("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
#' prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_vars(prof_file)
#'
#' nc_prof <- ncdf4::nc_open(prof_file)
#' argo_nc_read_vars(nc_prof)
#' ncdf4::nc_close(nc_prof)
#'
argo_vars <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  path <- as_argo_path(path)
  cached <- argo_download(path, download = download, quiet = quiet)
  tbls <- lapply(cached, argo_read_vars, vars = argo_unsanitize_vars(vars))
  tbl <- vctrs::vec_rbind(!!! tbls)
  tbl$name <- tolower(tbl$name)
  tbl$name <- stringr::str_replace(tbl$name, "^juld", "date")
  tbl
}

#' @rdname argo_vars
#' @export
argo_read_vars <- function(file, vars = NULL) {
  with_argo_nc_file(file, argo_nc_read_vars, vars = vars)
}

#' @rdname argo_vars
#' @export
argo_nc_read_vars <- function(nc, vars = NULL) {
  vars <- if (is.null(vars)) names(nc$var) else intersect(vars, names(nc$var))
  vars <- lapply(unclass(nc$var[vars]), argo_nc_var_as_tbl)

  tbl <- vctrs::vec_rbind(!!! vars)
  tbl$float <- argo_nc_extract_float(nc)

  # return an edited version of the raw values:
  # only include a few items, replace dim with the dimension names,
  # and trim whitespace
  tbl <- tbl[c("float", "name", "longname", "units", "prec", "ndims", "size", "dim")]
  tbl$dim <- lapply(tbl$dim, function(x) vapply(x, "[[", "name", FUN.VALUE = character(1)))
  tbl_is_char <- vapply(tbl, is.character, logical(1))
  tbl[tbl_is_char] <- lapply(tbl[tbl_is_char], stringr::str_trim)
  tbl
}

argo_nc_var_as_tbl <- function(var) {
  var$id <- NULL
  list_cols <- c("size", "dimids", "dim", "varsize", "dims", "missval")
  var_is_scalar <- !(names(var) %in% list_cols)

  var[!var_is_scalar] <- lapply(var[!var_is_scalar], list)
  tibble::new_tibble(lapply(var, unclass), nrow = 1)
}
