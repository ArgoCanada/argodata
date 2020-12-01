
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
  names(cached) <- stringr::str_remove(path, "^dac/")

  tbls <- argo_map(cached, argo_read_vars, vars = argo_unsanitize_vars(vars))
  tbl <- vctrs::vec_rbind(!!! tbls, .names_to = "file")
  tbl$name <- tolower(tbl$name)
  tbl$name <- stringr::str_replace(tbl$name, "^juld", "date")
  names(tbl) <- tolower(names(tbl))
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
  ncdf4_vars <- lapply(unclass(nc$var[vars]), argo_nc_var_as_tbl)
  attrs <- lapply(unclass(nc$var[vars]), argo_nc_var_attr_as_tbl, nc)

  tbl <- vctrs::vec_rbind(!!! ncdf4_vars)
  tbl_attrs <- vctrs::vec_rbind(!!! attrs)

  # return an edited version of the raw values:
  # only include a few items, replace dim with the dimension names,
  # and trim whitespace
  tbl <- tbl[c("name", "longname", "units", "prec", "ndims", "size", "dim")]
  tbl$dim <- lapply(tbl$dim, function(x) vapply(x, "[[", "name", FUN.VALUE = character(1)))
  tbl_is_char <- vapply(tbl, is.character, logical(1))
  tbl[tbl_is_char] <- lapply(tbl[tbl_is_char], stringr::str_trim)

  vctrs::vec_cbind(tbl, tbl_attrs)
}

argo_nc_var_as_tbl <- function(var) {
  var$id <- NULL

  list_cols <- c("size", "dimids", "dim", "varsize", "dims", "missval")
  var_is_scalar <- !(names(var) %in% list_cols)
  var[!var_is_scalar] <- lapply(var[!var_is_scalar], list)

  tibble::new_tibble(lapply(var, unclass), nrow = 1)
}

argo_nc_var_attr_as_tbl <- function(var, nc) {
  attrs <- ncdf4::ncatt_get(nc, var)
  # this doesn't work with variables of multiple types and is covered by
  # missval in the output
  attrs[["_FillValue"]] <- NULL
  names(attrs) <- stringr::str_c("att_", names(attrs))
  tibble::new_tibble(attrs, nrow = 1)
}
