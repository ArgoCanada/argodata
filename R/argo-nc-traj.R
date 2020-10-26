
#' Read Argo trajectory NetCDF files
#'
#' @inheritParams argo_nc_prof_read
#' @param vars A character vector of variable names. These
#'   values can be found for each file using
#'   [argo_nc_traj_list_vars()].
#'   If a variable does not exist in `nc`, a column of `NA`s
#'   is filled in its place.
#'
#' @return A [tibble::tibble()] containing columns "float" and
#'   all columns listed in `vars`.
#' @export
#'
#' @examples
#' nc_traj_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
#'   package = "argodata"
#' )
#' nc_traj <- ncdf4::nc_open(nc_traj_file)
#'
#' argo_nc_traj_list_vars(nc_traj)
#' argo_nc_traj_read(nc_traj)
#'
argo_nc_traj_read <- function(nc, vars = NULL, fill = list()) {
  vars <- vars %||% argo_nc_traj_list_vars(nc)
  n <- nc$dim$N_MEASUREMENT$len

  values <- rep(list(rep_len(NA, n)), length(vars))
  names(values) <- vars
  types <- vapply(nc$var[vars], function(var) var$prec %||% "missing", character(1))

  existing_vars <- intersect(vars, argo_nc_traj_list_vars(nc))
  values[existing_vars] <- lapply(
    existing_vars,
    function(x) ncdf4::ncvar_get(nc, x)
  )

  # char types are in the form character(), but should be character(n)
  values[types == "char"] <- lapply(
    values[types == "char"],
    function(x) rawToChar(charToRaw(x), multiple = TRUE)
  )

  # fill values that aren't in `nc` but are in `fill`
  fill_vars <- intersect(names(fill), setdiff(vars, existing_vars))
  values[fill_vars] <- lapply(fill[fill_vars], rep_len, n)

  # extract float info from filename if possible
  float_extract <- stringr::str_remove(
    stringr::str_extract(nc$filename, "dac/[a-z]+/[A-Za-z0-9]+"),
    "^dac/"
  )
  float <- list(float = rep(float_extract, n))

  # remove the 'dim' attribute from values
  cols <- lapply(c(float, values), "dim<-", NULL)
  tibble::new_tibble(cols, nrow = n)
}

#' @rdname argo_nc_traj_read
#' @export
argo_nc_traj_list_vars <- function(nc) {
  var_has_n_levels <- vapply(nc$var, function(x) x$dim[[1]]$name == "N_MEASUREMENT", logical(1))
  names(nc$var)[var_has_n_levels]
}
