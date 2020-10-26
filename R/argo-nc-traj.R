
#' Read Argo trajectory NetCDF files
#'
#' @inheritParams argo_nc_prof_read
#'
#' @return A [tibble::tibble()] containing columns "float" and
#'   columns listed in `vars`.
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
#' ncdf4::nc_close(nc_traj)
#'
argo_nc_traj_read <- function(nc, vars = NULL) {
  nc_vars <- argo_nc_traj_list_vars(nc)
  vars <- if (is.null(vars)) nc_vars else intersect(vars, nc_vars)
  n <- nc$dim$N_MEASUREMENT$len

  # assign values
  values <- lapply(nc$var[vars], ncdf4::ncvar_get, nc = nc)

  # character types that are flags come as a single string, but need to be
  # one character per row
  vars_is_char <- vapply(nc$var[vars], function(var) identical(var$prec, "char"), logical(1))

  # comes as character(n_prof)
  values[vars_is_char] <- lapply(
    values[vars_is_char],
    function(x) rawToChar(charToRaw(x), multiple = TRUE)
  )

  # extract float info from filename if possible
  float_extract <- stringr::str_remove(
    stringr::str_extract(nc$filename, "dac/[a-z]+/[A-Za-z0-9]+"),
    "^dac/"
  )
  float <- list(float = vctrs::vec_rep(float_extract, n))

  # remove the 'dim' attribute from values
  cols <- lapply(c(float, values), "dim<-", NULL)
  tibble::new_tibble(cols, nrow = n)
}

#' @rdname argo_nc_traj_read
#' @export
argo_nc_traj_list_vars <- function(nc) {
  argo_nc_vars_by_dimension(nc, 1, "N_MEASUREMENT")
}
