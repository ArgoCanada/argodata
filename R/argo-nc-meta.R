
#' Read Argo meta NetCDF files
#'
#' @inheritParams argo_vars
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_nc_meta
#'
#' @examples
#' nc_meta_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_meta.nc",
#'   package = "argodata"
#' )
#' nc_meta <- ncdf4::nc_open(nc_meta_file)
#'
#' argo_nc_meta_read_config(nc_meta)
#'
#' ncdf4::nc_close(nc_meta)
#'
argo_nc_meta_read_config <- function(nc) {
  n_missions <- nc$dim$N_MISSIONS$len
  n <- nc$dim$N_CONFIG_PARAM$len

  if (is.null(n) || is.null(n_missions)) {
    abort(glue("'{ nc$filename }' is missing dimension 'N_CONFIG_PARAM' or 'N_MISSIONS'"))
  }

  mission_number <- argo_nc_values(nc, "CONFIG_MISSION_NUMBER")
  mission_number <- lapply(mission_number, vctrs::vec_rep, n)
  names <- argo_nc_values(nc, "CONFIG_PARAMETER_NAME")
  values <- argo_nc_values(nc, "CONFIG_PARAMETER_VALUE")

  argo_nc_new_tibble(nc, mission_number, names, values, nrow = n_missions * n)
}
