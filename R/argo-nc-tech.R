
#' Read Argo tech NetCDF files
#'
#' @inheritParams argo_nc_read_vars
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_nc_tech
#'
#' @examples
#' nc_tech_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_tech.nc",
#'   package = "argodata"
#' )
#' nc_tech <- ncdf4::nc_open(nc_tech_file)
#'
#' argo_nc_tech_read_tech_param(nc_tech)
#' ncdf4::nc_close(nc_tech)
#'
argo_nc_tech_read_tech_param <- function(nc) {
  argo_nc_assert_dimensions(nc, "N_TECH_PARAM")

  n <- nc$dim$N_TECH_PARAM$len
  names <- argo_nc_values(nc, "TECHNICAL_PARAMETER_NAME")
  values <- argo_nc_values(nc, "TECHNICAL_PARAMETER_VALUE")
  tibble::new_tibble(c(names, values), nrow = n)
}
