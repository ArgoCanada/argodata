
argo_nc_vars_by_dimension <- function(nc, which, dim_name) {
  var_has_dim <- vapply(nc$var, function(x) length(x$dim) >= which, logical(1))
  var_has_dim[var_has_dim] <- vapply(
    nc$var[var_has_dim],
    function(x) identical(x$dim[[which]]$name, dim_name),
    logical(1)
  )
  names(nc$var)[var_has_dim]
}

argo_juld_to_date <- function(juld) {
   argo_juld_epoch + as.difftime(juld, units = "days")
}

argo_juld_to_date_tbl <- function(tbl) {
  is_juld <- names(tbl) %in% c("juld", "juld_adjusted", "JULD", "JULD_ADJUSTED")
  tbl[is_juld] <- lapply(tbl[is_juld], argo_juld_to_date)
  names(tbl) <- stringr::str_replace(names(tbl), "^juld", "date")
  names(tbl) <- stringr::str_replace(names(tbl), "^JULD", "DATE")
  tbl
}

argo_juld_epoch <- as.POSIXct("1950-01-01 00:00:00 UTC", tz = "UTC")
