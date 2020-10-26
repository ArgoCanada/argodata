
argo_nc_values <- function(nc, vars) {
  values <- lapply(nc$var[vars], ncdf4::ncvar_get, nc = nc)
  lapply(values, argo_undimension)
}

argo_nc_vars_by_dimension <- function(nc, which, dim_name) {
  var_has_dim <- vapply(nc$var, function(x) length(x$dim) >= which, logical(1))
  var_has_dim[var_has_dim] <- vapply(
    nc$var[var_has_dim],
    function(x) identical(x$dim[[which]]$name, dim_name),
    logical(1)
  )
  names(nc$var)[var_has_dim]
}

argo_nc_extract_float <- function(nc) {
  stringr::str_remove(
    stringr::str_extract(
      nc$filename,
      "dac/[a-z]+/[A-Za-z0-9]+"
    ),
    "^dac/"
  )
}

argo_nc_new_tibble <- function(nc, ..., nrow) {
  # extract float info from filename if possible
  float <- list(float = vctrs::vec_rep(argo_nc_extract_float(nc), nrow))
  tibble::new_tibble(c(float, ...), nrow = nrow)
}

argo_string_to_chars <- function(x, n = NULL) {
  if (is.null(n)) {
    rawToChar(charToRaw(x), multiple = TRUE)
  } else {
    rawToChar(vapply(x, charToRaw, raw(n)), multiple = TRUE)
  }
}

argo_string_to_chars_tbl <- function(tbl, n = NULL) {
  is_char <- vapply(tbl, is.character, logical(1))
  tbl[is_char] <- lapply(tbl[is_char], argo_string_to_chars, n = n)
  tbl
}

argo_undimension <- function(x) {
  dim(x) <- NULL
  x
}

argo_juld_to_date <- function(juld) {
   argo_juld_epoch + as.difftime(juld, units = "days")
}

argo_juld_to_date_tbl <- function(tbl) {
  is_juld <- stringr::str_detect(names(tbl), "^(JULD|juld)")
  tbl[is_juld] <- lapply(tbl[is_juld], argo_juld_to_date)
  names(tbl) <- stringr::str_replace(names(tbl), "^juld", "date")
  names(tbl) <- stringr::str_replace(names(tbl), "^JULD", "DATE")
  tbl
}

argo_juld_epoch <- as.POSIXct("1950-01-01 00:00:00 UTC", tz = "UTC")
