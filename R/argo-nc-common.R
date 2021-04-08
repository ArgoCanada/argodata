
argo_read_many <- function(assert_fun, read_fun, ...,
                           path, vars, download, quiet) {
  path <- as_argo_path(path)
  assert_fun(path)

  cached <- argo_download(path, download = download, quiet = isTRUE(quiet))

  # names should be of the 'file' version, which can be
  # joined with one of the global tables
  names(cached) <- stringr::str_remove(path, "^dac/")

  if (!isTRUE(quiet)) {
    files_word <- if (length(cached) != 1) "files" else "file"
    title <- glue("Extracting from { length(cached) } { files_word }")
    message(title)
  }

  # temporary hack to support quiet in some read_fun
  if ("quiet" %in% names(formals(read_fun))) {
    tbls <- argo_map(
      cached,
      read_fun,
      vars = argo_unsanitize_vars(vars),
      quiet = quiet,
      ...
    )
  } else {
    tbls <- argo_map(
      cached,
      read_fun,
      vars = argo_unsanitize_vars(vars),
      ...
    )
  }

  tbl <- vctrs::vec_rbind(!!! tbls, .names_to = "file")
  names(tbl) <- argo_sanitize_vars(names(tbl))

  argo_juld_to_date_tbl(tbl)
}

with_argo_nc_file <- function(file, fun, ...) {
  # the dimension values are used in almost every function and
  # suppressing them saves little time
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc))
  fun(nc, ...)
}

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

argo_nc_assert_dimensions <- function(nc, dimensions, action = abort) {
  dims <- Map("[[", list(nc$dim), dimensions)
  dim_is_missing <- vapply(dims, is.null, logical(1))
  if (any(dim_is_missing)) {
    missing_dims <- dimensions[dim_is_missing]
    dims_lab <- if(length(missing_dims) != 1) "dimensions" else "dimension"
    missing_dims_list <- glue::glue_collapse(
      paste0("'", missing_dims, "'"),
      sep = ",",
      last = " and "
    )

    action(glue("'{ nc$filename }' is missing { dims_lab } { missing_dims_list }."))
    FALSE
  } else {
    TRUE
  }
}

argo_assert_path_type <- function(path, pattern, file_type) {
  path_matches_pattern <- stringr::str_detect(path, pattern)

  if (any(!path_matches_pattern)) {
    bad_files <- path[!path_matches_pattern]
    paths <- if (length(bad_files) != 1) "paths" else "path"
    bad_files_label <- paste0(
      "'", utils::head(bad_files, 20), "'",
      collapse = "\n"
    )

    abort(
      glue(
        "Found { length(bad_files) } invalid Argo { file_type } { paths }:\n{ bad_files_label}"
      )
    )
  }

  invisible(path)
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

argo_unsanitize_vars <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    x <- toupper(x)
    stringr::str_replace(x, "^DATE", "JULD")
  }
}

argo_sanitize_vars <- function(x) {
  tolower(x)
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
