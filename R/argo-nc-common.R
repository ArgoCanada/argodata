
argo_read_many <- function(assert_fun, read_fun, ...,
                           path, vars, download, quiet, trim = FALSE) {
  path <- as_argo_path(path)
  assert_fun(path)

  # you should always be able to pass in an absolute path to
  # an actual file and have it work
  path_is_abs <- fs::is_absolute_path(path) & file.exists(path)

  cached <- path
  cached[!path_is_abs & !is.na(path)] <- argo_download(
    path[!path_is_abs & !is.na(path)],
    download = download,
    quiet = isTRUE(quiet)
  )

  # names should be of the 'file' version, which can be
  # joined with one of the global tables
  names(cached) <- stringr::str_remove(path, "^(dac|aux)/")

  # drop NA filenames (e.g., failed aux downloads)
  cached <- cached[!is.na(cached)]

  if (!isTRUE(quiet)) {
    files_word <- if (length(cached) != 1) "files" else "file"
    title <- glue("Extracting from { length(cached) } { files_word }")
    message(title)
  }

  tbls <- argo_map(
    cached,
    read_fun,
    vars = argo_unsanitize_vars(vars),
    quiet = quiet,
    ...
  )

  tbl <- vctrs::vec_rbind(!!! tbls, .names_to = "file")

  names(tbl) <- argo_sanitize_vars(names(tbl))

  tbl <- argo_juld_to_date_tbl(tbl)

  if (trim) {
    is_char <- vapply(tbl, is.character, logical(1))
    tbl[is_char] <- lapply(tbl[is_char], stringr::str_trim)
  }

  tbl
}

argo_assert_path_type <- function(path, pattern, file_type) {
  path_matches_pattern <- stringr::str_detect(path, pattern)

  if (any(!path_matches_pattern, na.rm = TRUE)) {
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

argo_unsanitize_vars <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    x <- toupper(x)
    x <- stringr::str_replace(x, "^DATE", "JULD")
    stringr::str_replace(x, "_DPRES$", "_dPRES")
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
