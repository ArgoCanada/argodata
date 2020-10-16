
multi_file_download <- function(url, dest, quiet = FALSE) {
  stopifnot(
    is.character(url),
    is.character(dest)
  )

  # recycle url along dest (could want to query one url many times)
  url <- rep_len(url, length(dest))

  # download the files! (should really use a pool of some kind)
  error_message <- vapply(seq_along(url), function(i) {
    if (!quiet) message(glue("'{ url[i] }' => '{ dest[i] }'"))
    if (!dir.exists(dirname(dest[i]))) {
      dir.create(dirname(dest[i]), recursive = TRUE, showWarnings = FALSE)
    }

    tryCatch({
      curl::curl_download(url[i], dest[i])
      NA_character_
    }, error = function(e) {
      paste0(e, collapse = "\n")
    })
  }, character(1))

  # check that all were downloaded
  has_error <- !is.na(error_message)
  if (any(has_error)) {
    missing_paths <- dest[has_error]
    missing_paths_lab <- paste0(
      "'", utils::head(missing_paths, 10), "': ", error_message[has_error],
      collapse = "\n"
    )
    files <- if (length(missing_paths) != 1) "files" else "file"
    abort(
      glue(
        "{ length(missing_paths) } { files } failed to download:\n{missing_paths_lab}"
      )
    )
  }

  invisible(dest)
}
