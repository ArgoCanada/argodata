
#' Download Argo data files
#'
#' @param path A path relative to the root directory of
#'   [argo_mirror()].
#' @param download A logical vector indicating whether or not
#'   a file should be downloaded. Defaults to the value of
#'   [argo_should_download()], which is `TRUE` for files that
#'   do not exist in the cache or global files that were cached more than 24
#'   hours ago.
#' @param quiet Use `FALSE` to show which files are downloaded and for more
#'   verbose error messages.
#'
#' @return A vector of cached filenames corresponding to `path`.
#' @export
#'
argo_download <- function(path, download = argo_should_download(path), quiet = TRUE) {
  if (is.data.frame(path)) {
    path <- path[["path"]]
  }

  path_download <- unique(path[rep_len(as.logical(download), length(path))])
  url_download <- argo_url(path_download)
  cached_download <- rlang::set_names(argo_cached(path_download), path_download)

  # download the files!
  for (i in seq_along(path_download)) {
    if (!quiet) message(glue("'{ url_download[i] }' => '{ cached_download[i] }'"))
    try(curl::curl_download(url_download[i], cached_download[i]), silent = quiet)
  }

  if (!all(file.exists(cached_download))) {
    missing_paths <- path_download[!file.exists(cached_download)]
    missing_paths_lab <- paste0("'", head(missing_paths, 10), "'", collapse = "\n")
    files <- if (length(missing_paths) != 1) "files" else "file"
    abort(
      glue(
        "{ length(missing_paths) } { files } failed to download:\n{missing_paths_lab}"
      )
    )
  }

  unname(cached[path])
}

#' @rdname argo_download
#' @export
argo_should_download <- function(path,
                                 max_global_cache_age = getOption("argodata.max_global_cache_age", 24),
                                 max_data_cache_age = getOption("argodata.max_data_cache_age", Inf)) {
  if (is.data.frame(path)) {
    path <- path[["path"]]
  }

  cached <- argo_cached(path)
  modified <- file.mtime(cached)
  age <- as.numeric(Sys.time() - file.mtime(), units = "hours")
  is_global <- path == "/" | path == ""

  (is_global & (age > max_global_cache_age)) |
      (age > max_data_cache_age)
}
