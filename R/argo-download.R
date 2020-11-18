
#' Download Argo data files
#'
#' @inheritParams argo_path_info
#' @param download A logical vector indicating whether or not
#'   a file should be downloaded. Defaults to the value of
#'   [argo_should_download()], which is `TRUE` for files that
#'   do not exist in the cache.
#' @param async Use `TRUE` to perform HTTP requests in parallel. This is much
#'   faster for large numbers of small files.
#' @param quiet Use `FALSE` to show which files are downloaded and for more
#'   verbose error messages.
#' @param max_global_cache_age,max_data_cache_age The maximum age in hours
#'   to keep cached files. Use `Inf` to always use cached files; use `-Inf`
#'   to always force download. You can set the default values of these using
#'   `options(argodata.max_global_cache_age = ...)`
#'   and/or `options(argodata.max_data_cache_age = ...)`.
#'
#' @return A vector of cached filenames corresponding to `path`.
#' @export
#'
argo_download <- function(path, download = NULL, async = NULL, quiet = FALSE) {
  path <- as_argo_path(path)
  download <- download %||% argo_should_download(path)
  async <- async %||% (length(path) > 1)

  path_download <- unique(path[rep_len(as.logical(download), length(path))])
  url_download <- argo_url(path_download)
  cached_download <- rlang::set_names(argo_cached(path_download), path_download)

  if (async) {
    multi_file_download_async(url_download, cached_download, quiet = quiet)
  } else {
    multi_file_download(url_download, cached_download, quiet = quiet)
  }

  invisible(argo_cached(path))
}

#' @rdname argo_download
#' @export
argo_should_download <- function(path,
                                 max_global_cache_age = getOption("argodata.max_global_cache_age", Inf),
                                 max_data_cache_age = getOption("argodata.max_data_cache_age", Inf)) {
  path <- as_argo_path(path)

  cached <- argo_cached(path)
  modified <- file.mtime(cached)
  age <- as.numeric(Sys.time() - modified, units = "hours")
  is_global <- dirname(path) == "/" | dirname(path) == "."

  !file.exists(cached) |
    (is_global & (age > max_global_cache_age)) |
    (age > max_data_cache_age)
}
