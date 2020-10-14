
#' Get and set the default cache directory
#'
#' The cache directory stores previously downloaded files to access them
#' more quickly. The cache directory stores files in the same format as a
#' an [argo_mirror()].
#'
#' @param cache_dir A writable directory in which downloaded files can be
#'   cached.
#' @inheritParams argo_mirror
#'
#' @export
#'
#' @examples
#' argo_cache_dir()
#'
#' temp_dir <- tempfile()
#' with_argo_cache_dir(temp_dir, argo_cache_dir())
#' unlink(temp_dir, recursive = TRUE)
#'
argo_cache_dir <- function() {
  getOption("argodata.cache_dir", NULL) %||% argo_tmp_dir
}

#' @rdname argo_cache_dir
#' @export
argo_set_cache_dir <- function(cache_dir) {
  if (is.null(cache_dir)) {
    old_cache_dir <- getOption("argodata.cache_dir", NULL)
    options(argodata.cache_dir = cache_dir)
    return(invisible(old_cache_dir))
  }

  if (!is.character(cache_dir) || length(cache_dir) != 1 || is.na(cache_dir)) {
    abort("`cache_dir` must be a character vector of length 1.")
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(cache_dir)) {
    abort(glue("'{ cache_dir }' is not a directory and could not be created."))
  }

  old_cache_dir <- getOption("argodata.cache_dir", NULL)
  options(argodata.cache_dir = as.character(fs::path_abs(cache_dir)))
  invisible(old_cache_dir)
}

#' @rdname argo_cache_dir
#' @export
with_argo_cache_dir <- function(cache_dir, expr) {
  old_cache_dir <- argo_set_cache_dir(cache_dir)
  on.exit(argo_set_cache_dir(old_cache_dir))
  force(expr)
}
