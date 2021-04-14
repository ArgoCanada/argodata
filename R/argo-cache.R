
#' Get and set the default cache directory
#'
#' The cache directory stores previously downloaded files to access them
#' more quickly. The cache directory stores files in the same format as a
#' an [argo_mirror()]. By default, the cache is stored in a temporary
#' directory that is cleared when the session is restarted. This ensures
#' access to the latest index and files by default.
#'
#' @inheritParams argo_path_info
#' @param cache_dir A writable directory in which downloaded files can be
#'   cached.
#' @param expr An expression to be evaluated with the specified
#'   default `cache_dir`.
#' @param all Should index files be downloaded even if they have not been
#'   previously downloaded?
#' @inheritParams argo_mirror
#' @inheritParams argo_should_download
#'
#' @return
#'   - `argo_cache_dir()`: The directory where cache files are located
#'   - `argo_set_cache_dir()`: The previously set cache directory
#'   - `with_argo_cache_dir()`, `with_argo_example_cache()`: The result of
#'     `expr`.
#'   - `argo_cached()`: The file path to the cached version of the Argo file
#'     (which may or may not exist).
#'   - `argo_update_data()`, `argo_update_global()`: The locations of the
#'     updated files.
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
argo_update_global <- function(max_global_cache_age = -Inf, all = TRUE, quiet = FALSE) {
  # slightly different than refreshing the data (downloads files that have
  # never been downloaded)
  global_files <- c(
    "ar_index_global_meta.txt.gz",
    "ar_index_global_prof.txt.gz",
    "ar_index_global_tech.txt.gz",
    "ar_index_global_traj.txt.gz",
    "argo_bio-profile_index.txt.gz",
    "argo_bio-traj_index.txt.gz",
    "argo_synthetic-profile_index.txt.gz"
  )

  if (!all) {
    global_files <- intersect(global_files, list.files(argo_cache_dir()))
  }

  # remove cached versions
  existing_globals <- ls(envir = argo_global_cache)
  rm(list = existing_globals, envir = argo_global_cache)

  argo_download(
    global_files,
    download = argo_should_download(global_files, max_global_cache_age = max_global_cache_age),
    async = FALSE,
    quiet = quiet
  )
}

#' @rdname argo_cache_dir
#' @export
argo_update_data <- function(max_data_cache_age = -Inf, quiet = FALSE) {
  nc_files <- list.files(argo_cache_dir(), "\\.nc$", recursive = TRUE)
  argo_download(
    nc_files,
    download = argo_should_download(nc_files, max_data_cache_age = max_data_cache_age),
    async = TRUE,
    quiet = quiet
  )
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

#' @rdname argo_cache_dir
#' @export
with_argo_example_cache <- function(expr) {
  old_options <- options(
    argodata.max_global_cache_age = Inf,
    argodata.max_data_cache_age = Inf
  )
  on.exit(options(old_options))
  with_argo_cache_dir(system.file("cache-test", package = "argodata"), expr)
}

#' @rdname argo_cache_dir
#' @export
argo_cached <- function(path) {
  as.character(fs::path(argo_cache_dir(), as_argo_path(path)))
}
