
# global temporary directory
argo_tmp_dir <- NULL

# nocov start

.onLoad <- function(...) {
  # we shortcut around tibble::new_tibble() for speed, but we need
  # the namespace loaded for the print methods to kick in
  requireNamespace("tibble", quietly = TRUE)

  # create package-specific temporary directory
  argo_tmp_dir <<- tempfile()
  dir.create(argo_tmp_dir)

  # also set cache_dir and mirror based on environment variables
  cache_dir_env <- Sys.getenv("R_ARGO_CACHE_DIR", unset = "")
  if (cache_dir_env != "") argo_set_cache_dir(cache_dir_env)

  mirror_env <- Sys.getenv("R_ARGO_MIRROR", unset = "")
  if (mirror_env != "") argo_set_mirror(mirror_env)
}

.onUnload <- function(...) {
  unlink(argo_tmp_dir, recursive = TRUE)
}

# nocov end
