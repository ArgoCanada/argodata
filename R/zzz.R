
# global temporary directory
argo_tmp_dir <- NULL

.onLoad <- function(...) {
  argo_tmp_dir <<- tempfile()
  dir.create(argo_tmp_dir)
}

.onUnload <- function(...) {
  unlink(argo_tmp_dir, recursive = TRUE)
}
