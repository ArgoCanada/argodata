
argo_path <- function(path) {
  if (is.data.frame(path)) {
    path <- path[["path"]]
  }

  if (!is.character(path)) {
    abort(
      "`path` must be a character vector or data frame with a character vector column named 'path'."
    )
  }

  path
}
