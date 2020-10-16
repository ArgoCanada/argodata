
argo_path <- function(path) {
  if (is.data.frame(path) && ("file" %in% names(path))) {
    path <- path[["file"]]
    path <- paste0(rep_len("dac/", length(path)), path)
  }

  if (!is.character(path)) {
    abort(
      "`path` must be a character vector or data frame with a character vector column named 'file'."
    )
  }

  path
}
