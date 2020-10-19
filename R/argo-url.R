
#' Construct Argo paths and URLs
#'
#' @inheritParams argo_cache_dir
#'
#' @return A full URL according to the current [argo_mirror()]
#'   and `path`.
#' @export
#'
#' @examples
#' argo_url("ar_index_global_prof.txt.gz")
#'
argo_url <- function(path) {
  path <- as_argo_path(path)

  if (length(path) == 0) {
    return(character(0))
  }

  path <- gsub("^/", "", path)
  paste0(argo_mirror(), "/", path)
}
