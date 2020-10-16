
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
  path <- argo_path(path)

  if (length(path) == 0) {
    return(character(0))
  }

  path <- gsub("^/", "", path)
  paste0(argo_mirror(), "/", path)
}

#' @rdname argo_url
#' @export
argo_path_global_meta <- function() {
  "/ar_index_global_prof.txt.gz"
}

#' @rdname argo_url
#' @export
argo_path_global_prof <- function() {
  "/ar_index_global_prof.txt.gz"
}

#' @rdname argo_url
#' @export
argo_path_global_tech <- function() {
  "/ar_index_global_tech.txt.gz"
}

#' @rdname argo_url
#' @export
argo_path_global_traj <- function() {
  "/ar_index_global_traj.txt.gz"
}
