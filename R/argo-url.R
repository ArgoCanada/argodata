
#' Construct Argo paths and URLs
#'
#' @inheritParams argo_mirror
#'
#' @return A relative path to a file.
#' @export
#'
#' @examples
#' argo_path_global_meta()
#' argo_path_global_prof()
#' argo_path_global_tech()
#' argo_path_global_traj()
#'
#' argo_url(argo_path_global_meta())
#'
argo_url <- function(path) {
  if (is.data.frame(path)) {
    path <- path[["path"]]
  }

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
