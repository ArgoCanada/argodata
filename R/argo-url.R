
#' Construct Argo paths and URLs
#'
#' @inheritParams argo_mirror
#'
#' @return A relative path to a file.
#' @export
#' @rdname argo_url
#'
#' @examples
#' argo_path_global_meta()
#' argo_path_global_prof()
#' argo_path_global_tech()
#' argo_path_global_traj()
#'
#' argo_url_global_meta()
#' argo_url_global_prof()
#' argo_url_global_tech()
#' argo_url_global_traj()
#'
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

#' @rdname argo_url
#' @export
argo_url_global_meta <- function(mirror = argo_mirror()) {
  paste0(mirror, argo_path_global_meta())
}

#' @rdname argo_url
#' @export
argo_url_global_prof <- function(mirror = argo_mirror()) {
  paste0(mirror, argo_path_global_prof())
}

#' @rdname argo_url
#' @export
argo_url_global_tech <- function(mirror = argo_mirror()) {
  paste0(mirror, argo_path_global_tech())
}

#' @rdname argo_url
#' @export
argo_url_global_traj <- function(mirror = argo_mirror()) {
  paste0(mirror, argo_path_global_traj())
}
