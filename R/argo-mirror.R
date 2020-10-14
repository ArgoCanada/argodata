
#' Argo Data default mirror
#'
#' Fetch the default Argo mirror using [argo_mirror()] or set it
#' using by [argo_set_mirror()]. The default mirror is set to
#' <ftp://ftp.ifremer.fr/ifremer/argo>. You can also set the default mirror
#' using `options("argodata.mirror = "path/to/mirror")`. Use
#' [with_argo_mirror()] to temporarily change the default mirror.
#'
#' @param mirror The URL to an Argo mirror or a path to a directory
#'   where Argo data has been cached or synced. According to the
#'   [Argo data acess page](http://www.argodatamgt.org/Access-to-data/Access-via-FTP-or-HTTPS-on-GDAC),
#'   The following public mirrors are available:
#'
#'   - <ftp://usgodae.org/pub/outgoing/argo>
#'   - <ftp://ftp.ifremer.fr/ifremer/argo>
#'   - <https://data-argo.ifremer.fr>
#'
#'   Use `NULL` to reset to the default mirror.
#'
#' @param expr An expression to be evaluated with the specified
#'   default `mirror`.
#'
#' @export
#'
#' @examples
#' argo_mirror()
#' with_argo_mirror("https://data-argo.ifremer.fr/", argo_mirror())
#'
argo_mirror <- function() {
  getOption("argodata.mirror", NULL) %||% "ftp://ftp.ifremer.fr/ifremer/argo"
}

#' @rdname argo_mirror
#' @export
argo_set_mirror <- function(mirror) {
  if (is.null(mirror)) {
    old_mirror <- argo_mirror()
    options(argodata.mirror = mirror)
    return(invisible(old_mirror))
  }

  if (!is.character(mirror) || length(mirror) != 1 || is.na(mirror)) {
    abort("`mirror` must be a character vector of length 1.")
  }

  if (dir.exists(mirror)) {
    mirror <- paste0("file://", mirror)
  }

  prefix <- strsplit(mirror, "//", fixed = TRUE)[[1]][1]
  if (!(prefix %in% c("ftp:", "http:", "https:", "file:"))) {
    abort("`mirror` must be a valid URL")
  }

  # strip trailing slash if one exists
  mirror <- gsub("/$", "", mirror)

  old_mirror <- argo_mirror()
  options(argodata.mirror = mirror)
  invisible(old_mirror)
}

#' @rdname argo_mirror
#' @export
with_argo_mirror <- function(mirror, expr) {
  old_mirror <- argo_set_mirror(mirror)
  on.exit(argo_set_mirror(old_mirror))
  force(expr)
}
