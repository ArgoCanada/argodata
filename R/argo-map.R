
#' Configure Long-Running Iteration
#'
#' Several operations in argodata require long-running iteration (e.g.,
#' over many NetCDF files). Users may wish to use parallel processing and/or
#' display progress during these operations; these functions allow custom
#' mappers to be set for these operations. You can configure
#'
#' @param .x A vector
#' @param .f A function
#' @param ... Passed to `.f`
#'
#' @export
#'
argo_map <- function(.x, .f, ...) {
  args <- rlang::list2(.x, .f, ...)
  do.call(argo_mapper(), args)
}

#' @rdname argo_map
#' @export
argo_set_mapper <- function(.f) {
  previous_mapper <- getOption("argodata.mapper")
  options(argodata.mapper = .f)
  invisible(previous_mapper)
}

#' @rdname argo_map
#' @export
argo_map_default <- function(.x, .f, ...) {
  force(.f)
  p <- progressr::progressor(along = .x)
  .f_with_progress <- function(...) {
    p()
    .f(...)
  }

  lapply(.x, .f_with_progress, ...)
}

#' @rdname argo_map
#' @export
argo_map_future <- function(.x, .f, ...) {
  force(.f)
  p <- progressr::progressor(along = .x)
  .f_with_progress <- function(...) {
    p()
    .f(...)
  }

  future.apply::future_lapply(.x, .f_with_progress, ...)
}

argo_mapper <- function() {
  x <- getOption("argodata.mapper", argo_map_default)

  if (is.character(x) && (length(x) == 1) && grepl("::", x)) {
    split <- strsplit(x, "::", fixed = TRUE)[[1]]
    asNamespace(split[1])[[split[2]]]
  } else if(is.function(x)) {
    x
  } else {
    abort("`getOption('argodata.mapper')` must be a string (e.g., 'pkg::fun') or a function.")
  }
}

with_argo_progress <- function(expr, quiet = FALSE, title = NULL) {
  if (!is.null(title) && !quiet) {
    message(title)
  }

  argo_progress_wrapper(quiet)(expr)
}

argo_progress_wrapper <- function(quiet) {
  if (quiet) {
    progressr::without_progress
  } else {
    getOption("argodata.progress_wrapper", argo_progress_wrapper_default)
  }
}

argo_progress_wrapper_default <- function(expr) {
  old_handlers <- progressr::handlers("progress") %||% "txtprogressbar"
  on.exit(progressr::handlers(old_handlers))
  progressr::with_progress(expr)
}
