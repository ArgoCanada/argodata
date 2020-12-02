
#' Configure iteration over NetCDF files
#'
#' Several operations in argodata require iteration over many NetCDF files.
#' Users may wish to use parallel processing and/or
#' display progress during these operations; these functions allow custom
#' mappers and/or progress handlers to be set.
#'
#' @param .x A vector of NetCDF files.
#' @param .f A function
#' @param ... Passed to `.f`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' many_profiles <- head(argo_global_prof(), 300)
#'
#' # use `argo_map_multisession` to load files in parallel
#' argo_set_mapper(argo_map_multisession)
#' argo_prof_levels(many_profiles)
#'
#' # use `argo_map_future` with some other future backend
#' library(future)
#' plan(multisession) # cluster, multicore, etc.
#' argo_set_mapper(argo_map_future)
#' argo_prof_levels(many_profiles)
#' }
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
  .f_with_progress <- function(file, ...) {
    p(message = basename(file))
    .f(file, ...)
  }

  lapply(.x, .f_with_progress, ...)
}

#' @rdname argo_map
#' @export
argo_map_multisession <- function(.x, .f, ...) {
  force(.f)
  prev_plan <- future::plan(future::multisession, .skip = TRUE)
  on.exit(future::plan(prev_plan, .skip = TRUE))

  p <- progressr::progressor(along = .x)
  .f_with_progress <- function(file, ...) {
    p(message = basename(file))
    .f(file, ...)
  }

  future.apply::future_lapply(.x, .f_with_progress, ...)
}

#' @rdname argo_map
#' @export
argo_map_future <- function(.x, .f, ...) {
  force(.f)

  p <- progressr::progressor(along = .x)
  .f_with_progress <- function(file, ...) {
    p(message = basename(file))
    .f(file, ...)
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
  # progress handler displays messages which are used extensively here
  # to communicate filenames/urls
  requireNamespace("progress", quietly = TRUE)
  old_handlers <- progressr::handlers("progress") %||% "txtprogressbar"
  on.exit(progressr::handlers(old_handlers))
  progressr::with_progress(expr)
}
