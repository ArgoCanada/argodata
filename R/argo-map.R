
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
#' @return
#'   - `argo_map()`, `argo_map_default()`: A `list()` along `.x` with
#'     `.f` applied to each element.
#'   - `argo_set_mapper()`: The previous mapper.
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
  pb <- progress::progress_bar$new(
    "[:bar] :file",
    total = length(.x)
  )
  pb$tick(0)

  .f_with_progress <- function(file, ...) {
    pb$tick(tokens = list(file = basename(file)))
    .f(file, ...)
  }

  lapply(.x, .f_with_progress, ...)
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
