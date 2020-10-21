
#' Load Argo global index files
#'
#' @inheritParams argo_download
#' @param file A path to a previously downloaded index file.
#'
#' @return A [tibble::tibble()].
#' @export
#'
argo_global_meta <- function(download = NULL, quiet = FALSE) {
  argo_global(
    path = "ar_index_global_meta.txt.gz",
    name = "meta",
    read_fun = argo_read_global_meta,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_global_meta
#' @export
argo_global_prof <- function(download = NULL, quiet = FALSE) {
  argo_global(
    path = "ar_index_global_prof.txt.gz",
    name = "prof",
    read_fun = argo_read_global_prof,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_global_meta
#' @export
argo_global_tech <- function(download = NULL, quiet = FALSE) {
  argo_global(
    path = "ar_index_global_tech.txt.gz",
    name = "tech",
    read_fun = argo_read_global_tech,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_global_meta
#' @export
argo_global_traj <- function(download = NULL, quiet = FALSE) {
  argo_global(
    path = "ar_index_global_traj.txt.gz",
    name = "traj",
    read_fun = argo_read_global_traj,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_global_meta
#' @export
argo_read_global_meta <- function(file) {
  readr::read_csv(
    file,
    col_types = readr::cols(
      file = readr::col_character(),
      profiler_type = readr::col_double(),
      institution = readr::col_character(),
      date_update = readr::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

#' @rdname argo_global_meta
#' @export
argo_read_global_prof <- function(file) {
  readr::read_csv(
    file,
    col_types = readr::cols(
      file = readr::col_character(),
      date = readr::col_datetime("%Y%m%d%H%M%S"),
      latitude = readr::col_double(),
      longitude = readr::col_double(),
      ocean = readr::col_character(),
      profiler_type = readr::col_double(),
      institution = readr::col_character(),
      date_update = readr::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

#' @rdname argo_global_meta
#' @export
argo_read_global_tech <- function(file) {
  readr::read_csv(
    file,
    col_types = readr::cols(
      file = readr::col_character(),
      institution = readr::col_character(),
      date_update = readr::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

#' @rdname argo_global_meta
#' @export
argo_read_global_traj <- function(file) {
  readr::read_csv(
    file,
    col_types = readr::cols(
      file = readr::col_character(),
      latitude_max = readr::col_double(),
      latitude_min = readr::col_double(),
      longitude_max = readr::col_double(),
      longitude_min = readr::col_double(),
      profiler_type = readr::col_double(),
      institution = readr::col_character(),
      date_update = readr::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

argo_global <- function(path, name, read_fun, download, quiet) {
  should_download <- download %||% argo_should_download(path)

  if (should_download || !(name %in% names(argo_global_cache))) {
    if (!quiet) message(glue("Loading argo_global_{ name }()"))
    value <- read_fun(argo_download(path, download = download, quiet = quiet))
    argo_global_cache[[name]] <- value

  } else if(identical(download, FALSE) && (name %in% names(argo_global_cache))) {
    abort(glue("File does not exist and `download = FALSE`:\n'{ path }'."))
  }

  argo_global_cache[[name]]
}

# environment where the loaded data tibbles are cached
argo_global_cache <- new.env(parent = emptyenv())
