
#' Load Argo global index files
#'
#' @inheritParams argo_download
#'
#' @return A [tibble::tibble()].
#' @export
#'
#' @examples
#' with_argo_example_cache({
#'   argo_global_meta()
#' })
#'
#' with_argo_example_cache({
#'   argo_global_tech()
#' })
#'
#' with_argo_example_cache({
#'   argo_global_traj()
#' })
#'
#' with_argo_example_cache({
#'   argo_global_bio_traj()
#' })
#'
#' \dontrun{
#' # extended download time if no cached version is available
#' argo_global_prof()
#' argo_global_bio_prof()
#' argo_global_synthetic_prof()
#' }
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
argo_global_bio_traj <- function(download = NULL, quiet = FALSE) {
  argo_global(
    path = "argo_bio-traj_index.txt.gz",
    name = "bio_traj",
    read_fun = argo_read_global_bio_traj,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_global_meta
#' @export
argo_global_bio_prof <- function(download = NULL, quiet = FALSE) {
  argo_global(
    path = "argo_bio-profile_index.txt.gz",
    name = "bio_prof",
    read_fun = argo_read_global_bio_prof,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_global_meta
#' @export
argo_global_synthetic_prof <- function(download = NULL, quiet = FALSE) {
  argo_global(
    path = "argo_synthetic-profile_index.txt.gz",
    name = "synthetic_prof",
    read_fun = argo_read_global_synthetic_prof,
    download = download,
    quiet = quiet
  )
}


#' Read global index files
#'
#' @param file A path to a previously downloaded index file.
#'
#' @return A [tibble::tibble()]
#' @export
#'
argo_read_global_meta <- function(file) {
  vroom::vroom(
    file, delim = ",",
    col_types = vroom::cols(
      file = vroom::col_character(),
      profiler_type = vroom::col_double(),
      institution = vroom::col_character(),
      date_update = vroom::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

#' @rdname argo_read_global_meta
#' @export
argo_read_global_prof <- function(file) {
  vroom::vroom(
    file, delim = ",",
    col_types = vroom::cols(
      file = vroom::col_character(),
      date = vroom::col_datetime("%Y%m%d%H%M%S"),
      latitude = vroom::col_double(),
      longitude = vroom::col_double(),
      ocean = vroom::col_character(),
      profiler_type = vroom::col_double(),
      institution = vroom::col_character(),
      date_update = vroom::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

#' @rdname argo_read_global_meta
#' @export
argo_read_global_tech <- function(file) {
  vroom::vroom(
    file, delim = ",",
    col_types = vroom::cols(
      file = vroom::col_character(),
      institution = vroom::col_character(),
      date_update = vroom::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

#' @rdname argo_read_global_meta
#' @export
argo_read_global_traj <- function(file) {
  vroom::vroom(
    file, delim = ",",
    col_types = vroom::cols(
      file = vroom::col_character(),
      latitude_max = vroom::col_double(),
      latitude_min = vroom::col_double(),
      longitude_max = vroom::col_double(),
      longitude_min = vroom::col_double(),
      profiler_type = vroom::col_double(),
      institution = vroom::col_character(),
      date_update = vroom::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

#' @rdname argo_read_global_meta
#' @export
argo_read_global_bio_traj <- function(file) {
  vroom::vroom(
    file, delim = ",",
    col_types = vroom::cols(
      file = vroom::col_character(),
      latitude_max = vroom::col_double(),
      latitude_min = vroom::col_double(),
      longitude_max = vroom::col_double(),
      longitude_min = vroom::col_double(),
      profiler_type = vroom::col_double(),
      institution = vroom::col_character(),
      parameters = vroom::col_character(),
      date_update = vroom::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

#' @rdname argo_read_global_meta
#' @export
argo_read_global_bio_prof <- function(file) {
  vroom::vroom(
    file, delim = ",",
    col_types = vroom::cols(
      file = vroom::col_character(),
      date = vroom::col_datetime("%Y%m%d%H%M%S"),
      latitude = vroom::col_double(),
      longitude = vroom::col_double(),
      ocean = vroom::col_character(),
      profiler_type = vroom::col_double(),
      institution = vroom::col_character(),
      parameters = vroom::col_character(),
      parameter_data_mode = vroom::col_character(),
      date_update = vroom::col_datetime("%Y%m%d%H%M%S")
    ),
    comment = "#"
  )
}

#' @rdname argo_read_global_meta
#' @export
argo_read_global_synthetic_prof <- function(file) {
  vroom::vroom(
    file, delim = ",",
    col_types = vroom::cols(
      file = vroom::col_character(),
      date = vroom::col_datetime("%Y%m%d%H%M%S"),
      latitude = vroom::col_double(),
      longitude = vroom::col_double(),
      ocean = vroom::col_character(),
      profiler_type = vroom::col_double(),
      institution = vroom::col_character(),
      parameters = vroom::col_character(),
      parameter_data_mode = vroom::col_character(),
      date_update = vroom::col_datetime("%Y%m%d%H%M%S")
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
