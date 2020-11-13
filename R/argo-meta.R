
#' Load Argo float meta
#'
#' @inheritParams argo_prof_levels
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_meta
#'
#' @examples
#' with_argo_example_cache({
#'   argo_meta_config_param("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_missions("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_trans_system("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_launch_config_param("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_sensor("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_meta_param("dac/csio/2900313/2900313_meta.nc")
#' })
#'
#' meta_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_meta.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_meta_config_param(meta_file)
#' argo_read_meta_missions(meta_file)
#' argo_read_meta_trans_system(meta_file)
#' argo_read_meta_launch_config_param(meta_file)
#' argo_read_meta_sensor(meta_file)
#' argo_read_meta_param(meta_file)
#'
argo_meta_config_param <- function(path, download = NULL, quiet = FALSE) {
  tbl <- argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_config_param,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet
  )

  is_char <- vapply(tbl, is.character, logical(1))
  tbl[is_char] <- lapply(tbl[is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_meta
#' @export
argo_meta_missions <- function(path, download = NULL, quiet = FALSE) {
  tbl <- argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_missions,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet
  )

  is_char <- vapply(tbl, is.character, logical(1))
  tbl[is_char] <- lapply(tbl[is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_meta
#' @export
argo_meta_trans_system <- function(path, download = NULL, quiet = FALSE) {
  tbl <- argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_trans_system,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet
  )

  is_char <- vapply(tbl, is.character, logical(1))
  tbl[is_char] <- lapply(tbl[is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_meta
#' @export
argo_meta_launch_config_param <- function(path, download = NULL, quiet = FALSE) {
  tbl <- argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_launch_config_param,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet
  )

  is_char <- vapply(tbl, is.character, logical(1))
  tbl[is_char] <- lapply(tbl[is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_meta
#' @export
argo_meta_sensor <- function(path, download = NULL, quiet = FALSE) {
  tbl <- argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_sensor,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet
  )

  is_char <- vapply(tbl, is.character, logical(1))
  tbl[is_char] <- lapply(tbl[is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_meta
#' @export
argo_meta_param <- function(path, download = NULL, quiet = FALSE) {
  tbl <- argo_read_many(
    assert_argo_meta_file,
    argo_read_meta_param,
    path = path,
    vars = NULL,
    download = download,
    quiet = quiet
  )

  is_char <- vapply(tbl, is.character, logical(1))
  tbl[is_char] <- lapply(tbl[is_char], stringr::str_trim)
  tbl
}

#' @rdname argo_meta
#' @export
argo_read_meta_config_param <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_meta_read_config_param
  )
}

#' @rdname argo_meta
#' @export
argo_read_meta_missions <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_meta_read_missions
  )
}

#' @rdname argo_meta
#' @export
argo_read_meta_trans_system <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_meta_read_trans_system
  )
}

#' @rdname argo_meta
#' @export
argo_read_meta_launch_config_param <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_meta_read_launch_config_param
  )
}

#' @rdname argo_meta
#' @export
argo_read_meta_sensor <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_meta_read_sensor
  )
}

#' @rdname argo_meta
#' @export
argo_read_meta_param <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_meta_read_param
  )
}

assert_argo_meta_file <- function(path) {
  is_traj_file <- stringr::str_detect(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/\\1_meta\\.nc$"
  )

  if (any(!is_traj_file)) {
    bad_files <- path[!is_traj_file]
    paths <- if (length(bad_files) != 1) "paths" else "path"
    bad_files_label <- paste0(
      "'", utils::head(bad_files, 20), "'",
      collapse = "\n"
    )

    abort(
      glue(
        "Found { length(bad_files) } invalid Argo meta { paths }:\n{ bad_files_label}"
      )
    )
  }

  invisible(path)
}
