
#' Load Argo trajectories
#'
#' @param file A previously downloaded Argo trajectory NetCDF file.
#' @inheritParams argo_prof_levels
#'
#' @return A [tibble::tibble()].
#' @export
#' @rdname argo_traj
#'
#' @examples
#' with_argo_example_cache({
#'   argo_traj_meas("dac/csio/2900313/2900313_Rtraj.nc")
#' })
#'
#' with_argo_example_cache({
#'   argo_traj_cycle("dac/csio/2900313/2900313_Rtraj.nc")
#' })
#'
#' traj_file <- system.file(
#'   "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_traj_meas(traj_file)
#' argo_read_traj_cycle(traj_file)
#'
argo_traj_meas <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  argo_read_many(
    assert_argo_traj_file,
    argo_read_traj_meas,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_traj
#' @export
argo_traj_cycle <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  argo_read_many(
    assert_argo_traj_file,
    argo_read_traj_cycle,
    path = path,
    vars = vars,
    download = download,
    quiet = quiet
  )
}

#' @rdname argo_traj
#' @export
argo_read_traj_meas <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_traj_read_meas,
    vars = vars
  )
}

#' @rdname argo_traj
#' @export
argo_read_traj_cycle <- function(file, vars = NULL) {
  with_argo_nc_file(
    file,
    argo_nc_traj_read_cycle,
    vars = vars
  )
}

assert_argo_traj_file <- function(path) {
  is_traj_file <- stringr::str_detect(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/\\1_(R|D)traj\\.nc$"
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
        "Found { length(bad_files) } invalid Argo trajectory { paths }:\n{ bad_files_label}"
      )
    )
  }

  invisible(path)
}
