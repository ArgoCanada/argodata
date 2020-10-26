
#' Load Argo trajectories
#'
#' @param file A previously downloaded Argo trajectory NetCDF file.
#' @inheritParams argo_prof
#'
#' @return A [tibble::tibble()].
#' @export
#'
argo_traj <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  path <- as_argo_path(path)
  assert_argo_traj_file(path)
  cached <- argo_download(path, download = download, quiet = quiet)

  tbls <- lapply(
    cached,
    argo_read_traj,
    vars = if (!is.null(vars)) toupper(vars) else vars
  )

  tbl <- vctrs::vec_rbind(!!! tbls)

  # make names lowercase
  names(tbl) <- tolower(names(tbl))

  # calculate datetime from juld and rename to `date`
  if ("juld" %in% colnames(tbl)) {
    tbl$juld <- as.POSIXct("1950-01-01 00:00:00 UTC", tz = "UTC") +
      as.difftime(tbl$juld, units = "days")
  }

  if ("juld_adjusted" %in% colnames(tbl)) {
    tbl$juld_adjusted <- as.POSIXct("1950-01-01 00:00:00 UTC", tz = "UTC") +
      as.difftime(tbl$juld_adjusted, units = "days")
  }

  names(tbl) <- stringr::str_replace(names(tbl), "^juld", "date")
  tbl
}

#' @rdname argo_traj
#' @export
argo_read_traj <- function(file, vars = NULL) {
  nc <- ncdf4::nc_open(file, suppress_dimvals = TRUE)
  on.exit(ncdf4::nc_close(nc))
  argo_nc_traj_read(nc, vars = vars)
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
