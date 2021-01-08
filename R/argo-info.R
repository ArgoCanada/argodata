
#' Load NetCDF general information
#'
#' @inheritParams argo_vars
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' with_argo_example_cache({
#'   argo_info("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
argo_info <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  path <- as_argo_path(path)
  assert_argo_nc_file(path)

  cached <- argo_download(path, download = download, quiet = quiet)

  # names should be of the 'file' version, which can be
  # joined with one of the global tables
  names(cached) <- stringr::str_remove(path, "^dac/")

  files_word <- if (length(cached) != 1) "files" else "file"
  with_argo_progress({
    tbls <- argo_map(
      cached,
      argo_read_info,
      vars = if(is.null(vars)) NULL else toupper(vars)
    )
  }, quiet = quiet, title = glue("Extracting from { length(cached) } { files_word }"))

  tbl <- vctrs::vec_rbind(!!! tbls, .names_to = "file")
  names(tbl) <- tolower(names(tbl))

  date_vars <- c(
    "reference_date_time", "date_creation", "date_update",
    "launch_date", "start_date", "startup_date",
    "end_mission_date"
  )

  var_is_datetime <- names(tbl) %in% date_vars
  tbl[var_is_datetime] <- lapply(
    tbl[var_is_datetime],
    strptime,
    format = "%Y%m%d%H%M%S",
    tz = "UTC"
  )

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)

  tbl
}

#' Read NetCDF general information
#'
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_info(prof_file)
#'
argo_read_info <- function(file, vars = NULL) {
  with_argo_nc_file(file, argo_nc_read_info, vars = vars)
}


#' Read NetCDF general information from 'ncdf4' objects
#'
#' @inheritParams argo_nc_read_vars
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#'
#' nc_prof <- ncdf4::nc_open(prof_file)
#' argo_nc_read_info(nc_prof)
#' ncdf4::nc_close(nc_prof)
#'
argo_nc_read_info <- function(nc, vars = NULL) {
  # some global metadata is stored as variables along a string dimension
  all_vars <- argo_nc_read_vars(nc)
  all_vars <- all_vars[all_vars$ndims == 1, ]
  all_vars$dim <- unlist(all_vars$dim)
  vars_tbl <- all_vars[stringr::str_detect(all_vars$dim, "^(STRING[0-9]+|DATE_TIME)$"), ]

  # other global metadata is stored as global attributes
  # to avoid name collisions that could result from making variable names
  # lowercase, namespace these as att_{ name }
  attrs <- ncdf4::ncatt_get(nc, 0)
  if (length(attrs) > 0) {
    names(attrs) <- stringr::str_c("att_", names(attrs))
  }

  all_names <- c(vars_tbl$name, names(attrs))
  var_names <- if (!is.null(vars)) intersect_any_case(all_names, vars) else all_names
  values <- argo_nc_values(nc, intersect(vars_tbl$name, var_names))
  attrs <- attrs[intersect(names(attrs), var_names)]
  tibble::new_tibble(c(values, attrs), nrow = 1L)
}

assert_argo_nc_file <- function(path) {
  argo_assert_path_type(path, "^dac/[a-z]+/([0-9a-zA-Z]+)/.*?\\.nc$", "NetCDF")
}

intersect_any_case <- function(x, y) {
  x_upper <- toupper(x)
  y_upper <- toupper(y)
  intersect_upper <- intersect(x_upper, y_upper)
  result <- c(x[x_upper %in% intersect_upper], y[y_upper %in% intersect_upper])
  result[!duplicated(toupper(result))]
}
