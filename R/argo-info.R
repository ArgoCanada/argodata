
#' Read NetCDF general information
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
#' prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_info(prof_file)
#'
#' nc_prof <- ncdf4::nc_open(prof_file)
#' argo_nc_read_info(nc_prof)
#' ncdf4::nc_close(nc_prof)
#'
argo_info <- function(path, vars = NULL, download = NULL, quiet = FALSE) {
  path <- as_argo_path(path)
  assert_argo_nc_file(path)

  cached <- argo_download(path, download = download, quiet = quiet)

  tbls <- lapply(
    cached,
    argo_read_info,
    vars = if(is.null(vars)) NULL else toupper(vars)
  )

  tbl <- vctrs::vec_rbind(!!! tbls)
  names(tbl) <- tolower(names(tbl))

  var_is_datetime <- names(tbl) %in% c("reference_date_time", "date_creation", "date_update")
  tbl[var_is_datetime] <- lapply(tbl[var_is_datetime], readr::parse_datetime, format = "%Y%m%d%H%M%S")

  val_is_char <- vapply(tbl, is.character, logical(1))
  tbl[val_is_char] <- lapply(tbl[val_is_char], stringr::str_trim)

  tbl
}

#' @rdname argo_info
#' @export
argo_read_info <- function(file, vars = NULL) {
  with_argo_nc_file(file, argo_nc_read_info, vars = vars)
}

#' @rdname argo_info
#' @export
argo_nc_read_info <- function(nc, vars = NULL) {
  all_vars <- argo_nc_read_vars(nc)
  all_vars <- all_vars[all_vars$ndims == 1, ]
  all_vars$dim <- unlist(all_vars$dim)
  vars_tbl <- all_vars[stringr::str_detect(all_vars$dim, "^(STRING[0-9]+|DATE_TIME)$"), ]
  var_names <- if (!is.null(vars)) intersect(vars, vars_tbl$name) else vars_tbl$name
  values <- argo_nc_values(nc, var_names)
  argo_nc_new_tibble(nc, values, nrow = 1L)
}

assert_argo_nc_file <- function(path) {
  is_prof_file <- stringr::str_detect(
    path,
    "^dac/[a-z]+/([0-9a-zA-Z]+)/.*?\\.nc$"
  )

  if (any(!is_prof_file)) {
    bad_files <- path[!is_prof_file]
    paths <- if (length(bad_files) != 1) "paths" else "path"
    bad_files_label <- paste0(
      "'", utils::head(bad_files, 20), "'",
      collapse = "\n"
    )

    abort(
      glue(
        "Found { length(bad_files) } invalid Argo NetCDF { paths }:\n{ bad_files_label}"
      )
    )
  }

  invisible(path)
}

