
#' Load NetCDF general information
#'
#' Use `argo_info()` to extract scalar variables and global attributes
#' from a vector of Argo NetCDF files. Use [argo_read_info()] to extract
#' variables from a previously-downloaded Argo NetCDF file.
#'
#' @inheritParams argo_vars
#'
#' @return A [tibble::tibble()] with one row per file. Columns containing
#'   global attribute information are prefixed with `att_` to differentiate
#'   them from variables with zero dimensions.
#' @export
#'
#' @examples
#' with_argo_example_cache({
#'   argo_info("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
argo_info <- function(path, download = NULL, quiet = NA) {
  path <- as_argo_path(path)
  assert_argo_nc_file(path)

  path_is_abs <- fs::is_absolute_path(path) & file.exists(path)
  cached <- path
  cached[!path_is_abs & !is.na(path)] <- argo_download(
    path[!path_is_abs & !is.na(path)],
    download = download,
    quiet = isTRUE(quiet)
  )

  # names should be of the 'file' version, which can be
  # joined with one of the global tables
  names(cached) <- stringr::str_remove(path, "^dac/")

  # drop NA filenames (e.g., failed aux downloads)
  cached <- cached[!is.na(cached)]

  if (!isTRUE(quiet)) {
    files_word <- if (length(cached) != 1) "files" else "file"
    title <- glue("Extracting from { length(cached) } { files_word }")
    message(title)
  }

  tbls <- argo_map(
    cached,
    argo_read_info,
    quiet = quiet
  )

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
#' Use `argo_read_info()` to extract variables and global attributes from
#' a previously-downloaded Argo NetCDF file. The variables read by
#' `argo_read_info()` are always length 1.
#'
#' @inheritParams argo_read_vars
#'
#' @return A [tibble::tibble()] with one row. Columns containing
#'   global attribute information are prefixed with `att_` to differentiate
#'   them from variables with zero dimensions.
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
argo_read_info <- function(file, quiet = FALSE) {
  scalar_string_values <- argo_nc_read_simple(
    file,
    dims = character(),
    quiet = quiet
  )

  nc <- nc_open(file)
  on.exit(nc_close(nc))

  # also read global attributes of length 1
  attrs <- vector("list", attr(nc, "inq")$ngatts)
  attr_names <- character(length(attrs))
  for (i in seq_along(attrs)) {
    attr_inq <- RNetCDF::att.inq.nc(nc, "NC_GLOBAL", i - 1L)
    attr_names[i] <- attr_inq$name
    attrs[[i]] <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", attr_inq$id)
  }

  if (length(attrs) > 0) {
    names(attrs) <- paste0("att_", attr_names)
  } else {
    names(attrs) <- character()
  }

  # this is a convenience function so don't bother with non-scalar attributes
  attrs <- attrs[vapply(attrs, length, integer(1)) == 1]

  vctrs::vec_cbind(
    scalar_string_values,
    new_tibble(attrs, nrow = 1L)
  )
}

assert_argo_nc_file <- function(path) {
  argo_assert_path_type(path, "^.+?\\.nc$", "NetCDF")
}
