
#' Load Argo NetCDF variable metadata
#'
#' @inheritParams argo_download
#' @inheritParams argo_nc_read_vars
#'
#' @return A [tibble::tibble()] with one row per variable and columns `name`,
#'   `longname`, `units`, `prec`, `ndims`, `size`, and `dim`. Columns
#'   `size` and `dim` can be unnested (e.g., using [tidyr::unnest()])
#'   to produce a tibble with one row per variable per dimension.
#' @export
#'
#' @examples
#' with_argo_example_cache({
#'   argo_vars("dac/csio/2900313/profiles/D2900313_000.nc")
#' })
#'
argo_vars <- function(path, download = NULL, quiet = NA) {
  path <- as_argo_path(path)
  cached <- argo_download(path, download = download, quiet = isTRUE(quiet))
  names(cached) <- stringr::str_remove(path, "^dac/")

  if (!isTRUE(quiet)) {
    files_word <- if (length(cached) != 1) "files" else "file"
    title <- glue("Extracting from { length(cached) } { files_word }")
    message(title)
  }

  tbls <- argo_map(
    cached,
    argo_read_vars,
    quiet = quiet
  )

  tbl <- vctrs::vec_rbind(!!! tbls, .names_to = "file")
  names(tbl) <- tolower(names(tbl))
  tbl
}


#' Read NetCDF variable metadata
#'
#' @param file A previously downloaded Argo NetCDF file.
#' @param quiet Use `TRUE` to stop for malformed files, `NA` to
#'   silently warn for malformed files (will return `NULL`), or
#'   `FALSE` to return `NULL` silently when a read error is encountered.
#' @param vars A vector of variable names to include. The ordering
#'   of the variables is not guaranteed, and variables that do not
#'   exist are ignored. For `nc_read_*()` and `read_*()` variants,
#'   these are the raw variable names (all caps, without "date"
#'   substituted for "juld").
#'
#' @return A [tibble::tibble()] with one row per variable and columns `name`,
#'   `longname`, `units`, `prec`, `ndims`, `size`, and `dim`. Columns
#'   `size` and `dim` can be unnested (e.g., using [tidyr::unnest()])
#'   to produce a tibble with one row per variable per dimension.
#' @export
#'
#' @examples
#' prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#'
#' argo_read_vars(prof_file)
#'
argo_read_vars <- function(file, vars = NULL, quiet = FALSE) {
  nc <- nc_open(file)
  on.exit(nc_close(nc))

  vars <- nc_list_vars(nc)

  # this is just for convenience so drop attributes with length != 1
  # _FillValue will have a different type between variables, so needs
  # a list() column
  attrs <- lapply(vars$attrs, function(x) {
    x[names(x) == "_FillValue"] <- lapply(x[names(x) == "_FillValue"], list)
    x <- x[vapply(x, length, integer(1)) == 1L]
    names(x) <- paste0("att_", names(x))
    new_tibble(x, nrow = 1L)
  })

  dims_all <- nc_list_dims(nc)
  dims <- lapply(vars$dim_id, function(x) {
    i <- match(x, dims_all$id)
    new_tibble(
      list(dim = list(dims_all$name[i]), size = list(dims_all$length[i])),
      nrow = 1L
    )
  })

  vctrs::vec_cbind(
    new_tibble(vars["name"], nrow = length(dims)),
    vctrs::vec_rbind(!!! dims),
    vctrs::vec_rbind(!!! attrs)
  )
}

nc_list_dims <- function(nc) {
  nc_n <- attr(nc, "inq", exact = TRUE)$ndims
  id <- integer(nc_n)
  name <- character(nc_n)
  length <- integer(nc_n)

  for (i in seq_len(nc_n)) {
    inq <- dim.inq.nc(nc, i - 1L)
    id[i] <- inq$id
    name[i] <- inq$name
    length[i] <- inq$length
  }

  list(
    id = id,
    name = name,
    length = length
  )
}

nc_list_vars <- function(nc) {
  nc_n <- attr(nc, "inq", exact = TRUE)$nvars

  name <- character(nc_n)
  id <- integer(nc_n)
  dim_id <- vector("list", nc_n)
  attrs <- vector("list", nc_n)

  for (i in seq_len(nc_n)) {
    inq <- var.inq.nc(nc, i - 1L)

    name[i] <- inq$name
    id[i] <- inq$id
    dim_id[[i]] <- inq$dimids

    attrs[[i]] <- vector("list", inq$natts)
    attr_names <- character(inq$natts)
    for (j in seq_len(inq$natts)) {
      att_inq <- RNetCDF::att.inq.nc(nc, inq$id, j - 1L)
      attrs[[i]][[j]] <- RNetCDF::att.get.nc(nc, inq$id, j - 1L)
      attr_names[j] <- att_inq$name
    }
    names(attrs[[i]]) <- attr_names
  }

  list(
    name = name,
    id = id,
    dim_id = dim_id,
    attrs = attrs
  )
}



#' Read NetCDF variable metadata from a 'ncdf4' object
#'
#' @param nc A handle created using [ncdf4::nc_open()].
#' @param vars A vector of variable names to include. The ordering
#'   of the variables is not guaranteed, and variables that do not
#'   exist are ignored. For `nc_read_*()` and `read_*()` variants,
#'   these are the raw variable names (all caps, without "date"
#'   substituted for "juld").
#'
#' @return A [tibble::tibble()] with one row per variable and columns `name`,
#'   `longname`, `units`, `prec`, `ndims`, `size`, and `dim`. Columns
#'   `size` and `dim` can be unnested (e.g., using [tidyr::unnest()])
#'   to produce a tibble with one row per variable per dimension.
#' @export
#'
#' @examples
#' prof_file <- system.file(
#'   "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
#'   package = "argodata"
#' )
#'
#' nc_prof <- ncdf4::nc_open(prof_file)
#' argo_nc_read_vars(nc_prof)
#' ncdf4::nc_close(nc_prof)
#'
argo_nc_read_vars <- function(nc, vars = NULL) {
  vars <- if (is.null(vars)) names(nc$var) else intersect(vars, names(nc$var))
  ncdf4_vars <- lapply(unclass(nc$var[vars]), argo_nc_var_as_tbl)
  attrs <- lapply(unclass(nc$var[vars]), argo_nc_var_attr_as_tbl, nc)

  tbl <- vctrs::vec_rbind(!!! ncdf4_vars)
  tbl_attrs <- vctrs::vec_rbind(!!! attrs)

  # return an edited version of the raw values:
  # only include a few items, replace dim with the dimension names,
  # and trim whitespace
  tbl <- tbl[c("name", "longname", "units", "prec", "ndims", "size", "dim")]
  tbl$dim <- lapply(tbl$dim, function(x) vapply(x, "[[", "name", FUN.VALUE = character(1)))
  tbl_is_char <- vapply(tbl, is.character, logical(1))
  tbl[tbl_is_char] <- lapply(tbl[tbl_is_char], stringr::str_trim)

  vctrs::vec_cbind(tbl, tbl_attrs)
}

argo_nc_var_as_tbl <- function(var) {
  var$id <- NULL

  list_cols <- c("size", "dimids", "dim", "varsize", "dims", "missval")
  var_is_scalar <- !(names(var) %in% list_cols)
  var[!var_is_scalar] <- lapply(var[!var_is_scalar], list)

  tibble::new_tibble(lapply(var, unclass), nrow = 1)
}

argo_nc_var_attr_as_tbl <- function(var, nc) {
  attrs <- ncdf4::ncatt_get(nc, var)
  # this doesn't work with variables of multiple types and is covered by
  # missval in the output
  attrs[["_FillValue"]] <- NULL
  if (length(attrs) > 0) {
    names(attrs) <- stringr::str_c("att_", names(attrs))
  }

  tibble::new_tibble(attrs, nrow = 1)
}
