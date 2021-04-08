
# These functions are wrappers around NetCDF reading that are highly
# optimized for Argo NetCDF files and highly optimized in general because
# these functions are called a *lot* when thousands of profiles are being
# read. Another important piece of these functions is the ability to noisily
# return NULL on a read error so that one file that is outside the assumptions
# of the read function doesn't cause all the others to stop reading.

#' @importFrom RNetCDF file.inq.nc open.nc close.nc var.get.nc dim.inq.nc var.inq.nc
nc_open <- function(path) {
  nc <- open.nc(path)
  attr(nc, "inq") <- file.inq.nc(nc)
  nc
}

nc_close <- function(nc) close.nc(nc)

new_tibble <- function(x, nrow) {
  structure(x, row.names = c(NA, as.integer(nrow)), class = c("tbl_df", "tbl", "data.frame"))
}

empty_tibble <- function() {
  x <- list()
  names(x) <- character(0)
  structure(x, row.names = c(NA, as.integer(nrow)), class = c("tbl_df", "tbl", "data.frame"))
}

nc_dims <- function(nc, dim_name) {
  n <- length(dim_name)
  dim_id <- integer(n)
  dim_length <- integer(n)
  has_dim <- rep(FALSE, n)

  nc_n <- attr(nc, "inq", exact = TRUE)$ndims
  n_done <- 0L
  for (nc_i in 0:(nc_n - 1)) {
    inq <- dim.inq.nc(nc, nc_i)
    i <- match(inq$name, dim_name, nomatch = -1L)
    if (i != -1L) {
      has_dim[i] <- TRUE
      dim_id[i] <- inq$id
      dim_length[i] <- inq$length
      n_done <- n_done + 1L
      if (n_done == n) break;
    }
  }

  list(
    dim_id = dim_id,
    dim_name = dim_name,
    has_dim = has_dim,
    dim_length = dim_length
  )
}

nc_vars <- function(nc, var_name) {
  n <- length(var_name)
  var_id <- integer(n)
  has_var <- rep(FALSE, n)
  var_dim_id <- vector("list", n)

  nc_n <- attr(nc, "inq", exact = TRUE)$nvars
  n_done <- 0L
  for (nc_i in 0:(nc_n - 1)) {
    inq <- var.inq.nc(nc, nc_i)
    i <- match(inq$name, var_name, nomatch = -1L)
    if (i != -1L) {
      has_var[i] <- TRUE
      var_id[i] <- inq$id
      var_dim_id[[i]] <- inq$dimids
      n_done <- n_done + 1L
      if (n_done == n) break;
    }
  }

  list(
    var_id = var_id,
    var_name = var_name,
    has_var = has_var,
    var_dim_id = var_dim_id
  )
}

nc_find_vars <- function(nc, dim_id) {
  nc_n <- attr(nc, "inq", exact = TRUE)$nvars
  var_name <- character(nc_n)
  var_string <- logical(nc_n)
  var_id <- integer(nc_n)
  var_dim_id <- vector("list", nc_n)

  n_done <- 0L
  for (nc_i in 0:(nc_n - 1)) {
    inq <- var.inq.nc(nc, nc_i)
    ids <- inq$dimids
    is_string <- (inq$type == "NC_CHAR") && identical(ids[-1], dim_id)

    if (identical(dim_id, ids) || is_string) {
      n_done <- n_done + 1L
      var_name[n_done] <- inq$name
      var_id[n_done] <- inq$id
      var_string[n_done] <- is_string
      var_dim_id[[n_done]] <- ids
    }
  }

  ind <- seq_len(n_done)
  list(
    var_id = var_id[ind],
    var_name = var_name[ind],
    var_string = var_string[ind],
    var_dim_id = var_dim_id[ind]
  )
}


warn_or_stop_read_error <- function(msg, quiet = FALSE) {
  if (identical(quiet, FALSE)) {
    abort(msg)
  } else if (identical(quiet, NA)) {
    warning(msg, immediate. = TRUE, call. = FALSE)
  } else {
    # do nothing
  }
}

warn_or_stop_missing_dims <- function(file, dims, quiet = FALSE) {
  missing <- paste0("'", dims$dim_name[!dims$has_dim], "'", collapse = " and ")
  dimensions <- if (sum(!dims$has_dim) != 1) "dimensions" else "dimension"
  warn_or_stop_read_error(
    glue("'{ file }' is missing { dimensions }:\n{ missing }"),
    quiet = quiet
  )
}

warn_or_stop_var_unexpected_length <- function(file, var_name, len, expected_len,
                                               quiet = FALSE) {
  warn_or_stop_read_error(
    glue(
      "'{ file }' variable '{ var_name }' has unexpected length { len } (expected { expected_len })"
    ),
    quiet = quiet
  )
}

sanitize_or_stop_vars <- function(vars, file, dim_id, quiet = FALSE) {
  if (!all(vars$has_var)) {
    missing <- paste0("'", vars$var_name[!vars$has_var], "'")
    variables <- if (sum(!vars$has_var) != 1) "variables" else "variable"
    warn_or_stop_read_error(
      glue("'{ file }' is missing { variables }:\n{ missing }"),
      quiet = quiet
    )
    vars <- lapply(vars, "[", vars$has_var)
  }

  n_vars <- length(vars$var_dim_id)
  expected_dim <- logical(n_vars)
  var_string <- logical(n_vars)
  for (i in seq_along(vars$var_dim_id)) {
    ids <- vars$var_dim_id[[i]]
    # no access to type here but maybe there should be
    var_string[i] <- identical(ids[-1], dim_id)
    expected_dim[i] <- identical(dim_id, ids) || var_string[i]
  }

  vars$var_string <- var_string

  if (!all(expected_dim)) {
    missing <- paste0("'", vars$var_name[!expected_dim], "'")
    variables <- if (sum(!expected_dim) != 1) "variables" else "variable"
    warn_or_stop_read_error(
      glue(
        "'{ file }' is has { variables } with unexpected dimensions:\n{ missing }"
      ),
      quiet = quiet
    )
    vars <- lapply(vars, "[", expected_dim)
  }

  vars
}

nc_read_vars <- function(nc, vars) {
  n_vars <- length(vars[[1]])
  values <- vector("list", n_vars)

  for (i in seq_len(n_vars)) {
    value <- var.get.nc(
      nc, vars$var_id[i],
      na.mode = 0L,
      rawchar = !vars$var_string[i]
    )
    dim(value) <- NULL

    if (is.raw(value)) {
      value <- rawToChar(value, multiple = TRUE)
    }
    values[[i]] <- value
  }

  names(values) <- vars$var_name
  values
}

argo_read_prof_levels2 <- function(file, vars = NULL, quiet = FALSE) {
  nc <- nc_open(file)
  on.exit(nc_close(nc))

  dims <- nc_dims(nc, c("N_LEVELS", "N_PROF"))
  if (!all(dims$has_dim)) {
    warn_or_stop_missing_dims(file, dims, quiet = quiet)
    return(NULL)
  }

  vars <- if (is.null(vars)) {
    nc_find_vars(nc, dims$dim_id)
  } else {
    sanitize_or_stop_vars(nc_vars(nc, vars), file, dims$dim_id, quiet = quiet)
  }

  n_levels <- dims$dim_length[1]
  n_prof <- dims$dim_length[2]
  n <- n_levels * n_prof

  dim_values <- list(
    N_LEVELS = vctrs::vec_rep(seq_len(n_levels), n_prof),
    N_PROF = vctrs::vec_rep_each(seq_len(n_prof), n_levels)
  )

  values <- nc_read_vars(nc, vars)

  new_tibble(c(dim_values, values), nrow = n)
}
