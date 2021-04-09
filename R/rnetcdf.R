
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

# In read functions we don't ever have to list all dimensions,
# but we do have to verify that some exist and get their ids to check/find
# variables along them. We also need a list of the string dimensions
# (names specific to Argo) to properly identify when we want to return
# a vector of single characters (e.g., _QC columns) or a true string.
nc_dims <- function(nc, dim_name) {
  n <- length(dim_name)
  dim_id <- integer(n)
  dim_length <- integer(n)
  has_dim <- rep(FALSE, n)

  nc_n <- attr(nc, "inq", exact = TRUE)$ndims
  string_dim_id <- integer(nc_n)
  n_string <- 0L
  for (nc_i in 0:(nc_n - 1)) {
    inq <- dim.inq.nc(nc, nc_i)
    i <- match(inq$name, dim_name, nomatch = -1L)
    if (i != -1L) {
      has_dim[i] <- TRUE
      dim_id[i] <- inq$id
      dim_length[i] <- inq$length
    }

    if (startsWith(inq$name, "STRING") || (inq$name == "DATE_TIME")) {
      n_string <- n_string + 1L
      string_dim_id[n_string] <- inq$id
    }
  }

  list(
    dim_id = dim_id,
    dim_name = dim_name,
    has_dim = has_dim,
    dim_length = dim_length,
    string_dim_id = string_dim_id[seq_len(n_string)]
  )
}

# Lists only variables found in `var_name`. These are always
# checked by sanitize_or_stop_vars() for the correct dimensionality.
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

# Finds variables with the requested dimension family (taking into account
# string variables).
nc_find_vars <- function(nc, dim_id, string_dim_ids) {
  nc_n <- attr(nc, "inq", exact = TRUE)$nvars
  var_name <- character(nc_n)
  var_string <- logical(nc_n)
  var_id <- integer(nc_n)
  var_dim_id <- vector("list", nc_n)

  n_done <- 0L
  for (nc_i in 0:(nc_n - 1)) {
    inq <- var.inq.nc(nc, nc_i)
    ids <- inq$dimids
    is_string <- identical(ids[-1], dim_id) && (ids[1] %in% string_dim_ids)

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

# when length(dim_id) == 0, the above doesn't work for non-string variables
nc_find_vars0 <- function(nc, string_dim_ids) {
  nc_n <- attr(nc, "inq", exact = TRUE)$nvars
  var_name <- character(nc_n)
  var_string <- logical(nc_n)
  var_id <- integer(nc_n)
  var_dim_id <- vector("list", nc_n)

  n_done <- 0L
  for (nc_i in 0:(nc_n - 1)) {
    inq <- var.inq.nc(nc, nc_i)
    ids <- inq$dimids
    is_string <- identical(ids[-1], integer()) && (ids[1] %in% string_dim_ids)

    if ((inq$ndims == 0) || is_string) {
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

# Base reporter that either stops (probably what you want for a single read),
# warns (probably what you want for a bulk read the first time) or drops
# (probably what you want for a bulk read in a script).
warn_or_stop_read_error <- function(msg, quiet = FALSE) {
  if (identical(quiet, FALSE)) {
    abort(msg)
  } else if (identical(quiet, NA)) {
    warning(msg, immediate. = TRUE, call. = FALSE)
  } else {
    # do nothing
  }
}

# Some types of files just don't have some dimensions (meta files is where
# this was first encountered). It's helpful to just warn for these and
# return NULL in a bulk read.
warn_or_stop_missing_dims <- function(file, dims, quiet = FALSE) {
  missing <- paste0("'", dims$dim_name[!dims$has_dim], "'", collapse = " and ")
  dimensions <- if (sum(!dims$has_dim) != 1) "dimensions" else "dimension"
  warn_or_stop_read_error(
    glue("'{ file }' is missing { dimensions }:\n{ missing }"),
    quiet = quiet
  )
}

# There's no guarantee that user-supplied variables will be along the
# dimensions of the read or are available in the file. Warning when these
# are absent is helpful in bulk reads.
sanitize_or_stop_vars <- function(vars, file, dim_id, string_dim_ids, quiet = FALSE) {
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
    var_string[i] <- identical(ids[-1], dim_id) && (ids[1] %in% string_dim_ids)
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

# By default vars is NULL, which means we can use `nc_find_vars()`
# and skip sanitizing. When the user supplies vars, we might need to
# remove problematic ones.
nc_resolve_vars <- function(nc, vars, dims, file = "", quiet = FALSE) {
  n_dim <- length(dims[[1]])
  if (is.null(vars) && (n_dim > 0)) {
    nc_find_vars(nc, dims$dim_id, dims$string_dim_id)
  } else if(is.null(vars)) {
    nc_find_vars0(nc, dims$string_dim_id)
  } else {
    sanitize_or_stop_vars(
      nc_vars(nc, vars),
      file,
      dims$dim_id,
      dims$string_dim_id,
      quiet = quiet
    )
  }
}

# Wrapper around var.get.nc() that takes into account the string-ness
# of the variable. It would also be possible to check the resulting
# lengths here but the dimension checking appears to work for this so far.
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

# These are just faster wrappers around expand.grid(), which unnecessarily
# creates a data.frame() and whose speed can be improved upon with the
# simple but most common cases of one, two, and three-variable expansions
#' @importFrom vctrs vec_rep vec_rep_each
nc_read_dims <- function(nc, dims) {
  lengths <- dims$dim_length
  n_dims <- length(lengths)

  dim_values <- if (n_dims == 1L) {
    list(seq_len(lengths))
  } else if (n_dims == 2L) {
    n1 <- lengths[1]
    n2 <- lengths[2]
    list(
      vec_rep(seq_len(n1), n2),
      vec_rep_each(seq_len(n2), n1)
    )
  } else if (n_dims == 3L) {
    n1 <- lengths[1]
    n2 <- lengths[2]
    n3 <- lengths[3]
    list(
      vec_rep(seq_len(n1), n2 * n3),
      vec_rep(vec_rep_each(seq_len(n2), n1), n3),
      vec_rep_each(seq_len(n3), n1 * n2)
    )
  } else {
    unclass(unname(do.call(expand.grid, lapply(lengths, seq_len))))
  }

  names(dim_values) <- dims$dim_name
  dim_values
}

# Most Argo tables can be read using this function, varying `dims`.
argo_nc_read_simple <- function(file, dims, vars = NULL, quiet = FALSE) {
  nc <- nc_open(file)
  on.exit(nc_close(nc))

  dims <- nc_dims(nc, dims)
  if (!all(dims$has_dim)) {
    warn_or_stop_missing_dims(file, dims, quiet = quiet)
    return(NULL)
  }

  vars <- nc_resolve_vars(nc, vars, dims, file = file, quiet = quiet)
  dim_values <- nc_read_dims(nc, dims)
  values <- nc_read_vars(nc, vars)
  new_tibble(c(dim_values, values), nrow = prod(dims$dim_length))
}
