
#' @importFrom RNetCDF file.inq.nc open.nc close.nc var.get.nc dim.inq.nc var.inq.nc
nc_open <- function(path) {
  nc <- open.nc(path)
  attr(nc, "inq") <- file.inq.nc(nc)
  nc
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

warn_missing_dims <- function(file, dims) {
  missing <- paste0("'", dims$dim_name[!dims$has_dim], "'", collapse = " and ")
  dimensions <- if (sum(!dims$has_dim) != 1) "dimensions" else "dimension"
  warn_immediate(glue("'{ file }' is missing { dimensions }:\n{ missing }"))
}

sanitize_vars <- function(vars, file, dim_id) {
  if (!all(vars$has_var)) {
    missing <- paste0("'", vars$var_name[!vars$has_var], "'")
    variables <- if (sum(!vars$has_var) != 1) "variables" else "variable"
    warn_immediate(glue("'{ file }' is missing { variables }:\n{ missing }"))
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
    warn_immediate(
      glue(
        "'{ file }' is has { variables } with unexpected dimensions:\n{ missing }"
      )
    )
    vars <- lapply(vars, "[", expected_dim)
  }

  vars
}

argo_read_prof_levels2 <- function(file, vars = NULL) {
  nc <- nc_open(file)
  on.exit(close.nc(nc))

  dims <- nc_dims(nc, c("N_LEVELS", "N_PROF"))
  if (!all(dims$has_dim)) {
    warn_missing_dims(file, dims)
    return(NULL)
  }

  vars <- if (is.null(vars)) {
    nc_find_vars(nc, dims$dim_id)
  } else {
    sanitize_vars(nc_vars(nc, vars), file, dims$dim_id)
  }

  n_vars <- length(vars[[1]])
  if (n_vars == 0) {
    return(tibble::new_tibble(list(), nrow = 0))
  }

  values <- vector("list", n_vars)
  for (i in seq_len(n_vars)) {
    values[[i]] <- var.get.nc(nc, vars$var_id[i], rawchar = !vars$var_string[i])
  }
  names(values) <- vars$var_name

  is_raw <- vapply(values, is.raw, logical(1))
  values[is_raw] <- lapply(values[is_raw], rawToChar, multiple = TRUE)

  n_levels <- dims$dim_length[1]
  n_prof <- dims$dim_length[2]
  dim_values <- list(
    N_PROF = vctrs::vec_rep(seq_len(n_prof), n_levels),
    N_LEVELS = vctrs::vec_rep_each(seq_len(n_levels), n_prof)
  )

  tibble::new_tibble(c(dim_values, values), nrow = n_prof * n_levels)
}
