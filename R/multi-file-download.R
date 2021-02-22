
multi_file_download <- function(url, dest) {
  stopifnot(
    is.character(url),
    is.character(dest)
  )

  # recycle url along dest (could want to query one url many times)
  url <- rep_len(url, length(dest))

  if (length(url) == 0) {
    return(invisible(character(0)))
  }

  pb <- progress::progress_bar$new(
    "[:bar] :file",
    total = length(url)
  )
  pb$tick(0)

  # download the files!
  error_message <- vapply(seq_along(url), function(i) {
    pb$tick(tokens = list(file = basename(url[i])))

    if (!dir.exists(dirname(dest[i]))) {
      dir.create(dirname(dest[i]), recursive = TRUE, showWarnings = FALSE)
    }

    tryCatch({
      curl::curl_download(url[i], dest[i])
      NA_character_
    }, error = function(e) {
      # If this is a user interrupt, stop completely
      if (identical(e$message, "Operation was aborted by an application callback")) {
        abort(e$message) # nocov
      }

      paste0(e, collapse = "\n")
    })
  }, character(1))

  # check that all were downloaded
  has_error <- !is.na(error_message)
  if (any(has_error)) {
    missing_paths <- dest[has_error]
    missing_paths_lab <- paste0(
      "'", utils::head(missing_paths, 10), "': ", error_message[has_error],
      collapse = "\n"
    )
    files <- if (length(missing_paths) != 1) "files" else "file"
    abort(
      glue(
        "{ length(missing_paths) } { files } failed to download:\n{missing_paths_lab}"
      )
    )
  }

  invisible(dest)
}

multi_file_download_async <- function(url, dest) {
  stopifnot(
    is.character(url),
    is.character(dest)
  )

  # recycle url along dest (could want to query one url many times)
  url <- rep_len(url, length(dest))

  # create a mutable object that keeps track of success/failure
  results <- new.env(parent = emptyenv())

  if (length(url) == 0) {
    return(invisible(character(0)))
  }

  pool <- curl::new_pool(total_con = 6, host_con = 6)
  pb <- progress::progress_bar$new(
    "[:bar] :file",
    total = length(url)
  )
  pb$tick(0)
  key <- paste(url, dest)

  for (i in seq_along(url)) {
    results[[paste(url[i], dest[i])]] <- FALSE
    curl::curl_fetch_multi(
      url[i],
      multi_download_async_success(url[i], dest[i], results, pb),
      multi_download_async_failure(url[i], dest[i], results, pb),
      pool = pool
    )
  }

  # this will block as long as files are being downloaded
  curl::multi_run(pool = pool)

  n_error <- sum(!unlist(as.list(results)))

  if (n_error > 0) {
    files <- if (n_error != 1) "files" else "file"
    bad_urls <- paste0("'", url[!unlist(as.list(results)[key])], "'", collapse = "\n")
    abort(glue("{ n_error }/{ length(url) } { files } failed to download:\n{ bad_urls }"))
  }

  invisible(dest)
}

multi_download_async_success <- function(url, dest, results, pb) {
  force(url)
  force(dest)
  force(results)
  force(pb)

  function(res) {
    pb$tick(tokens = list(file = basename(url)))

    if (res$status_code >= 300) {
      results[[paste(url, dest)]] <- FALSE
      return()
    }

    if (!dir.exists(dirname(dest))) dir.create(dirname(dest), recursive = TRUE)
    con <- file(dest, "wb")
    on.exit(close(con))

    writeBin(res$content, con)
    results[[paste(url, dest)]] <- TRUE
  }
}

multi_download_async_failure <- function(url, dest, results, pb) {
  force(url)
  force(dest)
  force(results)
  force(pb)

  function(msg) {
    pb$tick(tokens = list(file = basename(url)))
    results[[paste(url, dest)]] <- FALSE
  }
}
