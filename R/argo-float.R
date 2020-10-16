
as_argo_float <- function(float, download = NULL, quiet = TRUE) {
  if (is.data.frame(float) && ("file" %in% names(float))) {
    float <- float[["file"]]
  }

  if (!is.character(float)) {
    abort(
      "`float` must be a character vector or data frame with a character vector column named 'file'."
    )
  }

  float_na <- is.na(float)

  # a path will start with "dac/"
  float_final <- stringr::str_remove(
    stringr::str_extract(float, "dac/[a-z]+/[0-9a-z]+"),
    "^dac/"
  )

  # a file will start with data processing center/float id
  float_final[is.na(float_final) & !float_na] <- stringr::str_extract(
    float[is.na(float_final) & !float_na],
    "[a-z]+/[0-9a-z]+"
  )

  # a raw float ID is unique and can be looked up
  if (any(is.na(float_final) & !float_na)) {
    meta <- argo_global_meta(download = download, quiet = quiet)
    meta_float <- as_argo_float(meta$file)
    meta_float_id <- stringr::str_remove(meta_float, "^[a-z]+/")
    float_final[is.na(float_final) & !float_na] <-
      meta_float[match(float[is.na(float_final) & !float_na], meta_float_id)]
  }

  # check float IDs
  if (any(is.na(float_final) & !float_na)) {
    bad_float_ids <- unique(float[is.na(float_final) & !float_na])
    bad_float_label <- paste0(
      "'", utils::head(bad_float_ids, 20), "'",
      collapse = "\n"
    )
    IDs <- if (length(bad_float_ids) != 1) "IDs" else "ID"
    abort(glue("Found { length( bad_float_ids ) } invalid float { IDs }:\n{ bad_float_label }"))
  }

  float_final
}
