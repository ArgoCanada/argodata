
test_that("argo cache can be set and fetched", {
  with_argo_mirror(NULL, {
    expect_identical(argo_cache_dir(), argo_tmp_dir)
    expect_identical(getOption("argodata.cache_dir"), NULL)

    new_tmp <- tempfile()
    with_argo_cache_dir(new_tmp, {
      expect_identical(
        fs::path_abs(argo_cache_dir()),
        fs::path_abs(new_tmp)
      )
    })
    unlink(new_tmp, recursive = TRUE)
  })
})

test_that("argo_set_cache_dir() errors for invalid values", {
  expect_error(argo_set_cache_dir(8L), "must be a character vector")
  tmp_file <- tempfile()
  file.create(tmp_file)
  expect_error(argo_set_cache_dir(tmp_file), "could not be created")
})
