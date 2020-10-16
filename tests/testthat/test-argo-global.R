
test_that("argo_global_meta() works", {
  skip_if_offline()

  tmp_cache <- tempfile()

  with_argo_cache_dir(tmp_cache, {
    expect_error(argo_global_meta(download = FALSE), "File does not exist")
    expect_message(argo_global_meta(download = NULL, quiet = FALSE), "ar_index")
    expect_silent(argo_global_meta(download = NULL, quiet = FALSE))
    expect_message(argo_global_meta(download = TRUE, quiet = FALSE), "ar_index")
  })

  unlink(tmp_cache, recursive = TRUE)
})

test_that("argo_global_prof() works", {
  skip("Long-running test")
  prof <- argo_global_prof()
  expect_is(prof, "tbl_df")
})
