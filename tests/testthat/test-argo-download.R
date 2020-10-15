
test_that("argo_should_download() works for global files", {

  tmp_dir <- tempfile()

  with_argo_cache_dir(tmp_dir, {
    expect_identical(
      argo_should_download("some_file", max_global_cache_age = -Inf),
      TRUE
    )

    expect_identical(
      argo_should_download("some_file", max_global_cache_age = Inf),
      TRUE
    )

    file.create(argo_cached("some_file"))

    expect_identical(
      argo_should_download("some_file", max_global_cache_age = -Inf),
      TRUE
    )

    expect_identical(
      argo_should_download("some_file", max_global_cache_age = Inf),
      FALSE
    )

    Sys.sleep(0.6)
    expect_identical(
      argo_should_download("some_file", max_global_cache_age = 0.5 / 60 / 60),
      TRUE
    )
    expect_identical(
      argo_should_download("some_file", max_global_cache_age = 1),
      FALSE
    )
  })

  unlink(tmp_dir, recursive = TRUE)
})

test_that("argo_should_download() works for data files", {

  tmp_dir <- tempfile()

  with_argo_cache_dir(tmp_dir, {
    expect_identical(
      argo_should_download("dir/some_file", max_data_cache_age = -Inf),
      TRUE
    )

    expect_identical(
      argo_should_download("dir/some_file", max_data_cache_age = Inf),
      TRUE
    )

    dir.create(argo_cached("dir"))
    file.create(argo_cached("dir/some_file"))

    expect_identical(
      argo_should_download("dir/some_file", max_data_cache_age = -Inf),
      TRUE
    )

    expect_identical(
      argo_should_download("dir/some_file", max_data_cache_age = Inf),
      FALSE
    )

    Sys.sleep(0.6)
    expect_identical(
      argo_should_download("dir/some_file", max_data_cache_age = 0.5 / 60 / 60),
      TRUE
    )
    expect_identical(
      argo_should_download("dir/some_file", max_data_cache_age = 1),
      FALSE
    )
  })

  unlink(tmp_dir, recursive = TRUE)
})
