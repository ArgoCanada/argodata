
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

test_that("argo_download() works", {
  skip_if_offline()
  skip_if_not(Sys.getenv("R_ARGO_CACHE_DIR") != "")

  # with async = FALSE
  tmp_cache <- tempfile()
  expect_false(file.exists(file.path(tmp_cache, "ar_index_global_meta.txt.gz")))
  expect_identical(
    fs::path_abs(with_argo_cache_dir(tmp_cache, argo_download("ar_index_global_meta.txt.gz", async = FALSE, quiet = TRUE))),
    fs::path_abs(file.path(tmp_cache, "ar_index_global_meta.txt.gz"))
  )
  expect_true(file.exists(file.path(tmp_cache, "ar_index_global_meta.txt.gz")))

  # with async = TRUE
  unlink(file.path(tmp_cache, "ar_index_global_meta.txt.gz"))
  expect_false(file.exists(file.path(tmp_cache, "ar_index_global_meta.txt.gz")))
  expect_identical(
    fs::path_abs(with_argo_cache_dir(tmp_cache, argo_download("ar_index_global_meta.txt.gz", async = TRUE, quiet = TRUE))),
    fs::path_abs(file.path(tmp_cache, "ar_index_global_meta.txt.gz"))
  )
  expect_true(file.exists(file.path(tmp_cache, "ar_index_global_meta.txt.gz")))

  expect_identical(
    fs::path_abs(with_argo_cache_dir(tmp_cache, argo_download("ar_index_global_meta.txt.gz", quiet = TRUE))),
    fs::path_abs(file.path(tmp_cache, "ar_index_global_meta.txt.gz"))
  )

  unlink(tmp_cache, recursive = TRUE)
})

test_that("argu_download_aux() works", {
  skip_if_offline()
  skip_if_not(Sys.getenv("R_ARGO_CACHE_DIR") != "")

  paths <- c(
    "dac/meds/4902533/profiles/R4902533_001.nc",
    "dac/meds/4900632/profiles/D4900632_003.nc"
  )

  aux <- argo_download_aux(paths, quiet = TRUE)
  expect_identical(
    aux,
    c(argo_cached(as_argo_path_aux(paths[1])), NA_character_)
  )

  expect_identical(
    argo_download_aux("aux/meds/4902533/profiles/R4902533_001_aux.nc"),
    argo_cached("aux/meds/4902533/profiles/R4902533_001_aux.nc")
  )
})
