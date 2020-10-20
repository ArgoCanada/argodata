
test_that("argo_global_meta() works", {
  tmp_cache <- tempfile()

  with_argo_cache_dir(tmp_cache, {
    with_argo_mirror(argo_test_mirror(), {
      expect_error(argo_global_meta(download = FALSE), "does not exist")
      expect_message(argo_global_meta(download = NULL, quiet = FALSE), "ar_index")
      expect_silent(argo_global_meta(download = NULL, quiet = FALSE))
      expect_message(argo_global_meta(download = TRUE, quiet = FALSE), "ar_index")
      expect_is(argo_global_meta(), "tbl_df")
      expect_identical(
        names(argo_global_meta()),
        c("file", "profiler_type", "institution", "date_update")
      )
    })
  })

  unlink(tmp_cache)
})

test_that("argo_global_prof() works", {
  # basically, only during devtools::test()
  skip_if_not(file.exists("../../inst/cache-test/ar_index_global_prof.txt.gz"))

  tmp_cache <- tempfile()

  with_argo_cache_dir(tmp_cache, {
    with_argo_mirror("../../inst/cache-test", {
      expect_is(argo_global_prof(), "tbl_df")
      expect_identical(
        names(argo_global_prof()),
        c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
          "institution", "date_update")
      )
    })
  })

  unlink(tmp_cache)
})

test_that("argo_global_tech() works", {
  tmp_cache <- tempfile()

  with_argo_cache_dir(tmp_cache, {
    with_argo_mirror(argo_test_mirror(), {
      expect_error(argo_global_tech(download = FALSE), "does not exist")
      expect_message(argo_global_tech(download = NULL, quiet = FALSE), "ar_index")
      expect_silent(argo_global_tech(download = NULL, quiet = FALSE))
      expect_message(argo_global_tech(download = TRUE, quiet = FALSE), "ar_index")
      expect_is(argo_global_tech(), "tbl_df")
      expect_identical(
        names(argo_global_tech()),
        c("file", "institution", "date_update")
      )
    })
  })

  unlink(tmp_cache)
})

test_that("argo_global_traj() works", {
  tmp_cache <- tempfile()

  with_argo_cache_dir(tmp_cache, {
    with_argo_mirror(argo_test_mirror(), {
      expect_error(argo_global_traj(download = FALSE), "does not exist")
      expect_message(argo_global_traj(download = NULL, quiet = FALSE), "ar_index")
      expect_silent(argo_global_traj(download = NULL, quiet = FALSE))
      expect_message(argo_global_traj(download = TRUE, quiet = FALSE), "ar_index")
      expect_is(argo_global_traj(), "tbl_df")
      expect_identical(
        names(argo_global_traj()),
        c("file", "latitude_max", "latitude_min", "longitude_max", "longitude_min",
          "profiler_type", "institution", "date_update")
      )
    })
  })

  unlink(tmp_cache)
})
