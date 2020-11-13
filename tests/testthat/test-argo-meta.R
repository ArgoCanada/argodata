
test_that("argo_meta_config_param() works", {
  with_argo_example_cache({
    meta <- argo_meta_config_param("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("float", "config_mission_number", "config_parameter_name", "config_parameter_value") %in%
          names(meta)
      )
    )
  })
})

test_that("argo_read_meta_config_param() works", {
  expect_is(
    argo_read_meta_config_param(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_missions() works", {
  expect_is(
    argo_read_meta_missions(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_trans_system() works", {
  expect_is(
    argo_read_meta_trans_system(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_launch_config_param() works", {
  expect_is(
    argo_read_meta_launch_config_param(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_sensor() works", {
  expect_is(
    argo_read_meta_sensor(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_param() works", {
  expect_is(
    argo_read_meta_param(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})



