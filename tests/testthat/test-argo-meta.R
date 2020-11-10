
test_that("argo_meta_config() works", {
  with_argo_example_cache({
    meta <- argo_meta_config("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("float", "config_mission_number", "config_parameter_name", "config_parameter_value") %in%
          names(meta)
      )
    )
  })
})

test_that("argo_read_meta_config() works", {
  expect_is(
    argo_read_meta_config(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})
