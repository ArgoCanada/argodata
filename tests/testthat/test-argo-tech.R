
test_that("argo_tech_tech_param() works", {
  with_argo_example_cache({
    meta <- argo_tech_tech_param("dac/csio/2900313/2900313_tech.nc", quiet = TRUE)
    expect_true(
      all(
        c("technical_parameter_name", "technical_parameter_value") %in%
          names(meta)
      )
    )
  })
})

test_that("argo_read_tech_tech_param() works", {
  expect_is(
    argo_read_tech_tech_param(
      system.file(
        "cache-test/dac/csio/2900313/2900313_tech.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_reac_tech_tech_param() works for tech files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_tech.nc",
    package = "argodata"
  )

  nc_all <- argo_read_tech_tech_param(nc)
  expect_true(
    all(
      c("TECHNICAL_PARAMETER_NAME", "TECHNICAL_PARAMETER_VALUE") %in% names(nc_all)
    )
  )
})
