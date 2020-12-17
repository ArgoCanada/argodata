
test_that("argo_nc_tech_read_tech_param() works for tech files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_tech.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_tech_read_tech_param(nc)
  expect_true(
    all(
      c("TECHNICAL_PARAMETER_NAME", "TECHNICAL_PARAMETER_VALUE") %in% names(nc_all)
    )
  )

  ncdf4::nc_close(nc)
})
