
test_that("argo_nc_meta_read_config() works for meta files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_meta.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_meta_read_config(nc)
  expect_true(
    all(
      c("float", "CONFIG_MISSION_NUMBER", "CONFIG_PARAMETER_NAME",  "CONFIG_PARAMETER_VALUE")
      %in% names(nc_all)
    )
  )

  ncdf4::nc_close(nc)
})
