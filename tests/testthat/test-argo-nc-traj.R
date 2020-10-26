
test_that("argo_nc_traj_*() works for trajectory files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_traj_read_meas(nc)
  expect_true(all(argo_nc_traj_vars_meas(nc) %in% names(nc_all)))

  nc_no_meta <- argo_nc_traj_read_meas(nc, vars = c("LONGITUDE", "LATITUDE", "EMPTY"))
  expect_identical(names(nc_no_meta), c("float", "LONGITUDE", "LATITUDE"))

  ncdf4::nc_close(nc)
})
