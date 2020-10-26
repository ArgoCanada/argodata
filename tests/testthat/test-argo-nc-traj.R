
test_that("argo_nc_traj_*() works for trajectory files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_traj_read(nc)
  expect_true(all(argo_nc_traj_list_vars(nc) %in% names(nc_all)))

  nc_no_meta <- argo_nc_traj_read(nc, vars = c("LONGITUDE", "LATITUDE", "EMPTY"))
  expect_identical(names(nc_no_meta), c("float", "LONGITUDE", "LATITUDE", "EMPTY"))
  expect_true(all(is.na(nc_no_meta$EMPTY)))

  ncdf4::nc_close(nc)
})
