
test_that("argo_read_traj_measurement() works for trajectory files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_traj_read_measurement(nc)
  expect_true(all(argo_nc_traj_vars_measurement(nc) %in% names(nc_all)))

  nc_no_meta <- argo_nc_traj_read_measurement(nc, vars = c("LONGITUDE", "LATITUDE", "EMPTY"))
  expect_identical(names(nc_no_meta), c("N_MEASUREMENT", "LONGITUDE", "LATITUDE"))

  ncdf4::nc_close(nc)
})

test_that("argo_read_traj_cycle() works for trajectory files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_traj_read_cycle(nc)
  expect_true(all(argo_nc_traj_vars_cycle(nc) %in% names(nc_all)))

  nc_no_meta <- argo_nc_traj_read_cycle(nc, vars = c("CYCLE_NUMBER_INDEX", "EMPTY"))
  expect_identical(names(nc_no_meta), c("N_CYCLE", "CYCLE_NUMBER_INDEX"))

  ncdf4::nc_close(nc)
})

test_that("argo_read_traj_history() works for trajectory files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_traj_read_history(nc)
  expect_true(all(argo_nc_traj_vars_history(nc) %in% names(nc_all)))

  nc_no_meta <- argo_nc_traj_read_history(nc, vars = c("HISTORY_INDEX_DIMENSION", "HISTORY_QCTEST", "EMPTY"))
  expect_identical(names(nc_no_meta), c("N_HISTORY", "HISTORY_INDEX_DIMENSION", "HISTORY_QCTEST"))

  ncdf4::nc_close(nc)
})
