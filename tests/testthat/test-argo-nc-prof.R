
test_that("argo_nc_prof_*_levels() works for single-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_levels(nc)
  expect_true(all(argo_nc_prof_vars_levels(nc) %in% names(nc_all)))

  nc_no_meta <- argo_nc_prof_read_levels(nc, vars = c("TEMP", "PRES", "EMPTY"))
  expect_identical(names(nc_no_meta), c("N_PROF", "N_LEVELS", "TEMP", "PRES"))

  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_levels() works for multi-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_prof.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_levels(nc)
  expect_identical(
    length(unique(nc_all$N_PROF)),
    length(
      list.files(
        system.file(
          "cache-test/dac/csio/2900313/profiles",
          package = "argodata"
        )
      )
    )
  )

  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_prof() works for single-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_prof(nc)
  expect_true(all(argo_nc_prof_vars_prof(nc) %in% names(nc_all)))

  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_calib() works for single-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_calib(nc)
  expect_true(all(argo_nc_prof_vars_calib(nc) %in% names(nc_all)))
  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_param() works for single-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_param(nc)
  expect_true(all(argo_nc_prof_vars_param(nc) %in% names(nc_all)))
  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_param() works for BGC files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2902746/profiles/BR2902746_001.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_param(nc)
  expect_true(all(argo_nc_prof_vars_param(nc) %in% names(nc_all)))

  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_prof() works for multi files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_prof.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_prof(nc)
  expect_true(all(argo_nc_prof_vars_prof(nc) %in% names(nc_all)))
  expect_identical(
    length(nc_all$CYCLE_NUMBER),
    length(
      list.files(
        system.file(
          "cache-test/dac/csio/2900313/profiles",
          package = "argodata"
        )
      )
    )
  )

  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_calib() works for multi files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_prof.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_calib(nc)
  expect_true(all(argo_nc_prof_vars_calib(nc) %in% names(nc_all)))
  expect_true(length(unique(nc_all$N_PROF)) > 1)

  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_param() works for multi files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_prof.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_param(nc)
  expect_true(all(argo_nc_prof_vars_param(nc) %in% names(nc_all)))
  expect_true(length(unique(nc_all$N_PROF)) > 1)

  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_history() works for single-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
      package = "argodata"
    )
  )
  nc_no_meta <- argo_nc_prof_read_history(
    nc,
    vars = c("HISTORY_START_PRES", "HISTORY_QCTEST", "EMPTY")
  )
  expect_identical(
    names(nc_no_meta),
    c("N_PROF", "N_HISTORY", "HISTORY_START_PRES", "HISTORY_QCTEST")
  )

  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_history() works for multi-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_prof.nc",
      package = "argodata"
    )
  )

  expect_identical(argo_nc_prof_read_history(nc), tibble::tibble())
  ncdf4::nc_close(nc)
})
