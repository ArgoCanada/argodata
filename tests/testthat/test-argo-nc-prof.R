
test_that("argo_nc_prof_*_levels() works for single-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_levels(nc)
  expect_true(all(argo_nc_prof_vars_levels(nc) %in% names(nc_all)))
  expect_true(all(argo_nc_prof_vars_prof(nc) %in% names(nc_all)))

  nc_no_meta <- argo_nc_prof_read_levels(nc, vars = c("TEMP", "PRES", "EMPTY"), meta = character(0))
  expect_identical(names(nc_no_meta), c("float", "TEMP", "PRES"))

  nc_no_vals <- argo_nc_prof_read_levels(nc, vars = character(0), meta = c("CYCLE_NUMBER", "EMPTY"))
  expect_identical(names(nc_no_vals), c("float", "CYCLE_NUMBER"))

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
  expect_true(all(nc_all$float == "csio/2900313"))
  expect_identical(
    length(unique(nc_all$CYCLE_NUMBER)),
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

  nc_no_vals <- argo_nc_prof_read_levels(nc, vars = character(0), meta = c("CYCLE_NUMBER", "EMPTY"))
  expect_identical(names(nc_no_vals), c("float", "CYCLE_NUMBER"))

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

test_that("argo_nc_prof_*_history() works for single-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_prof_read_history(nc)
  expect_true(all(argo_nc_prof_vars_history(nc) %in% names(nc_all)))
  expect_true(all(argo_nc_prof_vars_prof(nc) %in% names(nc_all)))

  nc_no_meta <- argo_nc_prof_read_history(
    nc,
    vars = c("HISTORY_START_PRES", "HISTORY_QCTEST", "EMPTY"),
    meta = character(0)
  )
  expect_identical(names(nc_no_meta), c("float", "HISTORY_START_PRES", "HISTORY_QCTEST"))

  nc_no_vals <- argo_nc_prof_read_history(nc, vars = character(0), meta = c("CYCLE_NUMBER", "EMPTY"))
  expect_identical(names(nc_no_vals), c("float", "CYCLE_NUMBER"))

  ncdf4::nc_close(nc)
})

test_that("argo_nc_prof_*_history() works for multi-profile nc files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_prof.nc",
      package = "argodata"
    )
  )

  expect_identical(argo_nc_prof_read_history(nc), tibble::tibble(float = character(0)))
  ncdf4::nc_close(nc)
})
