
test_that("argo_prof_levels() works", {
  with_argo_mirror(argo_test_mirror(), {
    prof <- argo_prof_levels("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
    expect_true(all(c("file", "n_prof", "n_levels", "pres", "temp") %in% names(prof)))
    expect_true(all(prof$n_prof == 1))

    prof <- argo_prof_levels(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("pres", "temp", "empty"),
      quiet = TRUE
    )

    expect_identical(
      names(prof),
      c("file",  "n_levels", "n_prof", "pres", "temp")
    )
  })
})

test_that("argo_prof_prof() works", {
  with_argo_example_cache({
    prof <- argo_prof_prof("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
    expect_true(all(c("file", "cycle_number", "latitude", "longitude") %in% names(prof)))
    expect_true(all(prof$n_prof == 1))
    expect_identical(nrow(prof), 1L)

    prof <- argo_prof_prof(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("longitude", "latitude", "empty"),
      quiet = TRUE
    )

    expect_identical(names(prof), c("file", "n_prof", "longitude", "latitude"))
  })
})

test_that("argo_prof_calib() works", {
  with_argo_example_cache({
    prof <- argo_prof_calib("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
    expect_true(all(c("file", "parameter", "n_param", "n_calib", "n_prof") %in% names(prof)))

    prof <- argo_prof_calib(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("scientific_calib_equation", "empty"),
      quiet = TRUE
    )

    expect_identical(
      names(prof),
      c("file", "n_param", "n_calib", "n_prof", "scientific_calib_equation")
    )
  })
})

test_that("argo_prof_param() works", {
  with_argo_example_cache({
    prof <- argo_prof_param("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
    expect_true(all(c("file", "n_param", "n_prof") %in% names(prof)))

    prof <- argo_prof_param(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = "station_parameters",
      quiet = TRUE
    )

    expect_identical(names(prof), c("file", "n_param", "n_prof", "station_parameters"))
  })
})

test_that("argo_prof_history() works", {
  with_argo_example_cache({
    prof <- argo_prof_history("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
    expect_true(all(c("file", "n_prof", "n_history", "history_qctest") %in% names(prof)))
    expect_true(all(prof$n_prof == 1))

    prof <- argo_prof_history(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("history_qctest", "empty"),
      quiet = TRUE
    )

    expect_identical(
      names(prof),
      c("file", "n_prof", "n_history", "history_qctest")
    )
  })
})

test_that("argo_prof_spectra() works", {
  with_argo_mirror(argo_test_mirror(), {
    prof <- argo_prof_spectra("dac/aoml/5906206/profiles/BD5906206_016.nc", quiet = TRUE)
    expect_true(
      all(
        c("file", "n_values", "n_prof", "n_levels", "uv_intensity_nitrate") %in%
          names(prof)
      )
    )
  })
})

test_that("argo_read_prof_levels() works", {
  expect_is(
    argo_read_prof_levels(
      system.file(
        "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_prof() works", {
  expect_is(
    argo_read_prof_prof(
      system.file(
        "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_calib() works", {
  expect_is(
    argo_read_prof_calib(
      system.file(
        "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_param() works", {
  expect_is(
    argo_read_prof_param(
      system.file(
        "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_history() works", {
  expect_is(
    argo_read_prof_history(
      system.file(
        "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})


test_that("argo_read_prof_levels() works for bio profiles", {
  expect_is(
    argo_read_prof_levels(
      system.file(
        "cache-test/dac/csio/2902746/profiles/BR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_prof() works for bio profiles", {
  expect_is(
    argo_read_prof_prof(
      system.file(
        "cache-test/dac/csio/2902746/profiles/BR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_history() works for bio profiles", {
  expect_is(
    argo_read_prof_history(
      system.file(
        "cache-test/dac/csio/2902746/profiles/BR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_levels() works for synthetic profiles", {
  expect_is(
    argo_read_prof_levels(
      system.file(
        "cache-test/dac/csio/2902746/profiles/SR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_prof() works for synthetic profiles", {
  expect_is(
    argo_read_prof_prof(
      system.file(
        "cache-test/dac/csio/2902746/profiles/SR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_history() errors properly for synthetic profiles", {
  expect_error(
    argo_read_prof_history(
      system.file(
        "cache-test/dac/csio/2902746/profiles/SR2902746_001.nc",
        package = "argodata"
      )
    ),
    "is missing dimension"
  )
})

test_that("assert_argo_prof_file() works", {
  expect_identical(
    assert_argo_prof_file("dac/aoml/3901309/profiles/R3901309_001D.nc"),
    "dac/aoml/3901309/profiles/R3901309_001D.nc"
  )
  expect_identical(
    assert_argo_prof_file("dac/csio/2902746/profiles/BR2902746_001.nc"),
    "dac/csio/2902746/profiles/BR2902746_001.nc"
  )
  expect_identical(
    assert_argo_prof_file("dac/csio/2902746/profiles/SR2902746_001.nc"),
    "dac/csio/2902746/profiles/SR2902746_001.nc"
  )
  expect_identical(
    assert_argo_prof_file("dac/csio/2900313/2900313_prof.nc"),
    "dac/csio/2900313/2900313_prof.nc"
  )
  expect_identical(
    assert_argo_prof_file("dac/csio/2902746/2902746_Sprof.nc"),
    "dac/csio/2902746/2902746_Sprof.nc"
  )
  expect_error(assert_argo_prof_file("not a file"), "Found 1 invalid Argo profile path")
})


test_that("argo_read_prof_levels() works for single-profile nc files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_levels(nc)
  expect_true(all(c("PRES", "PRES_QC", "TEMP", "TEMP_QC") %in% names(nc_all)))

  nc_no_meta <- argo_read_prof_levels(nc, vars = c("TEMP", "PRES", "EMPTY"), quiet = TRUE)
  expect_identical(names(nc_no_meta), c("N_LEVELS", "N_PROF", "TEMP", "PRES"))
})

test_that("argo_read_prof_levels() works for multi-profile nc files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_prof.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_levels(nc)
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
})

test_that("argo_read_prof_prof() works for single-profile nc files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_prof(nc)
  expect_true(all(c("N_PROF", "CYCLE_NUMBER", "DATA_CENTRE") %in% names(nc_all)))
  expect_identical(nrow(nc_all), 1L)
})

test_that("argo_read_prof_calib() works for single-profile nc files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_calib(nc)
  expect_true(all(c("N_PARAM", "N_CALIB", "N_PROF", "PARAMETER") %in% names(nc_all)))
})

test_that("argo_read_prof_param() works for single-profile nc files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_param(nc)
  expect_identical(names(nc_all), c("N_PARAM", "N_PROF", "STATION_PARAMETERS"))
})

test_that("argo_read_prof_param() works for BGC files", {
  nc <- system.file(
    "cache-test/dac/csio/2902746/profiles/BR2902746_001.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_param(nc)
  expect_identical(
    names(nc_all),
    c("N_PARAM", "N_PROF", "STATION_PARAMETERS", "PARAMETER_DATA_MODE")
  )
})

test_that("argo_read_prof_prof() works for multi files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_prof.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_prof(nc)
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
})

test_that("argo_read_prof_calib() works for multi files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_prof.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_calib(nc)
  expect_true(length(unique(nc_all$N_PROF)) > 1)
})

test_that("argo_read_prof_param() works for multi files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_prof.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_param(nc)
  expect_identical(names(nc_all), c("N_PARAM", "N_PROF", "STATION_PARAMETERS"))
  expect_true(length(unique(nc_all$N_PROF)) > 1)
})

test_that("argo_read_prof_history() works for single-profile nc files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
    package = "argodata"
  )
  nc_no_meta <- argo_read_prof_history(
    nc,
    vars = c("HISTORY_START_PRES", "HISTORY_QCTEST", "EMPTY"),
    quiet = TRUE
  )
  expect_identical(
    names(nc_no_meta),
    c("N_PROF", "N_HISTORY", "HISTORY_START_PRES", "HISTORY_QCTEST")
  )
})

test_that("argo_read_prof_history() works for multi-profile nc files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_prof.nc",
    package = "argodata"
  )

  expect_identical(ncol(argo_read_prof_history(nc)), 14L)
})

test_that("argo_read_prof_spectra() works", {
  # a file that contains no spectra variables
  nc <- system.file(
    "cache-test/dac/csio/2902746/profiles/BR2902746_001.nc",
    package = "argodata"
  )
  expect_null(
    expect_warning(
      argo_read_prof_spectra(nc, quiet = NA),
      "zero variables along an"
    )
  )

  # a file that contains one spectra variable
  nc <- system.file(
    "cache-test/dac/aoml/5906206/profiles/BD5906206_016.nc",
    package = "argodata"
  )

  nc_all <- argo_read_prof_spectra(nc)
  expect_identical(
    names(nc_all),
    c("N_VALUES41", "N_LEVELS", "N_PROF", "UV_INTENSITY_NITRATE")
  )

  expect_identical(
    argo_read_prof_spectra(nc, vars = "UV_INTENSITY_NITRATE"),
    nc_all
  )

  expect_null(
    expect_warning(
      argo_read_prof_spectra(nc, vars = character(), quiet = NA),
      "must be NULL",
    )
  )

  expect_null(
    expect_warning(
      argo_read_prof_spectra(nc, vars = "NOT_A_VAR", quiet = NA),
      "missing spectra variable",
    )
  )
})
