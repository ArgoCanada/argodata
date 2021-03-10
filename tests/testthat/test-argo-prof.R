
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
      c("file", "n_prof", "n_levels", "pres", "temp")
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

    expect_identical(names(prof), c("file", "n_param", "n_calib", "n_prof", "scientific_calib_equation"))
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
