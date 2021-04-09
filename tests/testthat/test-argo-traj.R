
test_that("argo_traj_measurement() works", {
  with_argo_example_cache({
    traj <- argo_traj_measurement("dac/csio/2900313/2900313_Rtraj.nc", quiet = TRUE)
    expect_true(
      all(
        c("file", "cycle_number", "longitude", "latitude") %in%
          names(traj)
      )
    )

    traj <- argo_traj_measurement(
      "dac/csio/2900313/2900313_Rtraj.nc",
      vars = c("cycle_number", "longitude", "empty"),
      quiet = TRUE
    )

    expect_identical(
      names(traj),
      c("file", "n_measurement", "cycle_number", "longitude")
    )
  })
})

test_that("argo_traj_cycle() works", {
  with_argo_example_cache({
    traj <- argo_traj_cycle("dac/csio/2900313/2900313_Rtraj.nc", quiet = TRUE)
    expect_true(all(c("date_descent_start", "data_mode") %in% names(traj)))

    traj <- argo_traj_cycle(
      "dac/csio/2900313/2900313_Rtraj.nc",
      vars = c("date_descent_start", "empty"),
      quiet = TRUE
    )

    expect_identical(names(traj), c("file", "n_cycle", "date_descent_start"))
  })
})

test_that("argo_traj_param() works", {
  with_argo_example_cache({
    traj <- argo_traj_param("dac/csio/2900313/2900313_Rtraj.nc", quiet = TRUE)
    expect_true(all(c("n_param", "trajectory_parameters") %in% names(traj)))

    traj <- argo_traj_param(
      "dac/csio/2900313/2900313_Rtraj.nc",
      vars = c("empty"),
      quiet = TRUE
    )

    expect_identical(names(traj), c("file", "n_param"))
  })
})

test_that("argo_traj_history() works", {
  with_argo_example_cache({
    traj <- argo_traj_history("dac/csio/2900313/2900313_Rtraj.nc", quiet = TRUE)
    expect_true(
      all(
        c("file", "history_index_dimension", "history_software") %in%
          names(traj)
      )
    )

    traj <- argo_traj_history(
      "dac/csio/2900313/2900313_Rtraj.nc",
      vars = c("history_index_dimension", "history_software", "empty"),
      quiet = TRUE
    )

    expect_identical(
      names(traj),
      c("file", "n_history", "history_index_dimension", "history_software")
    )
  })
})

test_that("argo_read_traj_measurement() works", {
  expect_is(
    argo_read_traj_measurement(
      system.file(
        "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_traj_cycle() works", {
  expect_is(
    argo_read_traj_cycle(
      system.file(
        "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})


test_that("argo_read_traj_cycle() works", {
  expect_is(
    argo_read_traj_param(
      system.file(
        "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_traj_history() works", {
  expect_is(
    argo_read_traj_history(
      system.file(
        "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_traj_measurement() works for bio files", {
  expect_is(
    argo_read_traj_measurement(
      system.file(
        "cache-test/dac/csio/2902746/2902746_BRtraj.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_traj_cycle() works for bio files", {
  expect_is(
    argo_read_traj_cycle(
      system.file(
        "cache-test/dac/csio/2902746/2902746_BRtraj.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_traj_history() works for bio files", {
  expect_is(
    argo_read_traj_history(
      system.file(
        "cache-test/dac/csio/2902746/2902746_BRtraj.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("assert_argo_traj_file() works", {
  with_argo_example_cache({
    expect_silent(assert_argo_traj_file(as_argo_path(argo_global_traj(quiet = TRUE))))
    expect_silent(assert_argo_traj_file(as_argo_path(argo_global_bio_traj(quiet = TRUE))))
  })

  expect_error(assert_argo_traj_file("not a file"), "Found 1 invalid Argo trajectory path")
})

test_that("argo_read_traj_measurement() works for trajectory files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
    package = "argodata"
  )

  nc_all <- argo_read_traj_measurement(nc)
  expect_true(all(c("N_MEASUREMENT", "JULD", "JULD_QC") %in% names(nc_all)))

  nc_no_meta <- argo_read_traj_measurement(
    nc,
    vars = c("LONGITUDE", "LATITUDE", "EMPTY"),
    quiet = TRUE
  )
  expect_identical(names(nc_no_meta), c("N_MEASUREMENT", "LONGITUDE", "LATITUDE"))
})

test_that("argo_read_traj_cycle() works for trajectory files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
    package = "argodata"
  )

  nc_all <- argo_read_traj_cycle(nc)
  expect_true(
    all(
      c("N_CYCLE", "JULD_DESCENT_START", "JULD_DESCENT_START_STATUS") %in% names(nc_all)
    )
  )

  nc_no_meta <- argo_read_traj_cycle(nc, vars = c("CYCLE_NUMBER_INDEX", "EMPTY"), quiet = TRUE)
  expect_identical(names(nc_no_meta), c("N_CYCLE", "CYCLE_NUMBER_INDEX"))
})

test_that("argo_read_traj_param() works for trajectory files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
    package = "argodata"
  )

  nc_all <- argo_read_traj_param(nc)
  expect_true(
    all(
      c("N_PARAM", "TRAJECTORY_PARAMETERS") %in% names(nc_all)
    )
  )
})

test_that("argo_read_traj_history() works for trajectory files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
    package = "argodata"
  )

  nc_all <- argo_read_traj_history(nc)
  expect_true(
    all(
      c("N_HISTORY", "HISTORY_PREVIOUS_VALUE", "HISTORY_STEP") %in% names(nc_all)
    )
  )

  nc_no_meta <- argo_read_traj_history(
    nc,
    vars = c("HISTORY_INDEX_DIMENSION", "HISTORY_QCTEST", "EMPTY"),
    quiet = TRUE
  )
  expect_identical(names(nc_no_meta), c("N_HISTORY", "HISTORY_INDEX_DIMENSION", "HISTORY_QCTEST"))
})
