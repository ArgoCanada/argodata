
test_that("argo_traj_meas() works", {
  with_argo_example_cache({
    traj <- argo_traj_meas("dac/csio/2900313/2900313_Rtraj.nc", quiet = TRUE)
    expect_true(all(c("cycle_number", "longitude", "latitude") %in% names(traj)))

    traj <- argo_traj_meas(
      "dac/csio/2900313/2900313_Rtraj.nc",
      vars = c("cycle_number", "longitude", "empty"),
      quiet = TRUE
    )

    expect_identical(names(traj), c("float", "cycle_number", "longitude"))
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

    expect_identical(names(traj), c("float", "date_descent_start"))
  })
})

test_that("argo_traj_history() works", {
  with_argo_example_cache({
    traj <- argo_traj_history("dac/csio/2900313/2900313_Rtraj.nc", quiet = TRUE)
    expect_true(all(c("history_index_dimension", "history_software") %in% names(traj)))

    traj <- argo_traj_history(
      "dac/csio/2900313/2900313_Rtraj.nc",
      vars = c("history_index_dimension", "history_software", "empty"),
      quiet = TRUE
    )

    expect_identical(names(traj), c("float", "history_index_dimension", "history_software"))
  })
})

test_that("argo_read_traj_meas() works", {
  expect_is(
    argo_read_traj_meas(
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

test_that("argo_read_traj_meas() works for bio files", {
  expect_is(
    argo_read_traj_meas(
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
