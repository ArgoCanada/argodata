
test_that("argo_traj() works", {
  with_argo_example_cache({
    traj <- argo_traj("dac/csio/2900313/2900313_Rtraj.nc", quiet = TRUE)
    expect_true(all(c("cycle_number", "longitude", "latitude") %in% names(traj)))

    traj <- argo_traj(
      "dac/csio/2900313/2900313_Rtraj.nc",
      vars = c("cycle_number", "longitude", "empty"),
      quiet = TRUE
    )

    expect_identical(names(traj), c("float", "cycle_number", "longitude"))
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

test_that("assert_argo_traj_file() works", {
  with_argo_example_cache({
    expect_silent(assert_argo_traj_file(as_argo_path(argo_global_traj(quiet = TRUE))))
  })

  expect_error(assert_argo_traj_file("not a file"), "Found 1 invalid Argo trajectory path")
})

