
test_that("argo_prof() works", {
  with_argo_mirror(argo_test_mirror(), {
    prof <- argo_prof("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
    expect_true(all(c("cycle_number", "pres", "temp") %in% names(prof)))
    expect_true(all(prof$cycle_number == 0))

    prof <- argo_prof(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("pres", "temp", "empty"),
      quiet = TRUE
    )

    expect_identical(names(prof), c("float", "cycle_number", "date", "pres", "temp", "empty"))
  })
})

test_that("argo_read_prof() works", {
  expect_is(
    argo_read_prof(
      system.file(
        "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("assert_argo_prof_file() works", {
  expect_identical(
    assert_argo_prof_file("dac/aoml/3901309/profiles/R3901309_001D.nc"),
    "dac/aoml/3901309/profiles/R3901309_001D.nc"
  )
  expect_error(assert_argo_prof_file("not a file"), "Found 1 invalid Argo profile path")
})
