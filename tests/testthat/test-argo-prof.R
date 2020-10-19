
test_that("argo_prof() works", {
  prof <- with_argo_mirror(
    argo_test_mirror(),
    argo_prof("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
  )

  expect_true(FALSE)
})

test_that("argo_read_prof() works", {
  expect_is(
    argo_read_prof("inst/cache-test/dac/csio/2900313/profiles/D2900313_000.nc"),
    "tbl_df"
  )
})

test_that("assert_argo_prof_file() works", {
  expect_identical(
    assert_argo_prof_file("aoml/3901309/profiles/R3901309_001D.nc"),
    "aoml/3901309/profiles/R3901309_001D.nc"
  )
  expect_error(assert_argo_prof_file("not a file"), "Found 1 invalid Argo profile path")
})
