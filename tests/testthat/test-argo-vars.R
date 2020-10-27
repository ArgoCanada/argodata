
test_that("argo_vars() works", {
  vars <- with_argo_example_cache({
    argo_vars("dac/csio/2900313/profiles/D2900313_000.nc")
  })

  expect_true(all(c("float", "name", "longname", "units") %in% colnames(vars)))
  expect_true(all(c("data_type", "temp", "date") %in% vars$name))

  vars <- with_argo_example_cache({
    argo_vars(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("data_type", "temp", "date", "empty")
    )
  })

  expect_setequal(vars$name, c("data_type", "temp", "date"))
})

test_that("argo_read_vars() works", {
  prof_file <- system.file(
    "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
    package = "argodata"
  )

  vars <- argo_read_vars(prof_file)
  expect_true(all(c("float", "name", "longname", "units") %in% colnames(vars)))
  expect_true(all(c("DATA_TYPE", "TEMP", "JULD") %in% vars$name))

  vars <- argo_read_vars(prof_file, vars = c("DATA_TYPE", "TEMP", "JULD", "empty"))
  expect_setequal(vars$name, c("DATA_TYPE", "TEMP", "JULD"))
})
