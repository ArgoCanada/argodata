
test_that("argo_info() works", {
  info <- with_argo_example_cache({
    argo_info("dac/csio/2900313/profiles/D2900313_000.nc")
  })

  expect_true(all(c("file", "data_type", "date_creation") %in% colnames(info)))

  info <- with_argo_example_cache({
    argo_info(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("data_type", "date_creation", "empty")
    )
  })

  expect_setequal(colnames(info), c("file", "data_type", "date_creation"))
})

test_that("argo_read_info() works", {
  prof_file <- system.file(
    "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
    package = "argodata"
  )

  info <- argo_read_info(prof_file)
  expect_true(all(c("DATA_TYPE", "DATE_CREATION") %in% colnames(info)))

  info <- argo_read_info(prof_file, vars = c("DATA_TYPE", "DATE_CREATION", "empty"))
  expect_setequal(colnames(info), c("DATA_TYPE", "DATE_CREATION"))
  expect_equal(nchar(info$DATA_TYPE), 16)
})
