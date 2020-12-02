
test_that("multi_file_download() works with zero items", {
  expect_identical(multi_file_download(character(0), character(0)), character(0))
})

test_that("multi_file_download() works", {
  skip_if_offline()

  dest <- tempfile()
  expect_identical(multi_file_download("https://httpbin.org/get", dest), dest)

  expect_true(file.exists(dest))

  expect_error(
    multi_file_download("https://httpbin.org/not_an_endpoint", dest),
    "HTTP error 404"
  )

  unlink(dest)

  dest2 <- tempfile()
  expect_identical(
    multi_file_download("https://httpbin.org/get", c(dest, dest2)),
    c(dest, dest2)
  )
  expect_true(file.exists(dest2))
  expect_true(file.exists(dest))

  unlink(dest)
  unlink(dest2)
})

test_that("multi_file_download() creates missing directories", {
  skip_if_offline()

  missing_dir <- tempfile()
  dest <- file.path(missing_dir, "some_file.json")
  expect_false(dir.exists(missing_dir))
  multi_file_download("https://httpbin.org/get", dest)
  expect_true(dir.exists(missing_dir))
  unlink(missing_dir, recursive = TRUE)
})

test_that("multi_file_download_async() works with zero items", {
  expect_identical(multi_file_download_async(character(0), character(0)), character(0))
})

test_that("multi_file_download_async() works", {
  skip_if_offline()

  dest <- tempfile()
  expect_identical(multi_file_download_async("https://httpbin.org/get", dest), dest)
  expect_true(file.exists(dest))

  expect_error(
    multi_file_download_async("https://httpbin.org/not_an_endpoint", dest),
    "1/1 file failed"
  )

  expect_error(
    expect_message(
      multi_file_download_async("https://httpbin.org/not_an_endpoint", dest),
      "failed with status 404"
    ),
    "1/1 file failed"
  )

  expect_error(
    expect_message(
      multi_file_download_async("file://path/to/nowhere", dest),
      "Fetching 'file://path/to/nowhere/' failed"
    ),
    "1/1 file failed"
  )

  unlink(dest)

  dest2 <- tempfile()
  expect_identical(
    multi_file_download_async("https://httpbin.org/get", c(dest, dest2)),
    c(dest, dest2)
  )
  expect_true(file.exists(dest2))
  expect_true(file.exists(dest))

  unlink(dest)
  unlink(dest2)
})

test_that("multi_file_download_async() creates missing directories", {
  skip_if_offline()

  missing_dir <- tempfile()
  dest <- file.path(missing_dir, "some_file.json")
  expect_false(dir.exists(missing_dir))
  multi_file_download_async("https://httpbin.org/get", dest)
  expect_true(dir.exists(missing_dir))
  unlink(missing_dir, recursive = TRUE)
})

