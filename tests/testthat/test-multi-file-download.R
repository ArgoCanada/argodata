
test_that("multi_file_download() works", {
  skip_if_offline()

  dest <- tempfile()
  expect_message(
    expect_identical(multi_file_download("https://httpbin.org/get", dest), dest),
    "https://httpbin"
  )
  expect_true(file.exists(dest))

  expect_silent(
    multi_file_download("https://httpbin.org/get", dest, quiet = TRUE)
  )

  expect_error(
    multi_file_download("https://httpbin.org/not_an_endpoint", dest, quiet = TRUE),
    "HTTP error 404"
  )

  unlink(dest)

  dest2 <- tempfile()
  expect_identical(
    multi_file_download("https://httpbin.org/get", c(dest, dest2), quiet = TRUE),
    c(dest, dest2)
  )
  expect_true(file.exists(dest2))
  expect_true(file.exists(dest))

  unlink(dest)
  unlink(dest2)
})
