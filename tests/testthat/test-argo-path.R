
test_that("argo_path() works", {
  expect_identical(argo_path("a path"), "a path")
  expect_identical(argo_path(data.frame(path = "a path")), "a path")
  expect_error(argo_path(4), "must be a character vector")
})