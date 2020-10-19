
test_that("as_argo_path() works", {
  expect_identical(as_argo_path("a path"), "a path")
  expect_identical(as_argo_path(data.frame(file = "a path")), "dac/a path")
  expect_error(as_argo_path(4), "must be a character vector")
  expect_identical(as_argo_path(character(0)), character(0))
  expect_identical(as_argo_path(data.frame(file = character(0))), character(0))
})
