
test_that("path and url functions work", {
  expect_true(startsWith(argo_url("some/path"), argo_mirror()))
  expect_identical(
    argo_url("/some/path"),
    argo_url("some/path")
  )
  expect_identical(
    argo_url(data.frame(path = "/some/path")),
    argo_url("/some/path")
  )
  expect_length(argo_url(character(0)), 0)
})
