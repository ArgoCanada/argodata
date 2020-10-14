
test_that("path and url functions work", {
  expect_is(argo_path_global_meta(), "character")
  expect_is(argo_path_global_prof(), "character")
  expect_is(argo_path_global_tech(), "character")
  expect_is(argo_path_global_traj(), "character")

  expect_true(startsWith(argo_url(argo_path_global_meta()), argo_mirror()))
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
