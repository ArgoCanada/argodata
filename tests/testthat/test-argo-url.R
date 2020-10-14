
test_that("path and url functions work", {
  expect_is(argo_path_global_meta(), "character")
  expect_is(argo_path_global_prof(), "character")
  expect_is(argo_path_global_tech(), "character")
  expect_is(argo_path_global_traj(), "character")

  expect_is(argo_url_global_meta(), "character")
  expect_is(argo_url_global_prof(), "character")
  expect_is(argo_url_global_tech(), "character")
  expect_is(argo_url_global_traj(), "character")
})
