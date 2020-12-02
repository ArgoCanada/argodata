
test_that("argo_mapper() can use functions and strings", {
  prev_mapper <- argo_set_mapper(NULL)
  expect_identical(argo_mapper(), argo_map_default)
  expect_identical(argo_map(1:2, `+`, 1L), list(2L, 3L))

  argo_set_mapper("base::lapply")
  expect_identical(argo_mapper(), lapply)
  expect_identical(argo_map(1:2, `+`, 1L), list(2L, 3L))

  argo_set_mapper(argo_map_future)
  expect_identical(argo_map(1:2, `+`, 1L), list(2L, 3L))

  argo_set_mapper(123)
  expect_error(argo_mapper(), "must be a string")

  argo_set_mapper(prev_mapper)
})