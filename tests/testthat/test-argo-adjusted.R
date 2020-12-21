
test_that("argo_use_adjusted() works", {
  tbl <- tibble::tibble(value = c(1, 2), value_adjusted = c(100, 200))
  expect_identical(
    argo_use_adjusted(tbl, value),
    tibble::tibble(value = c(100, 200), value_adjusted = c(100, 200))
  )

  expect_identical(argo_use_adjusted(tbl, !! character()), tbl)
})

test_that("argo_adjusted_cols() works", {
  expect_identical(
    argo_adjusted_cols(tibble::tibble(thing = 1, thing_adjusted = 2), thing),
    "thing_adjusted"
  )

  expect_error(
    argo_adjusted_cols(tibble::tibble(thing2 = 1, thing_adjusted = 2), thing),
    class = "vctrs_error_subscript_oob"
  )

  expect_error(
    argo_adjusted_cols(tibble::tibble(thing2 = 1, thing_adjusted = 2), thing2),
    "must have a paired"
  )
})

