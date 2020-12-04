
test_that("argo_qc_*() functions work", {
  tbl <- tibble::tibble(value = c("a", "b"), value_qc = c(1, 2))
  expect_identical(
    argo_qc_censor_if(tbl, value, 1),
    tibble::tibble(value = c(NA, "b"), value_qc = c(1, 2))
  )

  expect_identical(
    argo_qc_censor_if_not(tbl, value, 1),
    tibble::tibble(value = c("a", NA), value_qc = c(1, 2))
  )

  expect_identical(
    argo_qc_keep_if_any(tbl, value, 1),
    tibble::tibble(value = c("a"), value_qc = c(1))
  )

  expect_identical(
    argo_qc_keep_if_all(tbl, value, 1),
    tibble::tibble(value = c("a"), value_qc = c(1))
  )

  expect_identical(
    argo_qc_discard_if_any(tbl, value, 1),
    tibble::tibble(value = c("b"), value_qc = c(2))
  )

  expect_identical(
    argo_qc_discard_if_all(tbl, value, 1),
    tibble::tibble(value = c("b"), value_qc = c(2))
  )
})

test_that("argo_qc_*() functions work with zero columns selected", {
  tbl <- tibble::tibble(value = c("a", "b"), value_qc = c(1, 2))
  expect_identical(argo_qc_censor_if(tbl, character(0), 1), tbl)
  expect_identical(argo_qc_censor_if_not(tbl, character(0), 1), tbl)
  expect_identical(argo_qc_keep_if_all(tbl, character(0), 1), tbl)
  expect_identical(argo_qc_keep_if_any(tbl, character(0), 1), tbl)
  expect_identical(argo_qc_discard_if_all(tbl, character(0), 1), tbl)
  expect_identical(argo_qc_discard_if_any(tbl, character(0), 1), tbl)
})

test_that("argo_qc_cols() works", {
  expect_identical(
    argo_qc_cols(tibble::tibble(thing = 1, thing_qc = 2), thing),
    "thing_qc"
  )

  expect_error(
    argo_qc_cols(tibble::tibble(thing2 = 1, thing_qc = 2), thing),
    class = "vctrs_error_subscript_oob"
  )

  expect_error(
    argo_qc_cols(tibble::tibble(thing2 = 1, thing_qc = 2), thing2),
    "must have a paired"
  )
})
