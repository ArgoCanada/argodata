
test_that("qctest unnester works", {
  expect_identical(
    lapply(
      2^(1:26),
      unpack_flags,
      flags = 2^(26:1),
      values = letters
    ),
    as.list(rev(letters))
  )

  expect_identical(
    argo_unnest_history_qctest(
      tibble::tibble(n = 1:4, history_qctest = c("2", "4", "86", ""))
    ),
    tibble::tibble(
      n = c(1L, 2L, 3L, 3L, 3L, 4L),
      history_qctest = c("2", "4", "2", "4", "80", NA)
    )
  )
})

test_that("qctest unnester works with NA values", {
  expect_identical(
    argodata:::unpack_flags(NA_character_),
    NA_character_
  )
})
