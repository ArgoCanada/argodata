
test_that("argo_filter_date() works", {

  tbl <- tibble::tibble(
    date = as.POSIXct(
      c("2020-01-01 00:00:00", "2020-01-02 00:00:00"),
      tz = "UTC"
    )
  )

  expect_identical(
    argo_filter_date(tbl, "2020-01-01 00:00:00", "2020-01-02 00:00:00"),
    tbl
  )

  expect_identical(
    argo_filter_date(tbl, "2020-01-01 00:00:00", "2020-01-01 00:00:00"),
    tbl[1, ]
  )

  expect_identical(
    argo_filter_date(tbl, "2019-01-01 00:00:00", "2019-01-02 00:00:00"),
    tbl[integer(0), ]
  )

  expect_identical(
    argo_filter_date(tbl, NA),
    tbl[integer(0), ]
  )
})

test_that("argo_filter_updated() works", {

  tbl <- tibble::tibble(
    date_update = as.POSIXct(
      c("2020-01-01 00:00:00", "2020-01-02 00:00:00"),
      tz = "UTC"
    )
  )

  expect_identical(
    argo_filter_updated(tbl, "2020-01-01 00:00:00", "2020-01-02 00:00:00"),
    tbl
  )

  expect_identical(
    argo_filter_updated(tbl, "2020-01-01 00:00:00", "2020-01-01 00:00:00"),
    tbl[1, ]
  )

  expect_identical(
    argo_filter_updated(tbl, "2019-01-01 00:00:00", "2019-01-02 00:00:00"),
    tbl[integer(0), ]
  )

  expect_identical(
    argo_filter_updated(tbl, NA),
    tbl[integer(0), ]
  )
})
