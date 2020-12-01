
test_that("argo_filter_radius() works for lat/lon", {
  tbl <- tibble::tibble(
    latitude = c(0, 45),
    longitude = c(0, -64)
  )

  expect_identical(argo_filter_radius(tbl, 0, 0, 12000), tbl)
  expect_identical(argo_filter_radius(tbl, 0, 0, 10), tbl[1, ])
  expect_identical(argo_filter_radius(tbl, 45, -64, 10), tbl[2, ])
  expect_identical(argo_filter_radius(tbl, 45, -64, -10), tbl[integer(0), ])
})

test_that("argo_filter_rect() works for lat/lon", {
  tbl <- tibble::tibble(
    latitude = c(0, 12),
    longitude = c(0, -170)
  )

  expect_identical(argo_filter_rect(tbl, -90, 90, -180, 180), tbl)
  expect_identical(argo_filter_rect(tbl, -90, 90, -180, 180), tbl)
  expect_identical(argo_filter_rect(tbl, 11, 13, -180, -160), tbl[2, ])
  expect_identical(argo_filter_rect(tbl, 11, 13, 180, -160), tbl[2, ])
})

test_that("argo_filter_rect() works on rect", {
  tbl <- tibble::tibble(
    latitude_min = c(0, 12),
    latitude_max = c(12, 24),
    longitude_min = c(0, 170),
    longitude_max = c(10, -170)
  )

  expect_identical(argo_filter_rect(tbl, -90, 90, -180, 180), tbl)
  expect_identical(argo_filter_rect(tbl, -90, 90, -180, -160), tbl[2, ])
  expect_identical(argo_filter_rect(tbl, -90, 90, 160, 180), tbl[2, ])
  expect_identical(argo_filter_rect(tbl, -90, 90, 160, -160), tbl[2, ])
  expect_identical(argo_filter_rect(tbl, 89, 90, -180, -160), tbl[integer(0), ])
  expect_identical(argo_filter_rect(tbl, -90, 90, -10, 10), tbl[1, ])
})

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

test_that("latlon/rect checker gives reasonable errors", {
  expect_identical(
    tbl_has_latlon_or_rect(data.frame(latitude = 1, longitude = 1)),
    "latlon"
  )

  expect_identical(
    tbl_has_latlon_or_rect(
      data.frame(
        latitude_max = 1,
        latitude_min = 1,
        longitude_max = 1,
        longitude_min = 1
      )
    ),
    "rect"
  )

  expect_error(tbl_has_latlon_or_rect(data.frame()), "must contain columns")
})
