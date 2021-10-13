
test_that("argo_filter_float() works", {
  tbl <- tibble::tibble(
    file = c("0123/456.nc", "0123/789.nc")
  )

  expect_identical(argo_filter_float(tbl, "0123"), tbl[integer(0), ])
  expect_identical(argo_filter_float(tbl, "456"), tbl[1, ])
  expect_identical(argo_filter_float(tbl, "789"), tbl[2, ])
  expect_identical(argo_filter_float(tbl, c("456", "789")), tbl)
  expect_identical(argo_filter_float(tbl, "45"), tbl[integer(0), ])

  expect_error(argo_filter_float(tbl, c("abc", "def")), "must be a numeric identifier")
})

test_that("argo_filter_parameter() works", {
  tbl <- tibble::tibble(
    parameters = c("PARAM1 PARAM2", "PARAM3")
  )

  expect_identical(argo_filter_parameter(tbl, "param0"), tbl[integer(0), ])
  expect_identical(argo_filter_parameter(tbl, "param1"), tbl[1, ])
  expect_identical(argo_filter_parameter(tbl, "param3"), tbl[2, ])
  expect_identical(argo_filter_parameter(tbl, c("param1", "param3")), tbl)

  expect_error(argo_filter_parameter(tbl, c("abc", "$$$")), "must be an alpha-numeric string")
})


test_that("argo_filter_data_mode() works", {
  tbl <- tibble::tibble(
    file = c("D456_stuff.nc", "R456_stuff.nc", "123_Dprof.nc", "123_Rtraj.nc")
  )

  expect_identical(argo_filter_data_mode(tbl, "realtime"), tbl[c(2, 4), ])
  expect_identical(argo_filter_data_mode(tbl, "delayed"), tbl[c(1, 3), ])
  expect_error(argo_filter_data_mode(tbl, "fishing"), "must be one of")
})

test_that("argo_filter_parameter_data_mode() works", {
  tbl <- tibble::tibble(
    parameters = c("PARAM1 PARAM2", "PARAM3"),
    parameter_data_mode = c("RD", "A")
  )

  expect_identical(argo_filter_parameter_data_mode(tbl, "param0", "R"), tbl[integer(0), ])
  expect_identical(argo_filter_parameter_data_mode(tbl, "param1", "R"), tbl[1, ])
  expect_identical(argo_filter_parameter_data_mode(tbl, "param3", "R"), tbl[integer(0), ])
  expect_identical(argo_filter_parameter_data_mode(tbl, "param3", "A"), tbl[2, ])

  expect_error(
    argo_filter_parameter_data_mode(tbl, c("abc", "$$$")),
    "must be length 1"
  )

  expect_error(
    argo_filter_parameter_data_mode(tbl, "abc", "not a data mode"),
    "must be one of"
  )
})

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

test_that("argo_filter_radius() works for rect", {
  tbl <- tibble::tibble(
    latitude_min = c(0, 12),
    latitude_max = c(12, 24),
    longitude_min = c(0, 170),
    longitude_max = c(10, -170)
  )

  expect_identical(argo_filter_radius(tbl, 0, 0, 1000), tbl[1, ])
  expect_identical(argo_filter_radius(tbl, -7, -7, 1000), tbl[integer(0), ])
  expect_identical(argo_filter_radius(tbl, 12, 180, 1000), tbl[2, ])
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

test_that("argo_filter_rect() works for rect", {
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

test_that("argo_filter_direction() works", {
  tbl <- tibble::tibble(
    file = c("R13857_010.nc", "R3901309_001D.nc")
  )

  expect_identical(argo_filter_direction(tbl, "ascending"), tbl[1, ])
  expect_identical(argo_filter_direction(tbl, "descending"), tbl[2, ])
  expect_error(argo_filter_direction(tbl, "sideways, bro"), "must be one of")
})
