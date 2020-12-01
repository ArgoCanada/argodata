
test_that("argo_assert_columns() works", {
  expect_identical(argo_assert_columns(data.frame(a = 1), "a"), data.frame(a = 1))
  expect_error(argo_assert_columns(data.frame(a = 1), "b"), "`tbl` must have column 'b'")
  expect_error(argo_assert_columns(data.frame(a = 1), c("a", "b")), "`tbl` is missing column 'b'")
})

test_that("insert_vector() works", {
  expect_identical(
    insert_vector(c("one", "two", "three"), "XXX", 1),
    c("XXX", "one", "two", "three")
  )

  expect_identical(
    insert_vector(c("one", "two", "three"), "XXX", 2),
    c("one", "XXX", "two", "three")
  )

  expect_identical(
    insert_vector(c("one", "two", "three"), "XXX", 4),
    c("one", "two", "three", "XXX")
  )

  expect_identical(
    insert_vector(c("one", "two", "three"), c("XXX1", "XXX2"), 1),
    c("XXX1", "XXX2", "one", "two", "three")
  )

  expect_identical(
    insert_vector(c("one", "two", "three"), c("XXX1", "XXX2"), 4),
    c("one", "two", "three", "XXX1", "XXX2")
  )
})

test_that("geodist functions work", {
  expect_equal(geodist_lnglat(0, 0, 0, 90, R = 1), pi / 2)
  expect_equal(geodist_lnglat(0, 0, 90, 0, R = 1), pi / 2)
  expect_equal(geodist_lnglat(180, 0, 0, 0, R = 1), pi)
  expect_equal(geodist_lnglat(0, 90, 0, 0, R = 1), pi / 2)

  expect_equal(
    geodist_lnglat(0, 0, c(-180, -90, 0, 90, 180), 0, R = 1),
    c(pi, pi / 2, 0, pi / 2, pi)
  )

  expect_equal(
    geodist_lnglat(0, 0, c(-180, -90, 0, 90, 180), 0, R = 2),
    c(pi, pi / 2, 0, pi / 2, pi) * 2
  )
})

test_that("rectangle intersector works", {
  expect_identical(
    rect_intersection(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      list(xmin = 5, xmax = 15, ymin = 5, ymax = 15)
    ),
    list(xmin = 5, xmax = 10, ymin = 5, ymax = 10)
  )

  expect_identical(
    rect_intersection(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      list(xmin = 15, xmax = 25, ymin = 15, ymax = 25)
    ),
    list(xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_)
  )

  expect_identical(
    rect_intersection(
      list(xmin = NA_real_, xmax = 10, ymin = 0, ymax = 10),
      list(xmin = 15, xmax = 25, ymin = 15, ymax = 25)
    ),
    list(xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_)
  )
})

test_that("rectangle intersector predicate works", {
  expect_identical(
    rect_intersects(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      list(xmin = 5, xmax = 15, ymin = 5, ymax = 15)
    ),
    TRUE
  )

  expect_identical(
    rect_intersects(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      list(xmin = 15, xmax = 25, ymin = 15, ymax = 25)
    ),
    FALSE
  )

  expect_identical(
    rect_intersects(
      list(xmin = NA_real_, xmax = 10, ymin = 0, ymax = 10),
      list(xmin = 5, xmax = 15, ymin = 5, ymax = 15)
    ),
    NA
  )
})

test_that("rectangle contains predicate works", {
  expect_true(
    rect_contains(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      list(x = 5, y = 2)
    )
  )

  expect_false(
    rect_contains(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      list(x = 11, y = 2)
    )
  )

  expect_false(
    rect_contains(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      list(x = 5, y = 11)
    )
  )

  expect_identical(
    rect_contains(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      list(x = NA_real_, y = 2)
    ),
    NA
  )
})

test_that("rect_split_dateline() works", {
  expect_identical(
    rect_split_dateline(list(xmin = 0, xmax = 10, ymin = 0, ymax = 10)),
    list(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
    )
  )

  expect_identical(
    rect_split_dateline(list(xmin = 170, xmax = -170, ymin = 0, ymax = 10)),
    list(
      list(xmin = -180, xmax = -170, ymin = 0, ymax = 10),
      list(xmin = 170, xmax = 180, ymin = 0, ymax = 10)
    )
  )
})

test_that("rect_approx_points() works", {
  expect_identical(
    rect_approx_points(
      list(xmin = c(0, 10), xmax = c(2, 12), ymin = c(1, 11), ymax = c(3, 13)),
      n_detail = 3
    ),
    list(
      x = c(
        0, 1, 2, 0, 1, 2, 0, 1, 2,
        10, 11, 12, 10, 11, 12, 10, 11, 12
      ),
      y = c(
        1, 1, 1, 2, 2, 2, 3, 3, 3,
        11, 11, 11, 12, 12, 12, 13, 13, 13
      )
    )
  )
})

test_that("arg sanitizer works", {
  thing <- "something"
  expect_identical(vec_sanitize(thing, character()), thing)
  expect_error(vec_sanitize(thing, double()), "Can't convert `thing`")
  expect_error(vec_sanitize(thing, character(), 2), class = "vctrs_error_assert_size")
})

test_that("lng normzlier works", {
  expect_equal(normalize_lng(180), 180)
  expect_equal(normalize_lng(-180), -180)
  expect_equal(normalize_lng(-179), -179)
  expect_equal(normalize_lng(179), 179)
  expect_equal(normalize_lng(361), 1)
  expect_equal(normalize_lng(359), -1)
})
