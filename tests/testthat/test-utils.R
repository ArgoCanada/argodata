
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
