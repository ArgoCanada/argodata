
test_that("as_argo_float() works", {
  expect_identical(as_argo_float(data.frame(file = "aoml/1234")), "aoml/1234")
  expect_identical(as_argo_float("dac/aoml/1234"), "aoml/1234")
  expect_identical(as_argo_float("aoml/1234"), "aoml/1234")
  expect_identical(with_argo_mirror(argo_test_mirror(), as_argo_float("13857")), "aoml/13857")
  expect_identical(as_argo_float(NA_character_), NA_character_)
  expect_identical(as_argo_float(character(0)), character(0))

  expect_error(as_argo_float("not an ID"), "invalid float ID")
  expect_error(as_argo_float(4), "must be a character vector")
})
