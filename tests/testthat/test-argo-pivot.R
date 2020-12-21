
test_that("argo_pivot_longer() works", {
  tbl <- tibble::tibble(
    n_prof = c(1L, 1L),
    cycle_number = c(1L, 1L),
    date = as.POSIXct(c("2018-05-24 05:44:42", "2018-05-24 05:44:42"), tz = "UTC"),
    n_levels = 1:2,
    pres = c(0.20, 0.89),
    c1phase_doxy = c(35.31, 35.36),
    c1phase_doxy_qc = c("0", "0"),
    c2phase_doxy = c(5.61, 5.60),
    c2phase_doxy_qc = c("0", "0"),
    temp_doxy = c(30.50, 30.33),
    temp_doxy_qc = c("1", "1"),
    doxy = c(196.66, 196.38),
    doxy_qc = c("3", "3"),
    doxy_adjusted = c(196.66, 196.38),
    doxy_adjusted_qc = c("1", "1"),
    doxy_adjusted_error = c(4.67, 4.67)
  )

  longer <- argo_pivot_longer(tbl, n_levels)
  spec <- argo_pivot_longer_spec(tbl)

  expect_identical(colnames(longer), c("n_levels", "variable", names(spec)[-1]))
  expect_equal(nrow(longer), nrow(tbl) * nrow(spec))
  expect_identical(longer$n_levels, rep(1:2, nrow(spec)))
  expect_identical(longer$variable, rep(spec$.variable, each = nrow(tbl)))
  expect_identical(
    longer$value,
    c(
      196.66, 196.38,
      35.31, 35.36,
      5.61, 5.60,
      30.50, 30.33
    )
  )

  # zero row input should have same shape as n-row output
  expect_identical(
    argo_pivot_longer(tbl[integer(0), ], n_levels),
    longer[integer(0), ]
  )

  # zero variable output should have same columns as n-variable output
  expect_identical(
    argo_pivot_longer(tbl["n_levels"], n_levels),
    longer[integer(0), ]
  )
})

test_that("argo_pivot_longer_spec() works", {

  tbl <- tibble::tibble(
    file = character(0),
    n_prof = integer(0),
    cycle_number = integer(0),
    date = as.POSIXct(character()),
    n_levels = integer(0),
    pres = numeric(0),
    c1phase_doxy = numeric(0),
    c1phase_doxy_qc = character(0),
    c2phase_doxy = numeric(0),
    c2phase_doxy_qc = character(0),
    temp_doxy = numeric(0),
    temp_doxy_qc = character(0),
    doxy = numeric(0),
    doxy_qc = character(0),
    doxy_adjusted = numeric(0),
    doxy_adjusted_qc = character(0),
    doxy_adjusted_error = numeric(0)
  )

  spec <- argo_pivot_longer_spec(tbl)

  expect_identical(spec$.variable, c("doxy", "c1phase_doxy", "c2phase_doxy", "temp_doxy"))
  expect_identical(spec$value, spec$.variable)
  expect_identical(spec$value_qc, paste0(spec$value, "_qc"))
  expect_identical(spec$value_adjusted, c("doxy_adjusted", NA, NA, NA))
  expect_identical(spec$value_adjusted_qc, c("doxy_adjusted_qc", NA, NA, NA))
  expect_identical(spec$value_adjusted_error, c("doxy_adjusted_error", NA, NA, NA))

  expect_equal(nrow(argo_pivot_longer_spec(tibble::tibble())), 0)
})
