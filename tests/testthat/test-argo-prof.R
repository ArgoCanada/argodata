
test_that("argo_prof_levels() works", {
  with_argo_mirror(argo_test_mirror(), {
    prof <- argo_prof_levels("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
    expect_true(all(c("cycle_number", "pres", "temp") %in% names(prof)))
    expect_true(all(prof$cycle_number == 0))

    prof <- argo_prof_levels(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("pres", "temp", "empty"),
      quiet = TRUE
    )

    expect_identical(names(prof), c("float", "cycle_number", "date", "pres", "temp"))
  })
})

test_that("argo_prof_prof() works", {
  with_argo_example_cache({
    prof <- argo_prof_prof("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
    expect_true(all(c("cycle_number", "latitude", "longitude") %in% names(prof)))
    expect_true(all(prof$cycle_number == 0))
    expect_identical(nrow(prof), 1L)

    prof <- argo_prof_prof(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("longitude", "latitude", "empty"),
      quiet = TRUE
    )

    expect_identical(names(prof), c("float", "longitude", "latitude"))
  })
})

test_that("argo_prof_history() works", {
  with_argo_example_cache({
    prof <- argo_prof_history("dac/csio/2900313/profiles/D2900313_000.nc", quiet = TRUE)
    expect_true(all(c("cycle_number", "history_qctest") %in% names(prof)))
    expect_true(all(prof$cycle_number == 0))

    prof <- argo_prof_history(
      "dac/csio/2900313/profiles/D2900313_000.nc",
      vars = c("history_qctest", "empty"),
      quiet = TRUE
    )

    expect_identical(names(prof), c("float", "cycle_number", "date", "history_qctest"))
  })
})

test_that("argo_read_prof_levels() works", {
  expect_is(
    argo_read_prof_levels(
      system.file(
        "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_prof() works", {
  expect_is(
    argo_read_prof_prof(
      system.file(
        "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_history() works", {
  expect_is(
    argo_read_prof_history(
      system.file(
        "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})


test_that("argo_read_prof_levels() works for bio profiles", {
  expect_is(
    argo_read_prof_levels(
      system.file(
        "cache-test/dac/csio/2902746/profiles/BR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_prof() works for bio profiles", {
  expect_is(
    argo_read_prof_prof(
      system.file(
        "cache-test/dac/csio/2902746/profiles/BR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_history() works for bio profiles", {
  expect_is(
    argo_read_prof_history(
      system.file(
        "cache-test/dac/csio/2902746/profiles/BR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_levels() works for synthetic profiles", {
  expect_is(
    argo_read_prof_levels(
      system.file(
        "cache-test/dac/csio/2902746/profiles/SR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_prof() works for synthetic profiles", {
  expect_is(
    argo_read_prof_prof(
      system.file(
        "cache-test/dac/csio/2902746/profiles/SR2902746_001.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_prof_history() errors properly for synthetic profiles", {
  expect_error(
    argo_read_prof_history(
      system.file(
        "cache-test/dac/csio/2902746/profiles/SR2902746_001.nc",
        package = "argodata"
      )
    ),
    "is missing dimension"
  )
})

test_that("assert_argo_prof_file() works", {
  expect_identical(
    assert_argo_prof_file("dac/aoml/3901309/profiles/R3901309_001D.nc"),
    "dac/aoml/3901309/profiles/R3901309_001D.nc"
  )
  expect_identical(
    assert_argo_prof_file("dac/csio/2902746/profiles/BR2902746_001.nc"),
    "dac/csio/2902746/profiles/BR2902746_001.nc"
  )
  expect_identical(
    assert_argo_prof_file("dac/csio/2902746/profiles/SR2902746_001.nc"),
    "dac/csio/2902746/profiles/SR2902746_001.nc"
  )
  expect_error(assert_argo_prof_file("not a file"), "Found 1 invalid Argo profile path")
})
