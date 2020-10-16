
test_that("argo default mirror can be set and fetched", {
  with_argo_mirror(NULL, {
    expect_identical(argo_mirror(), "https://data-argo.ifremer.fr")
    expect_identical(getOption("argodata.mirror"), NULL)

    with_argo_mirror("ftp://ftp.ifremer.fr/ifremer/argo", {
      expect_identical(argo_mirror(), "ftp://ftp.ifremer.fr/ifremer/argo")
    })
  })
})

test_that("argo_set_mirror() converts paths to file:// urls", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  expect_match(with_argo_mirror(temp_dir, argo_mirror()), "^file://")

  unlink(temp_dir)
})

test_that("argo_set_mirror() errors for bad URLs", {
  expect_error(argo_set_mirror("not a mirror"), "must be a valid URL")
  expect_error(argo_set_mirror(1L), "must be a character vector")
})
