
test_that("argo default mirror can be set and fetched", {
  with_argo_mirror(NULL, {
    expect_identical(argo_mirror(), "ftp://ftp.ifremer.fr/ifremer/argo")
    expect_identical(getOption("argodata.mirror"), NULL)

    with_argo_mirror("https://data-argo.ifremer.fr/", {
      expect_identical(argo_mirror(), "https://data-argo.ifremer.fr")
    })
  })
})
