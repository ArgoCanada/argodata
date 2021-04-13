
test_that("argo_read_many() can silence the extract message", {
  with_argo_example_cache({
    expect_message(
      argo_read_many(
        assert_argo_prof_file,
        argo_read_prof_levels,
        path = "dac/aoml/5906206/profiles/BD5906206_016.nc",
        vars = NULL,
        download = FALSE,
        quiet = FALSE
      ),
      "Extracting from 1 file"
    )

    expect_silent(
      argo_read_many(
        assert_argo_prof_file,
        argo_read_prof_levels,
        path = "dac/aoml/5906206/profiles/BD5906206_016.nc",
        vars = NULL,
        download = FALSE,
        quiet = TRUE
      )
    )
  })
})
