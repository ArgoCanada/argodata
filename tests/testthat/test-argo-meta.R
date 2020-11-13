
test_that("argo_meta_config_param() works", {
  with_argo_example_cache({
    meta <- argo_meta_config_param("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("float", "config_mission_number", "config_parameter_name", "config_parameter_value") %in%
          names(meta)
      )
    )
  })
})

test_that("argo_meta_missions() works", {
  with_argo_example_cache({
    meta <- argo_meta_missions("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("float", "config_mission_number", "config_mission_comment") %in%
          names(meta)
      )
    )
  })
})

test_that("argo_meta_trans_system() works", {
  with_argo_example_cache({
    meta <- argo_meta_trans_system("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("float", "trans_system", "trans_system_id", "trans_frequency") %in%
          names(meta)
      )
    )
  })
})

test_that("argo_meta_launch_config_param() works", {
  with_argo_example_cache({
    meta <- argo_meta_launch_config_param("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("float", "launch_config_parameter_name", "launch_config_parameter_value") %in%
          names(meta)
      )
    )
  })
})

test_that("argo_meta_sensor() works", {
  with_argo_example_cache({
    meta <- argo_meta_sensor("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("float", "sensor", "sensor_maker", "sensor_model", "sensor_serial_no") %in%
          names(meta)
      )
    )
  })
})

test_that("argo_meta_param() works", {
  with_argo_example_cache({
    meta <- argo_meta_param("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("float", "parameter", "parameter_sensor",
          "parameter_units", "parameter_accuracy", "parameter_resolution",
          "predeployment_calib_equation", "predeployment_calib_coefficient",
          "predeployment_calib_comment") %in%
          names(meta)
      )
    )
  })
})


test_that("argo_read_meta_config_param() works", {
  expect_is(
    argo_read_meta_config_param(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_missions() works", {
  expect_is(
    argo_read_meta_missions(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_trans_system() works", {
  expect_is(
    argo_read_meta_trans_system(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_launch_config_param() works", {
  expect_is(
    argo_read_meta_launch_config_param(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_sensor() works", {
  expect_is(
    argo_read_meta_sensor(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})

test_that("argo_read_meta_param() works", {
  expect_is(
    argo_read_meta_param(
      system.file(
        "cache-test/dac/csio/2900313/2900313_meta.nc",
        package = "argodata"
      )
    ),
    "tbl_df"
  )
})



