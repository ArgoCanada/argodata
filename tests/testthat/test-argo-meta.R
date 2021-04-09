
test_that("argo_meta_config_param() works", {
  with_argo_example_cache({
    meta <- argo_meta_config_param("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("config_parameter_name", "config_parameter_value") %in%
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
        c("config_mission_number", "config_mission_comment") %in%
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
        c("trans_system", "trans_system_id", "trans_frequency") %in%
          names(meta)
      )
    )
  })
})

test_that("argo_meta_positioning_system() works", {
  with_argo_example_cache({
    meta <- argo_meta_positioning_system("dac/csio/2900313/2900313_meta.nc", quiet = TRUE)
    expect_true(
      all(
        c("positioning_system") %in%
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
        c("launch_config_parameter_name", "launch_config_parameter_value") %in%
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
        c("sensor", "sensor_maker", "sensor_model", "sensor_serial_no") %in%
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
        c("parameter", "parameter_sensor",
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

test_that("argo_read_meta_trans_system() works", {
  expect_is(
    argo_read_meta_positioning_system(
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


test_that("argo_read_meta_config_param() works for meta files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_meta.nc",
    package = "argodata"
  )

  nc_all <- argo_read_meta_config_param(nc)
  expect_true(
    all(
      c("CONFIG_PARAMETER_NAME",  "CONFIG_PARAMETER_VALUE")
      %in% names(nc_all)
    )
  )
})

test_that("argo_read_meta_missions() works for meta files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_meta.nc",
    package = "argodata"
  )

  nc_all <- argo_read_meta_missions(nc)
  expect_true(
    all(
      c("CONFIG_MISSION_NUMBER", "CONFIG_MISSION_COMMENT")
      %in% names(nc_all)
    )
  )
})

test_that("argo_read_meta_trans_system() works for meta files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_meta.nc",
    package = "argodata"
  )

  nc_all <- argo_read_meta_trans_system(nc)
  expect_true(
    all(
      c("TRANS_SYSTEM", "TRANS_SYSTEM_ID", "TRANS_FREQUENCY")
      %in% names(nc_all)
    )
  )
})


test_that("argo_read_meta_positioning_system() works for meta files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_meta.nc",
    package = "argodata"
  )

  nc_all <- argo_read_meta_positioning_system(nc)
  expect_true(
    all(
      c("POSITIONING_SYSTEM")
      %in% names(nc_all)
    )
  )
})

test_that("argo_read_meta_launch_config_param() works for meta files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_meta.nc",
    package = "argodata"
  )

  nc_all <- argo_read_meta_launch_config_param(nc)
  expect_true(
    all(
      c("LAUNCH_CONFIG_PARAMETER_NAME", "LAUNCH_CONFIG_PARAMETER_VALUE")
      %in% names(nc_all)
    )
  )
})

test_that("argo_read_meta_sensor() works for meta files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_meta.nc",
    package = "argodata"
  )

  nc_all <- argo_read_meta_sensor(nc)
  expect_true(
    all(
      c("SENSOR", "SENSOR_MAKER", "SENSOR_MODEL", "SENSOR_SERIAL_NO")
      %in% names(nc_all)
    )
  )
})

test_that("argo_read_meta_param() works for meta files", {
  nc <- system.file(
    "cache-test/dac/csio/2900313/2900313_meta.nc",
    package = "argodata"
  )

  nc_all <- argo_read_meta_param(nc)
  expect_true(
    all(
      c(
        "PARAMETER",
        "PARAMETER_SENSOR",
        "PARAMETER_UNITS",
        "PARAMETER_ACCURACY",
        "PARAMETER_RESOLUTION",
        "PREDEPLOYMENT_CALIB_EQUATION",
        "PREDEPLOYMENT_CALIB_COEFFICIENT",
        "PREDEPLOYMENT_CALIB_COMMENT"
      ) %in% names(nc_all)
    )
  )
})
