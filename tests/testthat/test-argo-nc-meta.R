
test_that("argo_nc_meta_read_config_param() works for meta files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_meta.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_meta_read_config_param(nc)
  expect_true(
    all(
      c("float", "CONFIG_MISSION_NUMBER", "CONFIG_PARAMETER_NAME",  "CONFIG_PARAMETER_VALUE")
      %in% names(nc_all)
    )
  )

  ncdf4::nc_close(nc)
})

test_that("argo_nc_meta_read_missions() works for meta files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_meta.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_meta_read_missions(nc)
  expect_true(
    all(
      c("float", "CONFIG_MISSION_NUMBER", "CONFIG_MISSION_COMMENT")
      %in% names(nc_all)
    )
  )

  ncdf4::nc_close(nc)
})

test_that("argo_nc_meta_read_trans_system() works for meta files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_meta.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_meta_read_trans_system(nc)
  expect_true(
    all(
      c("float", "TRANS_SYSTEM", "TRANS_SYSTEM_ID", "TRANS_FREQUENCY")
      %in% names(nc_all)
    )
  )

  ncdf4::nc_close(nc)
})


test_that("argo_nc_meta_read_positioning_system() works for meta files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_meta.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_meta_read_positioning_system(nc)
  expect_true(
    all(
      c("float", "POSITIONING_SYSTEM")
      %in% names(nc_all)
    )
  )

  ncdf4::nc_close(nc)
})

test_that("argo_nc_meta_read_launch_config_param() works for meta files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_meta.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_meta_read_launch_config_param(nc)
  expect_true(
    all(
      c("float", "LAUNCH_CONFIG_PARAMETER_NAME", "LAUNCH_CONFIG_PARAMETER_VALUE")
      %in% names(nc_all)
    )
  )

  ncdf4::nc_close(nc)
})

test_that("argo_nc_meta_read_sensor() works for meta files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_meta.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_meta_read_sensor(nc)
  expect_true(
    all(
      c("float", "SENSOR", "SENSOR_MAKER", "SENSOR_MODEL", "SENSOR_SERIAL_NO")
      %in% names(nc_all)
    )
  )

  ncdf4::nc_close(nc)
})

test_that("argo_nc_meta_read_param() works for meta files", {
  nc <- ncdf4::nc_open(
    system.file(
      "cache-test/dac/csio/2900313/2900313_meta.nc",
      package = "argodata"
    )
  )

  nc_all <- argo_nc_meta_read_param(nc)
  expect_true(
    all(
      c(
        "float",
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

  ncdf4::nc_close(nc)
})
