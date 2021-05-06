
test_that("argo_path_info() works", {
  info_meta <- argo_path_info("dac/csio/2900313/2900313_meta.nc")
  expect_identical(info_meta$file_type, "meta")
  expect_identical(info_meta$file_float, "2900313")

  info_tech <- argo_path_info("dac/csio/2900313/2900313_tech.nc")
  expect_identical(info_tech$file_type, "tech")
  expect_identical(info_tech$file_float, "2900313")

  info_traj <- argo_path_info("dac/csio/2902746/2902746_BRtraj.nc")
  expect_identical(info_traj$file_type, "traj")
  expect_identical(info_traj$file_float, "2902746")
  expect_identical(info_traj$file_data_mode, "R")
  expect_identical(info_traj$file_modifier, "B")

  info_prof <- argo_path_info("dac/csio/2902746/2902746_Sprof.nc")
  expect_identical(info_prof$file_type, "prof")
  expect_identical(info_prof$file_float, "2902746")
  expect_identical(info_prof$file_modifier, "S")

  info_prof_single <- argo_path_info("dac/csio/2902746/profiles/BR2902746_003.nc")
  expect_identical(info_prof_single$file_type, "prof")
  expect_identical(info_prof_single$file_float, "2902746")
  expect_identical(info_prof_single$file_data_mode, "R")
  expect_identical(info_prof_single$file_modifier, "B")
  expect_identical(info_prof_single$file_cycle, 3L)
})

test_that("argo_path_info() identifies file type and float for indexes", {
  meta_info <- argo_path_info(argo_global_meta(quiet = TRUE))
  expect_true(all(!is.na(meta_info$file_float)))
  expect_true(all(meta_info$file_type == "meta"))

  tech_info <- argo_path_info(argo_global_tech(quiet = TRUE))
  expect_true(all(!is.na(tech_info$file_float)))
  expect_true(all(tech_info$file_type == "tech"))

  traj_info <- argo_path_info(argo_global_traj(quiet = TRUE))
  expect_true(all(!is.na(traj_info$file_float)))
  expect_true(all(traj_info$file_type == "traj"))
  expect_setequal(traj_info$file_data_mode, c("R", "D"))

  bio_traj_info <- argo_path_info(argo_global_bio_traj(quiet = TRUE))
  expect_true(all(!is.na(bio_traj_info$file_float)))
  expect_true(all(bio_traj_info$file_type == "traj"))
  expect_true(all(bio_traj_info$file_modifier == "B"))

  skip_if_not(Sys.getenv("R_ARGO_CACHE_DIR") != "")

  prof_info <- argo_path_info(argo_global_prof(quiet = TRUE))
  expect_true(all(!is.na(prof_info$file_float)))
  expect_true(all(prof_info$file_type == "prof"))
  expect_true(all(!is.na(prof_info$file_cycle)))
  expect_setequal(prof_info$file_data_mode, c("R", "D"))
  expect_true(all(is.na(prof_info$file_modifier)))

  bio_prof_info <- argo_path_info(argo_global_bio_prof(quiet = TRUE))
  expect_true(all(!is.na(bio_prof_info$file_float)))
  expect_true(all(bio_prof_info$file_type == "prof"))
  expect_true(all(!is.na(bio_prof_info$file_cycle)))
  expect_setequal(bio_prof_info$file_data_mode, c("R", "D"))
  expect_true(all(bio_prof_info$file_modifier == "B"))

  synthetic_prof_info <- argo_path_info(argo_global_synthetic_prof(quiet = TRUE))
  expect_true(all(!is.na(synthetic_prof_info$file_float)))
  expect_true(all(synthetic_prof_info$file_type == "prof"))
  expect_true(all(!is.na(synthetic_prof_info$file_cycle)))
  expect_setequal(synthetic_prof_info$file_data_mode, c("R", "D"))
  expect_true(all(synthetic_prof_info$file_modifier == "S"))
})

test_that("argo_extract_path_info() works", {
  with_argo_example_cache({
    expect_identical(
      argo_extract_path_info(argo_global_meta(quiet = TRUE)),
      vctrs::vec_cbind(
        argo_path_info(argo_global_meta(quiet = TRUE)),
        argo_global_meta(quiet = TRUE)[-1]
      )
    )
  })
})

test_that("as_argo_path() works", {
  expect_identical(as_argo_path("a path"), "a path")
  expect_identical(as_argo_path(data.frame(file = "a path")), "dac/a path")
  expect_error(as_argo_path(4), "must be a character vector")
  expect_identical(as_argo_path(character(0)), character(0))
  expect_identical(as_argo_path(data.frame(file = character(0))), character(0))
})

test_that("as_argo_path_aux() works", {
  expect_identical(as_argo_path_aux("dac/a file.nc"), "aux/a file_aux.nc")
  # shouldn't change an existing aux path
  expect_identical(as_argo_path_aux("aux/a file_aux.nc"), "aux/a file_aux.nc")
})
