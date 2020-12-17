
#' Argo data type reference
#'
#' This table contains the acceptable values for the `data_type` field in
#' Argo NetCDF files. This field can be examined using [argo_info()].
#' See Reference Table 1 in the Argo User's Manual.
#'
#' @section References:
#' Argo User's Manual, November 2019. <https://doi.org/10.13155/29825>
#'
#' @examples
#' with_argo_example_cache({
#'   argo_info("dac/csio/2900313/2900313_meta.nc", vars = "data_type")
#' })
#'
#' argo_reference_data_type
#'
"argo_reference_data_type"

#' Argo QC flag reference
#'
#' This table contains the quality control descriptions of the one-character
#' flags that appear in variables that end with `_qc`. These variables appear
#' in [argo_prof_levels()], [argo_prof_prof()], and [argo_traj_measurement()]. See
#' also [argo_reference_history_qctest][argo_reference_history_qctest]
#' for information about specific tests.
#'
#' @inheritSection argo_reference_data_type References
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_prof_levels(
#'     "dac/csio/2902746/profiles/BR2902746_001.nc",
#'     vars = c("pres", "pres_qc", "doxy", "doxy_qc")
#'   ) %>%
#'     dplyr::left_join(
#'       argo_reference_qc_flag,
#'       by = c("doxy_qc" = "qc_flag")
#'     )
#' })
#'
#' argo_reference_qc_flag
#'
"argo_reference_qc_flag"

#' Argo institution code reference
#'
#' Contains a longer names of the institution abbreviations
#' in [argo_global_meta()] and other index files.
#'
#' @inheritSection argo_reference_data_type References
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_global_meta() %>%
#'     select(file, institution) %>%
#'     left_join(argo_reference_institution, by = "institution")
#' })
#'
#' argo_reference_institution
#'
"argo_reference_institution"

#' Argo location class reference
#'
#' @inheritSection argo_reference_data_type References
#'
#' @examples
#' argo_reference_location_class
#'
"argo_reference_location_class"

#' Argo history action reference
#'
#' This table contains descriptions for the codes used in the `history_action`
#' column present in [argo_prof_history()] and [argo_traj_history()].
#'
#' @inheritSection argo_reference_data_type References
#'
#' @examples
#' library(dplyr, warn.conflicts = TRUE)
#'
#' with_argo_example_cache({
#'   argo_prof_history(
#'     "dac/csio/2902746/profiles/BR2902746_001.nc",
#'     vars = "history_action"
#'   ) %>%
#'     left_join(
#'       argo_reference_history_action,
#'       by = "history_action"
#'     )
#' })
#'
#' argo_reference_history_action
#'
"argo_reference_history_action"

#' Argo profiler reference
#'
#' Contains a reference for values in the `profiler_type` column in the
#' [argo_global_meta()].
#'
#' @inheritSection argo_reference_data_type References
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_global_meta() %>%
#'     select(file, profiler_type) %>%
#'     left_join(argo_reference_profiler, by = "profiler_type")
#' })
#'
#' argo_reference_profiler
#'
"argo_reference_profiler"

#' Argo positioning system reference
#'
#' Contains a reference for values in the `positioning_system` column of
#' [argo_meta_positioning_system()].
#'
#' @inheritSection argo_reference_data_type References
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_meta_positioning_system("dac/csio/2900313/2900313_meta.nc") %>%
#'     left_join(argo_reference_positioning_system, by = "positioning_system")
#' })
#'
#' argo_reference_positioning_system
#'
"argo_reference_positioning_system"

#' Argo QC test reference
#'
#' Contains a reference for values in the `history_qctest` column of
#' [argo_prof_history()] and [argo_traj_history()]. The values in this
#' column are a hexadecimal representation of the sum of the `qctest_value`
#' column. See [argo_unnest_history_qctest()] to generate the values needed
#' to join this table.
#'
#' @inheritSection argo_reference_data_type References
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_prof_history(
#'     "dac/csio/2902746/profiles/BR2902746_001.nc",
#'     vars = "history_qctest"
#'   ) %>%
#'     argo_unnest_history_qctest() %>%
#'     left_join(argo_reference_history_qctest, by = "history_qctest") %>%
#'     select(history_qctest_description, everything())
#' })
#'
#' argo_reference_history_qctest
#'
"argo_reference_history_qctest"

#' Argo history step reference
#'
#' Contains a reference for values in the `history_test` column of
#' [argo_prof_history()] and [argo_traj_history()].
#'
#' @inheritSection argo_reference_data_type References
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' with_argo_example_cache({
#'   argo_prof_history(
#'     "dac/csio/2902746/profiles/BR2902746_001.nc",
#'     vars = "history_step"
#'   ) %>%
#'     left_join(argo_reference_history_step, by = "history_step")
#' })
#'
#' argo_reference_history_step
#'
"argo_reference_history_step"
