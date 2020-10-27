
library(tidyverse)
library(glue)

doc_from_vars <- . %>%
  mutate(
    units = if_else(units != "", glue("{ units }, "), "")
  ) %>%
  with(glue("- `{ name }` ({ units }{ prec }): { longname }")) %>%
  str_wrap(width = 70) %>%
  str_replace_all("\n", "\n#'    ") %>%
  paste0("#' ", ., collapse = "\n")

# ---- profile data ----

prof_file <- system.file(
  "cache-test/dac/csio/2900313/profiles/D2900313_000.nc",
  package = "argodata"
)
prof_nc <- ncdf4::nc_open(prof_file)

prof_vars_levels <- argo_nc_prof_vars_levels(prof_nc)
prof_vars_prof <- argo_nc_prof_vars_prof(prof_nc)
prof_vars_history <- argo_nc_prof_vars_history(prof_nc)

argo_vars("dac/csio/2900313/profiles/D2900313_000.nc", vars = prof_vars_levels) %>%
  doc_from_vars() %>%
  clipr::write_clip()

argo_vars("dac/csio/2900313/profiles/D2900313_000.nc", vars = prof_vars_prof) %>%
  doc_from_vars() %>%
  clipr::write_clip()

argo_vars("dac/csio/2900313/profiles/D2900313_000.nc", vars = prof_vars_history) %>%
  doc_from_vars() %>%
  clipr::write_clip()

# ---- trajectory data ----

traj_file <- system.file(
  "cache-test/dac/csio/2900313/2900313_Rtraj.nc",
  package = "argodata"
)
traj_nc <- ncdf4::nc_open(traj_file)

traj_vars_meas <- argo_nc_traj_vars_meas(traj_nc)
traj_vars_cycle <- argo_nc_traj_vars_cycle(traj_nc)
traj_vars_history <- argo_nc_traj_vars_history(traj_nc)

argo_vars("dac/csio/2900313/2900313_Rtraj.nc", vars = traj_vars_meas) %>%
  doc_from_vars() %>%
  clipr::write_clip()

argo_vars("dac/csio/2900313/2900313_Rtraj.nc", vars = traj_vars_cycle) %>%
  doc_from_vars() %>%
  clipr::write_clip()

argo_vars("dac/csio/2900313/2900313_Rtraj.nc", vars = traj_vars_history) %>%
  doc_from_vars() %>%
  clipr::write_clip()
