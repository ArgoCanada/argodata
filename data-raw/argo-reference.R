
library(tidyverse)
library(rvest)

# https://archimer.ifremer.fr/doc/00187/29825/68654.pdf
# https://doi.org/10.13155/29825

# argo_reference_data_type (Reference table 1) ----

argo_reference_data_type <- tibble(
  data_type = c(
    "Argo profile", "Argo trajectory", "Argo meta-data", "Argo technical data",
    "B-Argo profile", "B-Argo trajectory", "Argo profile merged", "Argo trajectory merged",
    "Argo synthetic profile"
  ),
  data_type_is_obsolete = c(F, F, F, F, F, T, T, T, F)
)

# argo_reference_qc_flag (Reference table 2) ----

prof_doc <- read_html("https://argo.ucsd.edu/data/how-to-use-argo-files/")
argo_reference_qc_flag <- html_table(prof_doc, header = TRUE)[[1]] %>%
  rename_all(str_to_lower) %>%
  rename_all(str_replace_all, " ", "_") %>%
  mutate_all(na_if, "") %>%
  transmute(
    qc_flag = as.character(qc_flag),
    qc_description = meaning,
    qc_comment_realtime = real_time_comment,
    qc_comment_delayed = delayed_mode_comment
  ) %>%
  as_tibble()


# argo_reference_institution (Reference table 4) ----

argo_reference_institution <- tibble::tribble(
  ~institution,                              ~institution_description,
          "AO",                                    "AOML, USA",
          "BO",                         "BODC, United Kingdom",
          "CI",          "Institute of Ocean Sciences, Canada",
          "CS",                             "CSIRO, Australia",
          "GE",                                 "BSH, Germany",
          "GT", "GTS : used for data coming from WMO GTS network",
          "HZ", "CSIO, China Second Institute of Oceanography",
          "IF",                              "Ifremer, France",
          "IN",                                "INCOIS, India",
          "JA",                                   "JMA, Japan",
          "JM",                               "Jamstec, Japan",
          "KM",                                   "KMA, Korea",
          "KO",                                 "KORDI, Korea",
          "LV", "Laboratoire Océanographique de Villefranche,",
          "MB",                                   "MBARI, USA",
          "ME",                                 "MEDS, Canada",
          "NA",                                    "NAVO, USA",
          "NM",                                 "NMDIS, China",
          "PM",                                    "PMEL, USA",
          "RU",                                       "Russia",
          "SI",                            "SIO, Scripps, USA",
          "SP",                                        "Spain",
          "UW",                "University of Washington, USA",
          "VL",        "Far Eastern Regional  Hydrometeorological Research Institute of Vladivostock, Russia",
          "WH",    "Woods Hole Oceanographic Institution, USA"
  )

# argo_reference_location_class (Reference table 5) ----

argo_reference_location_class <- tibble::tribble(
  ~location_class,                                                                                   ~location_class_description,
             "0",                                                                  "Argos accuracy estimation over 1500m radius",
             "1",                                                           "Argos accuracy estimation better than 1500m radius",
             "2",                                                           "Argos accuracy estimation better than 500 m radius",
             "3",                                                           "Argos accuracy estimation better than 250 m radius",
             "A",                                                           "Argos no accuracy estimation (3 messages received)",
             "B",                                                      "Argos no accuracy estimation (1 or 2 messages received)",
             "Z",                                                                                       "Argos invalid location",
             "G",                                                                  "GPS positioning accuracy (better than 10 m)",
             "I",                                                                          "Iridium accuracy (better than 5 km)",
             "D",                                                                           "Beidou accuracy (better than 10 m)",
             "E",                                                                          "GLONASS accuracy (better than 10 m)",
             "F",                                                                          "GALILEO accuracy (better than 10 m)",
             "H",                                                                             "GNSS accuracy (better than 10 m)",
             "R",                                                                                               "RAFOS accuracy",
             "U",  "Estimated position. Accuracy not sent back by the float. An estimated accuracy may be in AXES_ERROR_ELLIPSE"
  )

# argo_reference_history_action (Reference table 7) ----

argo_reference_history_action <- tibble::tribble(
  ~history_action,                                                                ~history_action_description,
                  "CF",                                                                "Change a quality flag",
                  "CR",                                                                        "Create record",
                  "CV",                                                                         "Change value",
                  "DC",                                   "Station was checked by duplicate checking software",
                  "ED",                                                               "Edit a parameter value",
                  "IP",                             "This history group operates on the complete input record",
                  "NG",                                                                        "No good trace",
                  "PE", "Position error. Profile position has been erroneously encoded. Corrected if possible",
                  "QC",                                                                      "Quality Control",
                "QCF$",                                                                         "Tests failed",
                "QCP$",                                                                       "Test performed",
                  "SV",                                                                          "Set a value",
                  "TE",    "Time error. Profile date/time has been erroneously encoded. Corrected if possible",
                  "UP",                                            "Station passed through the update program"
  )


# argo_reference_profiler (Reference table 8)

argo_reference_profiler <- str_trim("
831 P-Alace float
837 Arvor-C float
838 Arvor-D float
839 Provor-II float
840 Provor, no conductivity
841 Provor, Seabird conductivity sensor
842 Provor, FSI conductivity sensor
843 POPS ice Buoy/Float
844 Arvor, Seabird conductivity sensor
845 Webb Research, no conductivity
846 Webb Research, Seabird sensor
847 Webb Research, FSI sensor
848 Apex-EM float
849 Apex-D deep float
850 Solo, no conductivity
851 Solo, Seabird conductivity sensor
852 Solo, FSI conductivity sensor
853 Solo2, Seabird conductivity sensor
854 S2A float
855 Ninja, no conductivity sensor
856 Ninja, SBE conductivity sensor
857 Ninja, FSI conductivity sensor
858 Ninja, TSK conductivity sensor
859 Profiling Float, NEMO, no conductivity
860 Profiling Float, NEMO, SBE conductivity sensor
861 Profiling Float, NEMO, FSI conductivity sensor
862 Solo-D deep float
863 Navis-A Float
864 Ninja-D deep float
865 Nova float
") %>%
  read_lines() %>%
  str_split_fixed(" ", n = 2) %>%
  as.data.frame() %>%
  set_names(c("profiler_type", "profiler_description")) %>%
  mutate(profiler_type = as.numeric(profiler_type)) %>%
  as_tibble()

# argo_reference_positioning_system (Reference table 9) ----

argo_reference_positioning_system <- str_trim("
ARGOS ARGOS positioning system
GPS GPS positioning system
RAFOS RAFOS positioning system
IRIDIUM Iridium positioning system
BEIDOU Beidou navigation satellite system
GLONASS GLONASS navigation satellite system
GALILEO Galileo navigation satellite system
GNSS Global Navigation Satellite System
NONE For profile file only: if an estimated position is based on two or more positioning systems, or if the estimation does not rely on information from positioning systems
") %>%
  read_lines() %>%
  str_split_fixed(" ", n = 2) %>%
  as.data.frame() %>%
  set_names(c("positioning_system", "positioning_system_description")) %>%
  as_tibble()


# argo_reference_history_qctest (Reference table 11) ----

argo_reference_history_qctest <- str_trim("
1 2 Platform Identification test
2 4 Impossible Date test
3 8 Impossible Location test
4 16 Position on Land test
5 32 Impossible Speed test
6 64 Global Range test
7 128 Regional Global Parameter test
8 256 Pressure Increasing test
9 512 Spike test
10 1024 Top and Bottom Spike test (obsolete)
11 2048 Gradient test
12 4096 Digit Rollover test
13 8192 Stuck Value test
14 16384 Density Inversion test
15 32768 Grey List test
16 65536 Gross Salinity or Temperature Sensor Drift test
17 131072 Visual QC test
18 261144 Frozen profile test
19 524288 Deepest pressure test
20 1048576 Questionable Argos position test
21 2097152 Near-surface unpumped CTD salinity test
22 4194304 Near-surface mixed air/water test
") %>%
  read_lines() %>%
  str_split_fixed(" ", n = 3) %>%
  as.data.frame() %>%
  set_names(c("history_qctest_number", "history_qctest_flag", "history_qctest_description")) %>%
  mutate(
    history_qctest_number = as.integer(history_qctest_number),
    history_qctest_flag = as.integer(history_qctest_flag),
    history_qctest = sprintf("%X", history_qctest_flag)
  ) %>%
  as_tibble()

# argo_reference_history_step (Reference table 12) ----

argo_reference_history_step <- str_trim("
ARFM Convert raw data from telecommunications system to a processing format
ARGQ Automatic QC of data reported in real-time has been performed
IGO3 Checking for duplicates has been performed
ARSQ Delayed mode QC has been performed
ARCA Calibration has been performed
ARUP Real-time data have been archived locally and sent to GDACs
ARDU Delayed mode data have been archived locally and sent to GDACs
RFMT Reformat software to convert hexadecimal format reported by the buoy to our standard format
COOA Coriolis objective analysis performed
") %>%
  read_lines() %>%
  str_split_fixed(" ", n = 2) %>%
  as.data.frame() %>%
  set_names(c("history_step", "history_step_description")) %>%
  as_tibble()

# write data ----

usethis::use_data(argo_reference_data_type, overwrite = TRUE)
usethis::use_data(argo_reference_qc_flag, overwrite = TRUE)
usethis::use_data(argo_reference_institution, overwrite = TRUE)
usethis::use_data(argo_reference_location_class, overwrite = TRUE)
usethis::use_data(argo_reference_history_action, overwrite = TRUE)
usethis::use_data(argo_reference_profiler, overwrite = TRUE)
usethis::use_data(argo_reference_positioning_system, overwrite = TRUE)
usethis::use_data(argo_reference_history_qctest, overwrite = TRUE)
usethis::use_data(argo_reference_history_step, overwrite = TRUE)
