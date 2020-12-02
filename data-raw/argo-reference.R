
library(tidyverse)
library(rvest)

# https://archimer.ifremer.fr/doc/00187/29825/68654.pdf
# https://doi.org/10.13155/29825

# argo_reference_data_type (Reference table 1) ----

argo_reference_data_type <- tibble(
  name = c(
    "Argo profile", "Argo trajectory", "Argo meta-data", "Argo technical data",
    "B-Argo profile", "B-Argo trajectory", "Argo profile merged", "Argo trajectory merged",
    "Argo synthetic profile"
  ),
  is_obsolete = c(F, F, F, F, F, T, T, T, F)
)

# argo_reference_qc (Reference table 2) ----

prof_doc <- read_html("https://argo.ucsd.edu/data/how-to-use-argo-files/")
argo_reference_qc <- html_table(prof_doc, header = TRUE)[[1]] %>%
  rename_all(str_to_lower) %>%
  rename_all(str_replace_all, " ", "_") %>%
  mutate_all(na_if, "") %>%
  as_tibble()


# argo_reference_institution ----

argo_reference_institution <- tibble::tribble(
  ~institution,                              ~institution_long,
          "AO",                                    "AOML, USA",
          "BO",                         "BODC, United Kingdom",
          "CI",          "Institute of Ocean Sciences, Canada",
          "CS",                             "CSIRO, Australia",
          "GE",                                 "BSH, Germany",
          "GT", "GTS : used for data coming from WMO GTS netw",
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



# write data ----

usethis::use_data(argo_reference_data_type, overwrite = TRUE)
usethis::use_data(argo_reference_qc, overwrite = TRUE)
usethis::use_data(argo_reference_institution, overwrite = TRUE)
