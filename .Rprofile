
if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile")
}

# sets the argo cache dir environment variable when in development mode
if (file.exists("DESCRIPTION") && any(grepl("Package: argodata", readLines("DESCRIPTION")))) {
  Sys.setenv(R_ARGO_CACHE_DIR = file.path(getwd(), "cache-dev"))
}
