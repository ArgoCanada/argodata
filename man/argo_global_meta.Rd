% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argo-global.R
\name{argo_global_meta}
\alias{argo_global_meta}
\alias{argo_global_prof}
\alias{argo_global_tech}
\alias{argo_global_traj}
\alias{argo_read_global_meta}
\alias{argo_read_global_prof}
\alias{argo_read_global_tech}
\alias{argo_read_global_traj}
\title{Load Argo global index files}
\usage{
argo_global_meta(download = NULL, quiet = TRUE)

argo_global_prof(download = NULL, quiet = TRUE)

argo_global_tech(download = NULL, quiet = TRUE)

argo_global_traj(download = NULL, quiet = TRUE)

argo_read_global_meta(file)

argo_read_global_prof(file)

argo_read_global_tech(file)

argo_read_global_traj(file)
}
\arguments{
\item{download}{A logical vector indicating whether or not
a file should be downloaded. Defaults to the value of
\code{\link[=argo_should_download]{argo_should_download()}}, which is \code{TRUE} for files that
do not exist in the cache or global files that were cached more than 24
hours ago.}

\item{quiet}{Use \code{FALSE} to show which files are downloaded and for more
verbose error messages.}

\item{file}{A path to a previously downloaded index file.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}}.
}
\description{
Load Argo global index files
}