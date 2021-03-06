% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{argo_reference_institution}
\alias{argo_reference_institution}
\title{Argo institution code reference}
\format{
An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 25 rows and 2 columns.
}
\usage{
argo_reference_institution
}
\description{
Contains a longer names of the institution abbreviations
in \code{\link[=argo_global_meta]{argo_global_meta()}} and other index files.
}
\section{References}{

Argo User's Manual, November 2019. \doi{10.13155/29825}
}

\examples{
library(dplyr, warn.conflicts = FALSE)

with_argo_example_cache({
  argo_global_meta() \%>\%
    select(file, institution) \%>\%
    left_join(argo_reference_institution, by = "institution")
})

argo_reference_institution

}
\keyword{datasets}
