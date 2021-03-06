% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argo-cache.R, R/argo-mirror.R
\name{argo_cache_dir}
\alias{argo_cache_dir}
\alias{argo_update_global}
\alias{argo_update_data}
\alias{argo_set_cache_dir}
\alias{with_argo_cache_dir}
\alias{with_argo_example_cache}
\alias{argo_cached}
\alias{argo_test_mirror}
\title{Get and set the default cache directory}
\usage{
argo_cache_dir()

argo_update_global(max_global_cache_age = -Inf, all = TRUE, quiet = FALSE)

argo_update_data(max_data_cache_age = -Inf, quiet = FALSE)

argo_set_cache_dir(cache_dir)

with_argo_cache_dir(cache_dir, expr)

with_argo_example_cache(expr)

argo_cached(path)

argo_test_mirror()
}
\arguments{
\item{max_global_cache_age}{The maximum age in hours
to keep cached files. Use \code{Inf} to always use cached files; use \code{-Inf}
to always force download. You can set the default values of these using
\code{options(argodata.max_global_cache_age = ...)}
and/or \code{options(argodata.max_data_cache_age = ...)}.}

\item{all}{Should index files be downloaded even if they have not been
previously downloaded?}

\item{quiet}{Use \code{FALSE} to show which files are downloaded and for more
verbose error messages.}

\item{max_data_cache_age}{The maximum age in hours
to keep cached files. Use \code{Inf} to always use cached files; use \code{-Inf}
to always force download. You can set the default values of these using
\code{options(argodata.max_global_cache_age = ...)}
and/or \code{options(argodata.max_data_cache_age = ...)}.}

\item{cache_dir}{A writable directory in which downloaded files can be
cached.}

\item{expr}{An expression to be evaluated with the specified
default \code{cache_dir}.}

\item{path}{A path relative to the root directory of
\code{\link[=argo_mirror]{argo_mirror()}} or \code{\link[=argo_cache_dir]{argo_cache_dir()}}. This value can also
be a data.frame with a \code{file} column (e.g., a global index as
returned by \code{\link[=argo_global_meta]{argo_global_meta()}} and others).}
}
\value{
\itemize{
\item \code{argo_cache_dir()}: The directory where cache files are located
\item \code{argo_set_cache_dir()}: The previously set cache directory
\item \code{with_argo_cache_dir()}, \code{with_argo_example_cache()}: The result of
\code{expr}.
\item \code{argo_cached()}: The file path to the cached version of the Argo file
(which may or may not exist).
\item \code{argo_update_data()}, \code{argo_update_global()}: The locations of the
updated files.
}
}
\description{
The cache directory stores previously downloaded files to access them
more quickly. The cache directory stores files in the same format as a
an \code{\link[=argo_mirror]{argo_mirror()}}. By default, the cache is stored in a temporary
directory that is cleared when the session is restarted. This ensures
access to the latest index and files by default.
}
\examples{
argo_cache_dir()

temp_dir <- tempfile()
with_argo_cache_dir(temp_dir, argo_cache_dir())
unlink(temp_dir, recursive = TRUE)

}
