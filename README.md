
<!-- README.md is generated from README.Rmd. Please edit that file -->

# argodata

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/ArgoCanada/argodata/workflows/R-CMD-check/badge.svg)](https://github.com/ArgoCanada/argodata/actions)
[![Codecov test
coverage](https://codecov.io/gh/ArgoCanada/argodata/branch/master/graph/badge.svg)](https://codecov.io/gh/ArgoCanada/argodata?branch=master)
<!-- badges: end -->

The goal of argodata is to provide a data frame-based interface to data
generated by the [Argo floats program](https://argo.ucsd.edu/).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ArgoCanada/argodata")
```

If you can load the package, you’re all set\!

``` r
library(argodata)
```

## Example

The argodata package downloads files from the [FTP and HTTPS
mirrors](http://www.argodatamgt.org/Access-to-data/Access-via-FTP-or-HTTPS-on-GDAC),
caches them, and loads them into R. The `argo_global_*()` functions load
the index files that list the locations of other files on the mirror:

``` r
argo_global_meta()
#> # A tibble: 16,067 x 4
#>    file                     profiler_type institution date_update        
#>    <chr>                            <dbl> <chr>       <dttm>             
#>  1 aoml/13857/13857_meta.nc           845 AO          2018-10-11 20:00:14
#>  2 aoml/13858/13858_meta.nc           845 AO          2018-10-11 20:00:15
#>  3 aoml/13859/13859_meta.nc           845 AO          2018-10-11 20:00:25
#>  4 aoml/15819/15819_meta.nc           845 AO          2018-10-11 20:00:16
#>  5 aoml/15820/15820_meta.nc           845 AO          2018-10-11 20:00:18
#>  6 aoml/15851/15851_meta.nc           845 AO          2018-10-11 20:00:26
#>  7 aoml/15852/15852_meta.nc           845 AO          2018-10-11 20:00:28
#>  8 aoml/15853/15853_meta.nc           845 AO          2018-10-11 20:00:29
#>  9 aoml/15854/15854_meta.nc           845 AO          2018-10-11 20:00:30
#> 10 aoml/15855/15855_meta.nc           845 AO          2018-10-11 20:00:34
#> # ... with 16,057 more rows
argo_global_prof()
#> # A tibble: 2,355,451 x 8
#>    file  date                latitude longitude ocean profiler_type institution
#>    <chr> <dttm>                 <dbl>     <dbl> <chr>         <dbl> <chr>      
#>  1 aoml~ 1997-07-29 20:03:00    0.267     -16.0 A               845 AO         
#>  2 aoml~ 1997-08-09 19:21:12    0.072     -17.7 A               845 AO         
#>  3 aoml~ 1997-08-20 18:45:45    0.543     -19.6 A               845 AO         
#>  4 aoml~ 1997-08-31 19:39:05    1.26      -20.5 A               845 AO         
#>  5 aoml~ 1997-09-11 18:58:08    0.72      -20.8 A               845 AO         
#>  6 aoml~ 1997-09-22 19:57:02    1.76      -21.6 A               845 AO         
#>  7 aoml~ 1997-10-03 19:15:49    2.60      -21.6 A               845 AO         
#>  8 aoml~ 1997-10-14 18:39:35    1.76      -21.6 A               845 AO         
#>  9 aoml~ 1997-10-25 19:32:34    1.80      -21.8 A               845 AO         
#> 10 aoml~ 1997-11-05 18:51:42    1.64      -21.4 A               845 AO         
#> # ... with 2,355,441 more rows, and 1 more variable: date_update <dttm>
argo_global_tech()
#> # A tibble: 15,610 x 3
#>    file                     institution date_update        
#>    <chr>                    <chr>       <dttm>             
#>  1 aoml/13857/13857_tech.nc AO          2018-10-11 20:00:14
#>  2 aoml/13858/13858_tech.nc AO          2018-10-11 20:00:15
#>  3 aoml/13859/13859_tech.nc AO          2018-10-11 20:00:25
#>  4 aoml/15819/15819_tech.nc AO          2018-10-11 20:00:16
#>  5 aoml/15820/15820_tech.nc AO          2018-10-11 20:00:18
#>  6 aoml/15851/15851_tech.nc AO          2018-10-11 20:00:26
#>  7 aoml/15852/15852_tech.nc AO          2018-10-11 20:00:28
#>  8 aoml/15853/15853_tech.nc AO          2018-10-11 20:00:29
#>  9 aoml/15854/15854_tech.nc AO          2018-10-11 20:00:30
#> 10 aoml/15855/15855_tech.nc AO          2018-10-11 20:00:34
#> # ... with 15,600 more rows
argo_global_traj()
#> # A tibble: 17,204 x 8
#>    file  latitude_max latitude_min longitude_max longitude_min profiler_type
#>    <chr>        <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
#>  1 aoml~         6.93        0.008        -15.0         -33.8            845
#>  2 aoml~         5.21       -0.363         -9.50        -17.8            845
#>  3 aoml~         5.93       -0.939        -18.6         -33.7            845
#>  4 aoml~        -2.66       -9.19         -16.4         -40.0            845
#>  5 aoml~        -1.98       -7.14          -9.90        -35.8            845
#>  6 aoml~        -2.73       -6.22           3.33        -21.1            845
#>  7 aoml~        -5.04       -8.44          -1.49        -18.9            845
#>  8 aoml~        -4.70       -8.25          -4.40        -18.0            845
#>  9 aoml~        -4.81       -7.20          -6.41        -12.7            845
#> 10 aoml~        -1.79       -6.00           7            -5.45           845
#> # ... with 17,194 more rows, and 2 more variables: institution <chr>,
#> #   date_update <dttm>
```

You can load profile data by filtering `argo_global_prof()` and piping
to `argo_prof()`:

``` r
argo_global_prof() %>% 
  tail(100) %>% 
  argo_prof()
#> # A tibble: 9,199 x 18
#>    float cycle_number date                 pres pres_qc pres_adjusted
#>    <chr>        <int> <dttm>              <dbl> <chr>           <dbl>
#>  1 nmdi~          123 2014-11-12 04:40:37     0 1                  NA
#>  2 nmdi~          123 2014-11-12 04:40:37     6 1                  NA
#>  3 nmdi~          123 2014-11-12 04:40:37    17 1                  NA
#>  4 nmdi~          123 2014-11-12 04:40:37    26 1                  NA
#>  5 nmdi~          123 2014-11-12 04:40:37    36 1                  NA
#>  6 nmdi~          123 2014-11-12 04:40:37    46 1                  NA
#>  7 nmdi~          123 2014-11-12 04:40:37    57 1                  NA
#>  8 nmdi~          123 2014-11-12 04:40:37    66 1                  NA
#>  9 nmdi~          123 2014-11-12 04:40:37    76 1                  NA
#> 10 nmdi~          123 2014-11-12 04:40:37    86 1                  NA
#> # ... with 9,189 more rows, and 12 more variables: pres_adjusted_qc <chr>,
#> #   pres_adjusted_error <dbl>, psal <dbl>, psal_qc <chr>, psal_adjusted <dbl>,
#> #   psal_adjusted_qc <chr>, psal_adjusted_error <dbl>, temp <dbl>,
#> #   temp_qc <chr>, temp_adjusted <dbl>, temp_adjusted_qc <chr>,
#> #   temp_adjusted_error <dbl>
```

If you have previously downloaded Argo data, you can use `argo_read_*()`
functions:

``` r
argo_read_global_meta("cache-dev/ar_index_global_meta.txt.gz")
#> # A tibble: 16,067 x 4
#>    file                     profiler_type institution date_update        
#>    <chr>                            <dbl> <chr>       <dttm>             
#>  1 aoml/13857/13857_meta.nc           845 AO          2018-10-11 20:00:14
#>  2 aoml/13858/13858_meta.nc           845 AO          2018-10-11 20:00:15
#>  3 aoml/13859/13859_meta.nc           845 AO          2018-10-11 20:00:25
#>  4 aoml/15819/15819_meta.nc           845 AO          2018-10-11 20:00:16
#>  5 aoml/15820/15820_meta.nc           845 AO          2018-10-11 20:00:18
#>  6 aoml/15851/15851_meta.nc           845 AO          2018-10-11 20:00:26
#>  7 aoml/15852/15852_meta.nc           845 AO          2018-10-11 20:00:28
#>  8 aoml/15853/15853_meta.nc           845 AO          2018-10-11 20:00:29
#>  9 aoml/15854/15854_meta.nc           845 AO          2018-10-11 20:00:30
#> 10 aoml/15855/15855_meta.nc           845 AO          2018-10-11 20:00:34
#> # ... with 16,057 more rows
argo_read_prof("cache-dev/dac/nmdis/2901633/profiles/R2901633_070.nc")
#> # A tibble: 95 x 28
#>    float CYCLE_NUMBER DIRECTION DATA_MODE   JULD JULD_QC JULD_LOCATION LATITUDE
#>    <chr>        <int> <chr>     <chr>      <dbl> <chr>           <dbl>    <dbl>
#>  1 nmdi~           70 A         R         23161. 1              23161.     27.9
#>  2 nmdi~           70 A         R         23161. 1              23161.     27.9
#>  3 nmdi~           70 A         R         23161. 1              23161.     27.9
#>  4 nmdi~           70 A         R         23161. 1              23161.     27.9
#>  5 nmdi~           70 A         R         23161. 1              23161.     27.9
#>  6 nmdi~           70 A         R         23161. 1              23161.     27.9
#>  7 nmdi~           70 A         R         23161. 1              23161.     27.9
#>  8 nmdi~           70 A         R         23161. 1              23161.     27.9
#>  9 nmdi~           70 A         R         23161. 1              23161.     27.9
#> 10 nmdi~           70 A         R         23161. 1              23161.     27.9
#> # ... with 85 more rows, and 20 more variables: LONGITUDE <dbl>,
#> #   POSITION_QC <chr>, PROFILE_PRES_QC <chr>, PROFILE_PSAL_QC <chr>,
#> #   PROFILE_TEMP_QC <chr>, PRES <dbl>, PRES_QC <chr>, PRES_ADJUSTED <dbl>,
#> #   PRES_ADJUSTED_QC <chr>, PRES_ADJUSTED_ERROR <dbl>, PSAL <dbl>,
#> #   PSAL_QC <chr>, PSAL_ADJUSTED <dbl>, PSAL_ADJUSTED_QC <chr>,
#> #   PSAL_ADJUSTED_ERROR <dbl>, TEMP <dbl>, TEMP_QC <chr>, TEMP_ADJUSTED <dbl>,
#> #   TEMP_ADJUSTED_QC <chr>, TEMP_ADJUSTED_ERROR <dbl>
```

Documentation for Argo variable names, units, and more are available
from the [Argo Data Management Documentation
page](http://www.argodatamgt.org/Documentation).

## Cache management

Each Argo floats mirror is home to over 250 GB of data organized in
millions of files\! The argodata package caches all files that it
downloads, which you can set to a cache that persists between sessions.
If you have a [local mirror updated using
rsync](http://www.argodatamgt.org/Access-to-data/Argo-GDAC-synchronization-service)
you can pass this directory to `argo_set_mirror()`, which can also be
used to switch between the various mirrors provided by GDAC.

``` r
argo_set_cache_dir("path/to/cache")
argo_set_mirror("ftp://usgodae.org/pub/outgoing/argo")
argo_set_mirror("path/to/local/argo")
```

By default, index files are cached for 24 hours and data files are
cached indefinitely. These defaults reflect a balance between the
considerable time required to download new files and the frequency with
which files are updated on the mirror. To override these defaults you
can pass `download = TRUE` to functions that load data, or set
`options(argodata.max_data_cache_age = 3)`, where the number is in
hours. Use `Inf` to always use the cached version of a file.

``` r
argo_global_prof(download = TRUE)
options(argodata.max_global_cache_age = Inf)
options(argodata.max_data_cache_age = Inf)
```
