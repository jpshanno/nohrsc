
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nohrsc

<!-- badges: start -->

<!-- badges: end -->

The goal of nohrsc is to …

## Minimum Viable Product

  - Download data (use \*apply to make \>1 easy)
  - Query/Load downloaded data
  - Delete data

## First Improvement

  - Force redownload (delete/overwrite existing data)
  - Improve queries

## Second Improvement

  - Allow clipping before storage to save disk space

## To Do

  - \[ \] Add tests  
  - \[ \] Use assertthat to check archive/raster filenames etc

## Installation

You can install nohrsc using `remotes` by

``` r
remotes::install_github("jpshanno/nohrsc")
```

or without `remotes` by

``` r
nohrsc_source <- file.path(tempdir(), "nohrsc-master.zip")
download.file("https://github.com/jpshanno/nohrsc/archive/master.zip",
              nohrsc_source)
unzip(nohrsc_source,
        exdir = dirname(nohrsc_source))
install.packages(sub(".zip$", "", nohrsc_source),
                 repos = NULL,
                 type = "source")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(nohrsc)
## basic example code
```
