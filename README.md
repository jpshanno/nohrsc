
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nohrsc

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.org/jpshanno/nohrsc.svg?branch=master)](https://travis-ci.org/jpshanno/nohrsc)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/jpshanno/nohrsc?branch=master&svg=true)](https://ci.appveyor.com/project/jpshanno/nohrsc)
[![Coverage
Status](https://img.shields.io/codecov/c/github/jpshanno/nohrsc/master.svg)](https://codecov.io/github/jpshanno/nohrsc?branch=master)
<!-- badges: end -->

nohrsc is designed to make it easy to access the NOHRSC data archive. It
takes common name arguments and dates to construct the proper URLs and
access the correctly formatted file names. Multiple days and products
can be downloaded and extracted in a single call. The extracted rasters
will be converted to \*.bil format and an appropriate \*.hdr and \*.prj
will be created for each raster.

## Installation

If you are using Windows you should install nohrsc by downloading the
binary version (see below) of the package. Otherwise you can build
`nohrsc` from these source files.

**Windows Installation**

``` r
url = "https://drive.google.com/file/d/1blFGzzeLpTuuKM_Bfq812CIGQ9m88VH2/view?usp=sharing"
fn = tempfile(fileext=".zip")
download.file(destfile = fn, url = url)
install.packages(fn, repos = NULL)
```

**`remotes` Linux Installation**

``` r
remotes::install_github("jpshanno/nohrsc")
```

or without `remotes` by **Linux Installation without `remotes` package**

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

## Development Timeline

### Minimum Viable Product

  - [x] Download data (use \*apply to make \>1 easy)
  - [ ] Automatic data handling if path not specified (via rappdirs)
  - [ ] Query/Load downloaded data
  - [ ] Add citations to NOHRSC data & documentation
  - [ ] Unit tests  
  - [ ] Use assertthat to check archive/raster filenames etc

### First Improvement

  - [ ] Improve queries
  - [ ] Delete data

### Second Improvement

  - [ ] Allow clipping before storage to save disk space
  - [ ] [Change NA values being recorded as large positive
    integers](https://www.nohrsc.noaa.gov/archived_data/instructions.html)

## Example

Download data over three days and extract SWE and melt data:

``` r
library(nohrsc)
rasters <- 
  nsa_get_snodas(product = c("snow water equivalent", "snow melt"),
                 start.date = "2015-04-01",
                 end.date = "2015-04-03",
                 path = "snodas")
```
