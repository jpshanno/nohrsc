# Look-up automatic path if path not supplied
nsa_path <-
  function(path = ""){
    if(path != ""){
      # Add checks for existence & write access
      return(path)
    }

    # rappdirs

    stop("path must be specified, future versions will allows automatic storage.")
  }

# Check for internet connection
check_connection <-
  function(){
    if(!curl::has_internet()){
      stop("You must have access to an active internet connection to run this function.")
    }
  }

# Get product number from common name
look_up_product <-
  function(product){

    product <-
      tolower(product)

    good_prods <-
      c("liquid precipitation",
        "snow precipitation",
        "snow water equivalent",
        "snow depth",
        "snow pack average temperature",
        "blowing snow sublimation",
        "snow melt",
        "snow pack sublimation")

    if(!all(product %in% good_prods)){
      stop("product must be specified as a character vector of one or more of '", paste(good_prods, collapse = "', '"), "'")
    }

    vapply(product,
           switch,
           character(1),
           "liquid precipitation" = "1025",
           "snow precipitation" = "1025",
           "snow water equivalent" = "1034",
           "snow depth" = "1036",
           "snow pack average temperature" = "1038",
           "blowing snow sublimation" = "1039",
           "snow melt" = "1044",
           "snow pack sublimation" = "1050",
           USE.NAMES = FALSE)
  }

# Format supplied date
format_date <-
  function(start.date, end.date = NULL){

    stopifnot(is.character(start.date) | lubridate::is.timepoint(start.date))

    if(is.character(start.date)){
      if(!all(grepl("\\d{4}-\\d{1,2}-\\d{1,2}", start.date))){
        stop("date must be a character written as 'YYYY-MM-DD' or a datetime object")
      }
    }

    if(is.character(start.date)){
      start.date <-
        lubridate::ymd(start.date)
    }

    if(is.null(end.date)){
      date <- start.date
    } else {

      stopifnot(is.character(end.date) | lubridate::is.timepoint(end.date))

      if(is.character(end.date)){
        if(!all(grepl("\\d{4}-\\d{1,2}-\\d{1,2}", end.date))){
          stop("date must be a character written as 'YYYY-MM-DD' or a datetime object")
        }
      }

      if(is.character(end.date)){
        end.date <-
          lubridate::ymd(end.date)
      }

      date <-
        seq(start.date, end.date, 1)
    }

    lapply(date,
           function(date){
             year <-
               format(date, "%Y")

             month <-
               format(date, "%m")

             month_abb <-
               format(date, "%b")

             mday <-
               format(date, "%d")

             c(year = year,
               month = month,
               month_abb = month_abb,
               mday = mday,
               longdate = paste0(year, month, mday))
           }
    )
  }

# Create ftp url for download
build_url <-
  function(region, nsa_date){

    # Add tests:
    # identical("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/unmasked/2010/02_Feb/SNODAS_unmasked_20100208.tar", build_url(date = "2010-02-08"))
    # ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/unmasked/2010/02_Feb/SNODAS_unmasked_20100208.tar
    # identical("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/2010/04_Apr/SNODAS_20100404.tar", build_url(region = "CONUS", date = "2010-04-04"))
    # ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/2010/04_Apr/SNODAS_20100404.tar


    base_url <-
      "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/"

    folder <-
      ifelse(region == "CONUS",
             "masked",
             "unmasked")

    url_params <-
      vapply(nsa_date,
             function(date){
               paste0(folder, "/",
                      date["year"], "/",
                      date["month"], "_", date["month_abb"], "/",
                      "SNODAS_", ifelse(folder == "masked", "", paste0(folder, "_")), date["longdate"], ".tar")
             },
             character(1))


    paste0(base_url, url_params)
  }

#
# build_basefile <-
#   function(product){
#     # Use product info & date to build archive & raster file basename
#     # Build urls for all combinations of data at once
#     # dates <- 1:10
#     # products <- letters[1:3]
#     # paste(expand.grid(dates, products)[[1]], expand.grid(dates, products)[[2]], sep = ".")
#
#     product <-
#       look_up_product(product)
#
#   }

get_hdr_info <-
  function(param, hdr_lines){
    match.arg(param,
              c("nbits", "nrows", "ncols", "xmp", "ymp", "xdim", "ydim"))

    match_string <-
      switch(param,
             nbits = "(?<=Data bytes per pixel: )[0-9]*",
             ncols = "(?<=Number of columns)[0-9]*",
             nrows = "(?<=Number of rows)[0-9]*",
             xmp = "(?<=Benchmark x-axis coordinate)[0-9]*",
             ymp = "(?<=Benchmark y-axis coordinate)[\\-0-9]*",
             xdim = "(?<=X-axis resolution)[0-9]*",
             ydim = "(?<=Y-axis resolution)[0-9]*")

    hdr_line <-
      grep(match_string, hdr_lines, value = TRUE, perl = TRUE)

    param_match <-
      regexec("(?<=: ).*$", hdr_line, perl = TRUE)

    regmatches(hdr_line, param_match)[[1]]
  }

# Create .hdr file for each extracted raster
create_hdr <-
  function(file){
    # THIS NEEDS TO BE UPDATED TO GET ulxmap, ulymap, xdim, ydim FROM SNODAS HEADER
    # OR AT LEAST CHECKED THAT IT IS ALWAYS THE SAME VALUE

    hdr_file <-
      sub("bil$", "hdr", file)

    txt_file <-
      sub("bil$", "txt", file)

    hdr_lines <-
      unlist(lapply(txt_file, readLines))

    NBITS <-
      8*as.numeric(get_hdr_info("nbits", hdr_lines))
    NCOLS <-
      get_hdr_info("ncols", hdr_lines)
    NROWS <-
      get_hdr_info("nrows", hdr_lines)
    ULXMP <-
      get_hdr_info("xmp", hdr_lines)
    ULYMP <-
      get_hdr_info("ymp", hdr_lines)
    XDIM <-
      get_hdr_info("xdim", hdr_lines)
    YDIM <-
      get_hdr_info("ydim", hdr_lines)

    # Need to confirm that all rows were found for each .txt file

    hdr_content <-
      mapply(function(NBITS, NCOLS, NROWS, ULXMP,
                      ULYMP, XDIM, YDIM){
        c("byteorder M",
          "layout bil",
          "nbands 1",
          paste("nbits", NBITS),
          paste("ncols", NCOLS),
          paste("nrows", NROWS),
          paste("ulxmap", ULXMP),
          paste("ulymap", ULYMP),
          paste("xdim", XDIM),
          paste("ydim", YDIM))},
        NBITS, NCOLS, NROWS, ULXMP, ULYMP, XDIM, YDIM,
        SIMPLIFY = FALSE)

    mapply(function(hdr_content, hdr_file, txt_file){
      written <-
        writeLines(hdr_content,
                   hdr_file)

      deleted <-
        unlink(txt_file)},
      hdr_content,
      hdr_file,
      txt_file)

  }

# Create .prj file for each extracted raster
create_prj <-
  function(file){
    prj_file <-
      sub("bil$", "prj", file)

    prj_content <-
      'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]'

    prj_out <-
      writeLines(text = prj_content,
                 prj_file)
  }

# download_archive <-
#   function(){
#
#   }

# Extract selected files from archive
extract_archive <-
  function(archive, product){
    combos <-
      expand.grid(archive = archive,
                  product = product,
                  stringsAsFactors = FALSE)

    extracted_files <-
      mapply(
        function(archive, product){

          longdate <-
            sub(".*(\\d{8}).tar", "\\1", archive, perl = TRUE)

          region <-
            ifelse(grepl("SNODAS_unmasked_", archive),
                   "North America",
                   "CONUS")

          files <-
            build_filenames(product, longdate, region)

          tar_results <-
           untar(archive,
                         files = files[1:2],
                         tar = "internal",
                         exdir = dirname(archive))

          unzipped_files <-
            vapply(file.path(dirname(archive), files[1:2]),
                   gunzip,
                   character(1),
                   overwrite = TRUE,
                   USE.NAMES = FALSE)

          bil_file <-
            sub(".dat$", ".bil", unzipped_files[1])

          renames <-
            file.rename(unzipped_files[1],
                        bil_file)

          bil_file
        },
        combos[["archive"]],
        combos[["product"]],
        USE.NAMES = FALSE)

    c(extracted_files)
  }

# Build filenames for each product/archive
build_filenames <-
  function(product, longdate, region){
    # rr_mmmffppppSvvvvTttttaaaaTSyyyymmddhhIP00Z.xxx.gz

    # change parameter name to nsa_date

    # combos <- expand.grid(product = product,
    #                       date = nsa_date,
    #                       stringsAsFactors = FALSE)

    # lapply(product,
    #        function(product){
             rr <- ifelse(region == "CONUS", "us", "zz")

             mmm <- "ssm"

             ff <- ifelse(grepl("precipitation$", product),
                          "v0",
                          "v1")

             pppp <- look_up_product(product)

             S <- ifelse(grepl("precipitation$", product),
                         "S",
                         "")

             vvvv <- switch(product,
                            "liquid precipitation" = "lL00",
                            "snow precipitation" = "lL01",
                            "snow water equivalent" = "tS__",
                            "snow depth" = "tS__",
                            "snow pack average temperature" = "wS__",
                            "blowing snow sublimation" = "lL00",
                            "snow melt" = "bS__",
                            "snow pack sublimation" = "lL00")

             Ttttt <- switch(product,
                             "liquid precipitation" = "T0024",
                             "snow precipitation" = "T0024",
                             "snow water equivalent" = "T0001",
                             "snow depth" = "T0001",
                             "snow pack average temperature" = "A0024",
                             "blowing snow sublimation" = "T0024",
                             "snow melt" = "T0024",
                             "snow pack sublimation" = "T0024")

             aaaa <- "TTNA"

             TSyyyymmddhh <- paste0("TS", longdate, "05")

             I <- ifelse(Ttttt == "T0001", "H", "D")

             P00Z <- ifelse(product %in% c("liquid precipitation",
                                           "snow precipitation",
                                           "snow water equivalent",
                                           "snow depth",
                                           "snow pack average temperature"),
                            "P001",
                            "P000")

             basename <-
               paste0(rr, "_", mmm, ff, pppp, S, vvvv, Ttttt, aaaa, TSyyyymmddhh, I, P00Z)

               c(datgz = paste0(basename, ".dat.gz"),
                 txtgz = paste0(basename, ".txt.gz")
                 # ,dat = paste0(basename, ".dat"),
                 # txt = paste0(basename, ".txt")
                 # ,bil = paste0(basename, ".bil"),
                 # hdr = paste0(basename, ".hdr"),
                 # prj = paste0(basename, ".prj")
                 )
           # }
# )
  }
