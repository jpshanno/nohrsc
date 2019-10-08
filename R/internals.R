check_connection <-
  function(){
    if(!curl::has_internet()){
      stop("You must have access to an active internet connection to run this function.")
    }
  }

look_up_product <-
  function(prod){

    prod <-
      toupper(prod)

    good_prods <-
      c("SWE", "MELT")

    nohrsc_code <-
      switch (prod,
              'SWE' = action,
              'MELT' = )
  }

build_url <-
  function(region, prod, date){

    stopifnot(region %in% c("CONUS", "North America"))

    if(region == "CONUS"){
      folder <- "masked"
    } else {
      folder <- "unmasked"
    }

    prod <-
      look_up_product(prod)


    # Build urls for all combinations of data at once
    dates <- 1:10
    products <- letters[1:3]
    paste(expand.grid(dates, products)[[1]], expand.grid(dates, products)[[2]], sep = ".")
  }

build_basefile <-
  function(prod){
    # Use product info & date to build archive & raster file basename
  }

create_hdr <-
  function(base_file){
    # basefile should be the raster file name with no extension
    file <-
      paste0(base_file, ".hdr")

    hdr_content <-
      c("byteorder M",
        "layout bil",
        "nbands 1",
        "nbits 16",
        "ncols 6935",
        "nrows 3351",
        "ulxmap -124.729583333333",
        "ulymap 52.8704166666666",
        "xdim 0.00833333333333333",
        "ydim 0.00833333333333333")

    write_file(hdr_content,
               file)
  }
