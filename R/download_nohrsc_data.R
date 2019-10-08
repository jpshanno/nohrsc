download_nohrsc <- 
  function(date, destination = NULL, product = "ssmv11044bS"){
    
    date <- 
      lubridate::ymd(date)
    
    year <- 
      format(date, "%Y")
    
    month <- 
      format(date, "%m")
    
    month_abb <- 
      format(date, "%b")
    
    day <- 
      format(date, "%d")
    
    long_date <- 
      paste0(year, month, day)
    
    base_url <- 
      "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/"
    
    url_query <- 
      paste0(year, "/", month, "_", month_abb, "/SNODAS_", long_date, ".tar")
    
    url <-
      paste0(base_url, url_query)
    
    if(is.null(destination)){
      destination <- getwd()
    } else {
      stopifnot(dir.exists(destination))
    }
    
    # This would be a better approach in a package
    # destination <- 
    # here::here("data", "nohrsc")
    
    archive <- 
      regmatches(url, regexpr("SNODAS_[0-9]{8}.tar$", url))
    
    # This should download and extract in a temp directory and then copy the 
    # final files to the destination
    
    dest_file <- 
      file.path(destination, archive)
    
    raster_dat <- 
      file.path(destination, 
                paste0("us_", product, "__T0024TTNATS", long_date, "05DP000.dat"))
    
    raster_bil <- 
      sub(".dat", ".bil", raster_dat, fixed = TRUE)
    
    raster_archive <- 
      paste0(raster_dat, ".gz")
    
    hdr_file <- 
      sub(".dat", ".hdr", raster_dat, fixed = TRUE)
    
    prj_file <- 
      sub(".dat", ".prj", raster_dat, fixed = TRUE)
    
    # Download file
    if(!any(file.exists(dest_file), 
            file.exists(raster_dat), 
            file.exists(raster_bil), 
            file.exists(raster_archive))){
      download.file(url, dest_file)
    }
    
    # Extract Archive
    if(!any(file.exists(raster_dat), 
            file.exists(raster_bil), 
            file.exists(raster_archive)) &
       file.exists(dest_file)){
      untar(dest_file,
            files = basename(raster_archive),
            exdir = destination)
    }
    
    # Decompress raster
    if(!any(file.exists(raster_dat), 
            file.exists(raster_bil)) &
       file.exists(raster_archive)){
      R.utils::gunzip(raster_archive)
    }
    
    # https://www.nohrsc.noaa.gov/archived_data/instructions.html        
    # Rename .dat to .bil
    if(!file.exists(raster_bil) &
       file.exists(raster_dat)){
      file_renamed <- 
        file.rename(raster_dat,
                    raster_bil)
    }
    
    # Create .hdr header
    if(!file.exists(hdr_file)){
      create_hdr(hdr_file)
    }
    
    # Create prj file
    if(!file.exists(prj_file)){
      prj_content <-
        'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]'
      
      writeLines(prj_content,
                 con = prj_file)
    }
    
    unlink(dest_file)
  }
