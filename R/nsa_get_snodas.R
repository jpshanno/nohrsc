#' Download and extract national snow anlaysis data
#'
#' @param product A character vector of one or more prodcuts available via
#'   NOHRSC Acceptable values are "liquid precipitation", "snow precipitation",
#'   "snow water equivalent", "snow depth", "snow pack average temperature",
#'   "blowing snow sublimation", "snow melt", and "snow pack sublimation"
#' @param start.date A start date supplied as a character string ("YYYY-MM-DD")
#'   or a datetime object
#' @param end.date NULL if only one date is wanted, otherwise the end date of a
#'   time span formated as start.date
#' @param region Either "CONUS" or "North America" indicating if the results
#'   should be contained to the continental US or not
#' @param path The output path to save the raster data
#' @param delete.archive A logical indicating if the downloaded archives be
#'   deleted after the rasters have been extracted. Deafults to FALSE
#' @param force.download Not currently used
#'
#' @return A charcter vector of the extracted filenames
#'
#' @references \url{https://nsidc.org/data/g02158}
#' @export
#'
#' @examples
#' \dontrun{
#' raster_files <-
#'   nsa_get_snodas(c("snow water equivalent", "snow melt"),
#'                  start.date = "2010-01-06",
#'                  end.date = "2010-01-08",
#'                  path = "snodas")
#' }
nsa_get_snodas <-
  function(product,
           start.date,
           end.date = NULL,
           region = "North America",
           path = "",
           delete.archive = FALSE,
           force.download = FALSE){

    stopifnot(region %in% c("CONUS", "North America"))

    stopifnot(is.character(region))

    date <-
      format_date(start.date, end.date)

    # Would check for existing data here and then download and extract

    url <-
      build_url(region, date)

    archive_file <-
      file.path(nsa_path(path), regmatches(url, regexpr("SNODAS_.*.tar$", url)))

    if(sum(!file.exists(archive_file)) != 0){
      check_connection()
      utils::download.file(url,
                           archive_file)
    }

    product_files <-
      extract_archive(archive_file, product)

    hdr <- create_hdr(product_files)
    prj <- lapply(product_files, create_prj)

    if(delete.archive){
      removed <- lapply(archive_file,
                        unlink)
    }
    product_files
  }
