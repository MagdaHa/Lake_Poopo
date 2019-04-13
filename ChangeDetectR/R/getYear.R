#' getYear
#'
#' This function returns the recording date (only year, yyyy) on an satellite raster image based on the original file name (only for Landsat 4, 5 and 8).
#' output: numeric value
#' @param file path where the image is stored
#' @details this function does xxx
#' 
#' @keywords date, raster layer, remote sensing
#' @export
#' @examples


getYear <- function(file) {
  years <- as.character(c(1950:2020))
  for (i in 1:length(years)){
    if (grepl(years[i], file)) {
      return(years[i])
    }
  }
}