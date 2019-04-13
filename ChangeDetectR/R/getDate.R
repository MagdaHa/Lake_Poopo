#' getDate
#'
#' This function returns the recording date (only year and month, yyyymm) on an satellite raster image based on the original file na,e (only for Landsat 4, 5 and 8).
#' output: numeric value
#' @param file path where the image is stored
#' @details this function does xxx
#' 
#' @keywords date, raster layer, remote sensing
#' @export
#' @examples


getDate <- function(file) {
  date <- as.character(c(195004:202007))
  for (i in 1:length(date)){
    if (grepl(date[i], file)) {
      return(date[i])
    }
  }
}