#' change_detection
#'
#' This function gives back a raster layer of the difference between two raster images (eg. surface changes during a year)
#' output: raster layer
#' @param jul the first raster you want to use (newer date)
#' @param apr the second raster you want to use (older date)
#' @details this function does xxx
#' 
#' @keywords change detection, raster layer, remote sensing
#' @export
#' @examples


area_change <- function(jul,apr) {
  change <- jul-apr
  return(change)
}


