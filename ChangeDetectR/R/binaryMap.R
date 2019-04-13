#' binaryMap
#'
#' This function calculates a binary map with the values 1 and 0 by setting a threshold.
#' output: raster layer
#' @param raster the raster you want to create a binary map out of (e.g index raste: NDVI, NDWI)
#' @param threshold defined threshold where each pixel gets assigned to one class
#' 
#' @details this function does xxx
#' @keywords threshold, index calculation, remote sensing
#' @export
#' @examples

binaryMap <- function(raster, threshold) {
  
  bin <- raster
  bin[raster <= threshold] <- 0
  bin[raster > threshold] <- 1
  
  return(bin)
}