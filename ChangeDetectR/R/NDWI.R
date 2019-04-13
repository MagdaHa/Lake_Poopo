#' NDWI
#'
#' This function calculates the NDWI (Normaized Difference Vegetation Index) for Satellite image
#' output: raster layer
#' @param green green band as input
#' @param nir nir band as input

#' 
#' @details this function does xxx
#' @keywords Landsat, bands, NDWI, water index, remote sensing
#' @export
#' @examples


NDWI <- function(green,nir) {
  ndwi <- (green-nir)/(green+nir)
  return(ndwi)
}