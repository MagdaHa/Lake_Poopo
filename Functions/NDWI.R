#calculating NDWI for each image
#function NDWI
NDWI <- function(green,nir) {
  ndwi <- (green-nir)/(green+nir)
  return(ndwi)
}