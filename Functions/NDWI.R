#calculating NDWI for each image
#function NDWI
NDWI <- function(green,nir) {   #band 3 and 5
  ndwi <- (green-nir)/(green+nir)
  return(ndwi)
}

for (row in (1:nrow(csv_NDWI))) {
  nir <- brick(raster(as.character(csv_NDWI[row,"NIR"])))
  green <- brick(raster(as.character(csv_NDWI[row,"GREEN"])))
  result <- NDWI(green, nir)
  plot(result)
  name <- paste0('NDWI_', csv_NDWI [row, "ï..DATE"], '.tif')
  #writeRaster(result, filename = name , format="GTiff",overwrite=TRUE)
  print(paste0("processed image: ", name))
}