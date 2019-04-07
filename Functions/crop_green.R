#cropping GREEN bands
#CROP_GREEN <- function(e, crop_green, name){}
for (row in (1:nrow(raw_data))) {
  e <- extent(650000, 750000, -2130000, -2040000)
  crop_green <- crop(raster(as.character(raw_data[row, "GREEN"])), extent(e))
  plot(crop_green)
  name <- paste0('crop_GREEN_', raw_data[row, "ï..DATE"], '.tif')
  writeRaster(crop_green, filename = name , format="GTiff",overwrite=TRUE)
  print(paste0("processed image: ", name))
}