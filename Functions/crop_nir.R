#cropping NIR bands
#CROP_NIR <- function(e, crop_nir, name){}
for (row in (1:nrow(raw_data))) {
  e <- extent(650000, 750000, -2130000, -2040000) #predefined new extent
  crop_nir <- crop(raster(as.character(raw_data[row, "NIR"])), extent(e))
  plot(crop_nir)
  name <- paste0('crop_NIR_', raw_data[row, "ï..DATE"], '.tif')
  writeRaster(crop_nir, filename = name , format="GTiff",overwrite=TRUE)
}