#Change Vector Analysis
#CVA <- function(april_raster, July_raster, cva){}
for (row in (1:nrow(csv_change))) {
  april_raster <- brick(as.character(csv_change[row,"APRIL"]))[[1:2]]
  july_raster <- brick(as.character(csv_change[row,"JULY"]))[[1:2]]
  cva <- rasterCVA(april_raster, july_raster)
  plot(cva)
  name <- paste0('CVA_', csv_change [row, "YEAR"], '.tif')
  #writeRaster(cva, filename = name , format="GTiff",overwrite=TRUE)
  #savePlot(filename=name, type = "tiff", device = dev.cur(), restoreConsole = T) ##??????????????????
  print(paste0("processed image: ", name))
}