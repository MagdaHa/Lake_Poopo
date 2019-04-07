#extract water area
#defining class intervals
#from <- c(-1, -0.5, 0, 0.1)
#to <- c(-0.5, 0, 0.1, 1)
#becomes <- c(0, 0, 0, 1)
#classDef <- cbind(from, to, becomes)

#water_area <- function(newClasses, name){}
for (row in (1:nrow(csv_class))) {
  newClasses <- reclassify (raster(as.character(csv_class[row,"NDWI"])), rcl = classDef)
  plot(newClasses)
  name <- paste0('class_', csv_class [row, "DATE"], '.tif')
  #writeRaster(newClasses, filename = name , format="GTiff",overwrite=TRUE)
  print(paste0("processed image: ", name))
}