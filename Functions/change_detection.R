#function change
CHANGE <- function(jul,apr) {
  change <- jul-apr
  return(change)
}

#change detection
for (row in (1:nrow(csv_change))) {
  jul <- brick(raster(as.character(csv_change[row,"JULY"])))
  apr <- brick(raster(as.character(csv_change[row,"APRIL"])))
  result <- CHANGE(jul, apr)
  plot(result)
  name <- paste0('change_minus_', csv_change [row, "ï..YEAR"], '.tif')
  #writeRaster(result, filename = name , format="GTiff",overwrite=TRUE)
  print(paste0("processed image: ", name))
}
