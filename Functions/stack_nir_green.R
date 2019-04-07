#stack NIR and GREEN images together
#STACK <- function(stack, name){}
for (row in (1:nrow(csv_stack))) {
  stack <- stack(raster(as.character(csv_stack[row,"GREEN"])), raster(as.character(csv_stack[row,"NIR"])))
  plot(stack)
  name <- paste0('stack_', csv_stack [row, "ï..DATE"], '.tif')
  writeRaster(stack, filename = name , format="GTiff",overwrite=TRUE)
  print(paste0("processed image: ", name))
}