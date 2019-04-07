#water area in July
#AREA_JULY <- function(jul_area, name){}
area_july <- data.frame()
for (row in (1:nrow(csv_change))) {
  jul_area <- calc_water_area(brick(raster(as.character(csv_change[row,"JULY"]))))
  #apr_area <- calc_water_area(brick(raster(as.character(csv_change[row,"APRIL"]))))
  name <- paste0(as.character(csv_change[row, "ï..YEAR"]), "-07-01")
  area_july[row, 1] <- name
  area_july[row, 2] <- jul_area
  print(paste0("processed image: ", name))
}
names(area_july) <- c("YEAR", "JULY")
write.csv(area_july, file = "area_july.csv", sep = ";", na="NA", dec = ".")