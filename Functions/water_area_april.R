#water area in April
#AREA_APRIL <- function(apr_area, name){}
area_april <- data.frame()
for (row in (1:nrow(csv_change))) {
  #jul_area <- calc_water_area(brick(raster(as.character(csv_change[row,"JULY"]))))
  apr_area <- calc_water_area(brick(raster(as.character(csv_change[row,"APRIL"]))))
  name <- paste0(as.character(csv_change[row, "ï..YEAR"]), "-04-01")
  area_april[row, 1] <- name
  area_april[row, 2] <- apr_area
  print(paste0("processed image: ", name))
}
names(area_april) <- c("YEAR", "APRIL")
write.csv(area_april, file = "area_april.csv", sep = ";", na="NA", dec = ".")
