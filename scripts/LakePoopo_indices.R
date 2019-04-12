#############################
#### Lake Poopo, Bolivia ####
#############################
#author: Magdalena Halbgewachs, 2019
##content:

# 1.) cropping the downloaded Landsat scenes to an predefined extent
# 2.) calculating the NDWI for the years 1989 to 2018
# 3.) change detection in water level per year (based on NDWI) ==> Change Vector Analysis
# 4.) calculating the fluctuations of the water surface during all years
# 5.) correlation between precipitation and water area per month/year

#### package: changeDetectR ####

#loading required packages
library(sp)
library(raster)
library(RStoolbox)
source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\get_band.R")
source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\get_sensor_type.R")
source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\NDWI.R")

#-----------------------------------------------------------------------------------------------------
# 1.) creating subsets of all images
#-----------------------------------------------------------------------------------------------------

raw_data_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\raw"
crop_out_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\cropped_data"


dirs_full <- list.dirs(raw_data_folder)
dirs_full <- dirs_full[-1]
dirs <- list.dirs(raw_data_folder, full.names = FALSE)
dirs <- dirs[-1]

#cropping NIR and GREEN bands
for (i in 1:length(dirs_full)) {
  sensor_type <- get_sensor_type(dirs_full[i])
  nir_band <- get_band("NIR", dirs_full[i], sensor_type)
  green_band <- get_band("GREEN", dirs_full[i], sensor_type)
  #crop
  e <- extent(650000, 750000, -2130000, -2040000)
  crop_nir <- crop(nir_band, e)
  crop_green <- crop(green_band, e)
  #write
  name_nir <- paste0(crop_out_folder, '\\crop_NIR_', dirs[i], '.tif')
  name_green <- paste0(crop_out_folder, '\\crop_GREEN_', dirs[i], '.tif')
  writeRaster(crop_nir, filename = name_nir, format="GTiff",overwrite=TRUE)
  message (paste0("finished cropping NIR band:", name_nir))
  writeRaster(crop_green, filename = name_green, format="GTiff",overwrite=TRUE)
  message (paste0("finished cropping GREEN band:", name_green))
}


#--------------------------------------------------------------------------------------------------------
# 2.) NDWI
#---------------------------------------------------------------------------------------------------------
cropped_bands_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\cropped_data"
ndwi_out_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\NDWI"

list_green <- list.files(cropped_bands_folder, pattern="GREEN", full.names = T, no.. = T)
list_nir <- list.files(cropped_bands_folder, pattern="NIR", full.names = T, no.. = T)

#calculating NDWI
for (i in 1:length (list_nir)) {
  nir <- brick(list_nir[i])
  green <- brick(list_green[i])
  ndwi <- NDWI (green, nir)
  name_ndwi <- paste0(ndwi_out_folder, '\\NDWI_', dirs[i], '.tif')
  writeRaster(ndwi, filename = name_ndwi, format="GTiff",overwrite=TRUE)
  message (paste0("finished NDWI image:", name_nir))
}


#-------------------------------------------------------------------------------------------------------
# 4.) extracting water area for each month (based on NDWI) -> classification
#-------------------------------------------------------------------------------------------------------
ndwi_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\NDWI"
water_out_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\classification"

list_ndwi <- list.files(ndwi_folder, pattern="NDWI", full.names = T, no.. = T)

#defining class intervals
#> 0.1 = water; < 0.1 = non water
from <- c(-1, -0.5, 0, 0.1)
to <- c(-0.5, 0, 0.1, 1)
becomes <- c(0, 0, 0, 1)
classDef <- cbind(from, to, becomes)

for (i in 1:length (list_ndwi)) {   #error!!
  classes <- reclassify(brick(list_ndwi[i]), rcl = classDef)
  name_class <- paste0(water_out_folder, '\\class_', dirs[i], '.tif')
  writeRaster(class, filename = name_class, format="GTiff",overwrite=TRUE)
  message (paste0("finished classified image:", name_nir))
}


#----------------------------------
setwd("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\classification")
csv_class<- read.csv(file="C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\stack.csv", header=T, sep=";")

#defining class intervals
#> 0.1 = water; < 0.1 = non water
from <- c(-1, -0.5, 0, 0.1)
to <- c(-0.5, 0, 0.1, 1)
becomes <- c(0, 0, 0, 1)
classDef <- cbind(from, to, becomes)

par(mfrow=c(4,6))
for (row in (1:nrow(csv_class))) {
  newClasses <- reclassify (raster(as.character(csv_class[row,"NDWI"])), rcl = classDef)
  plot(newClasses)
  name <- paste0('class_', csv_class [row, "DATE"], '.tif')
  writeRaster(newClasses, filename = name , format="GTiff",overwrite=TRUE)
  print(paste0("processed image: ", name))
}


#-------------------------------------------------------------------------------------------------------
# 5.) Change detection: minus calculation of NDWI water area per year
#-------------------------------------------------------------------------------------------------------
##minus calculation of two images
classification_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\classification"
change_out_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\change_minus"

list_class <- list.files(classification_folder, pattern="class", full.names = T, no.. = T)








#-----------------------------------------------
setwd("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\change_minus")
csv_change<- read.csv(file="C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\change_minus.csv", header=T, sep=";")

#function change
CHANGE <- function(jul,apr) {
  change <- jul-apr
  return(change)
}

#calculating change
par(mfrow=c(2,6))
for (row in (1:nrow(csv_change))) {
  jul <- brick(raster(as.character(csv_change[row,"JULY"])))
  apr <- brick(raster(as.character(csv_change[row,"APRIL"])))
  result <- CHANGE(jul, apr)
  plot(result)
  name <- paste0('change_minus_', csv_change [row, "ï..YEAR"], '.tif')
  writeRaster(result, filename = name , format="GTiff",overwrite=TRUE)
  print(paste0("processed image: ", name))
}

#-------------------------------------------------------------------------------------------------------
# 6.) calculating water area per month/year
#-------------------------------------------------------------------------------------------------------
setwd("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\area")
csv_change<- read.csv(file="C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\change_minus.csv", header=T, sep=";")

# calculation in km²
calc_water_area <- function(x, res_x, res_y){
  #define default value for res_x and res_y
  if (missing(res_x) & missing(res_y)){
    res_x <- 0.03
    res_y <- 0.03
  }
  vals <- getValues(x)
  water_cells <- sum(vals ==1, na.rm = T)
  water_area <-  sum(vals ==1, na.rm = T) * res_x * res_y
  return(water_area)
}

#-----APRIL
area_april <- data.frame()
for (row in (1:nrow(csv_change))) {
  apr_area <- calc_water_area(brick(raster(as.character(csv_change[row,"APRIL"]))))
  name <- paste0(as.character(csv_change[row, "ï..YEAR"]), "-04-01")
  area_april[row, 1] <- name
  area_april[row, 2] <- apr_area
  print(paste0("processed image: ", name))
}
names(area_april) <- c("YEAR", "APRIL")
write.csv(area_april, file = "area_april.csv", sep = ";", na="NA", dec = ".")

#-----JULY
area_july <- data.frame()
for (row in (1:nrow(csv_change))) {
  jul_area <- calc_water_area(brick(raster(as.character(csv_change[row,"JULY"]))))
  name <- paste0(as.character(csv_change[row, "ï..YEAR"]), "-07-01")
  area_july[row, 1] <- name
  area_july[row, 2] <- jul_area
  print(paste0("processed image: ", name))
}
names(area_july) <- c("YEAR", "JULY")
write.csv(area_july, file = "area_july.csv", sep = ";", na="NA", dec = ".")
