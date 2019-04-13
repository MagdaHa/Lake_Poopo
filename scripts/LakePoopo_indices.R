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
#install.packages('devtools', dependencies = TRUE)
library(devtools)
#install.packages("backports")
library(backports)
install_github("MagdaHa/Lake_Poopo/ChangeDetectR")
library(ChangeDetectR)
#source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\get_band.R")
#source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\get_sensor_type.R")
#source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\NDWI.R")
#source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\binaryMap.R")
#source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\change_detection.R")
#source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\calc_water_area.R")
#source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\getDate.R")
#source("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\Lake_Poopo\\Functions\\getYear.R")

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
  date <- getDate(dirs_full[i])
  #crop
  e <- extent(650000, 750000, -2130000, -2040000)
  crop_nir <- crop(nir_band, e)
  crop_green <- crop(green_band, e)
  #write
  writeRaster(crop_nir, filename = paste0(crop_out_folder, "/crop_nir", date, ".tif"), format = "GTiff", overwrite = T)
  writeRaster(crop_green, filename = paste0(crop_out_folder, "/crop_green", date, ".tif"), format = "GTiff", overwrite = T)
  message (paste0("finished processed image:", date))
} 

#--------------------------------------------------------------------------------------------------------
# 2.) NDWI
#---------------------------------------------------------------------------------------------------------
cropped_bands_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\cropped_data"
ndwi_out_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\NDWI"

list_green <- list.files(cropped_bands_folder, pattern="green", full.names = T, no.. = T)
list_nir <- list.files(cropped_bands_folder, pattern="nir", full.names = T, no.. = T)


for (i in 1:length (list_nir)) {
  #ndwi calculation
  nir <- brick(list_nir[i])
  green <- brick(list_green[i])
  ndwi <- NDWI (green, nir)
  date <- getDate(list_green[i])
  #write
  writeRaster(ndwi, filename = paste0(ndwi_out_folder, "/NDWI_", date, ".tif"), format = "GTiff", overwrite = T)
  message (paste0("finished processed image:", date))
}
#-------------------------------------------------------------------------------------------------------
# 4.) extracting water area for each month (based on NDWI) -> classification, binary images
#-------------------------------------------------------------------------------------------------------
ndwi_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\NDWI"
binary_out_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\classification"

list_ndwi <- list.files(ndwi_folder, pattern="NDWI", full.names = T, no.. = T)


for (i in 1:length(list_ndwi)) {
  #binary map
  threshold <- 0.1
  ndwi_ras <- brick(list_ndwi[i])
  class <- binaryMap(ndwi_ras, threshold)
  date <- getDate(list_ndwi[i])
  #write
  writeRaster(class, filename = paste0(binary_out_folder, "/class_", date, ".tif"), format = "GTiff", overwrite = T)
  message(paste0("finished classified image:", date))
}


#-------------------------------------------------------------------------------------------------------
# 5.) Change detection: minus calculation of NDWI water area per year
#-------------------------------------------------------------------------------------------------------
##minus calculation of two images
classification_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\classification"
change_out_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\change_minus"

list_change_april <- list.files(classification_folder, pattern="04", full.names = T, no.. = T) #geth nicht! zu viele Bilder!
list_change_july <- list.files(classification_folder, pattern="07", full.names = T, no.. = T)


for (i in 1:length(list_change_april)) {
  apr <- brick(list_change_april[i])
  jul <- brick(list_change_july[i])
  change <- change_detection(jul, apr)
  year <- getYear(list_change_april[i])
  writeRaster(change, filename = paste0(change_out_folder, "\\change_", year, ".tif"), format = "GTiff", overwrite = T)
  message(paste0("finished image:", year))
}


#-------------------------------------------------------------------------------------------------------
# 6.) calculating water area per month/year
#-------------------------------------------------------------------------------------------------------
classification_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\classification"

list_area <- list.files(classification_folder, pattern="class", full.names = T, no.. = T)
list_area_april <- list.files(classification_folder, pattern="04", full.names = T, no.. = T)
list_area_july <- list.files(classification_folder, pattern="07", full.names = T, no.. = T)

#create data frames for all areas and july and april separated
area_df <- data.frame(matrix(ncol = 2, nrow = 22))
names(area_df) <- c("DATE", "AREA")
area_df$DATE <- c("1989-04-01", "1989-07-01", "1995-04-01", "1989-07-01", "1999-04-01", "1999-07-01", 
                  "2005-04-01", "2005-07-01", "2009-04-01", "2009-07-01",
                  "2013-04-01", "2013-07-01", "2014-04-01", "2014-07-01", "2015-04-01", 
                  "2015-07-01", "2016-04-01", "2016-07-01", "2017-04-01", "2017-07-01", "2018-04-01", "2018-07-01")
area_df$AREA <- 0

area_df_april <- data.frame(matrix(ncol = 2, nrow = 11))
names(area_df_april) <- c("YEAR", "AREA")
area_df_april$YEAR <- c("1989-04-1", "1995-04-1", "1999-04-1", "2005-04-1", "2009-04-1",
                  "2013-04-1", "2014-04-1", "2015-04-1", "2016-04-1", "2017-04-1", "2018-04-1")
area_df_april$AREA <- 0

area_df_july <- data.frame(matrix(ncol = 2, nrow = 11))
names(area_df_july) <- c("YEAR", "AREA")
area_df_july$YEAR <- c("1989-07-01", "1995-07-01", "1999-07-01", "2005-07-01", "2009-07-01",
                        "2013-07-01", "2014-07-01", "2015-07-01", "2016-07-01", "2017-07-01", "2018-07-01")
area_df_july$AREA <- 0

#area calculations, separated by months
for (i in 1:length(list_area)) {
  raster <- brick(list_area[i])
  year <- getYear(list_area)
  area_df[i,2] <- calc_water_area(raster)
  message(paste0("finished year:", year)) #incorrect
}

#only April areas
for (i in 1:length(list_area_april)) {
  raster <- brick(list_area_april[i])
  year <- getYear(list_area_april)
  area_df_april[i,2] <- calc_water_area(raster)
  message(paste0("finished year:", year)) #incorrect
}

#only July areas
for (i in 1:length(list_area_july)) {
  x <- brick(list_area_july[i])
  year <- getYear(list_area_july)
  area_df_july[i,2] <- calc_water_area(x)
  message(paste0("finished year:", year)) #incorrect
}

#save data frames as csv
setwd("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat")
write.csv(area_df, file = "area_new.csv", sep = ";", na="NA", dec = ".")
write.csv(area_df_april, file = "area_april.csv", sep = ";", na="NA", dec = ".")
write.csv(area_df_july, file = "area_july.csv", sep = ";", na="NA", dec = ".")
