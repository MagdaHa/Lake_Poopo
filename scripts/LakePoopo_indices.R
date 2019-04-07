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

#-----------------------------------------------------------------------------------------------------
# 1.) creating subsets of all images
#-----------------------------------------------------------------------------------------------------
#extent(NDWI_0718)

#importing csv with raw data paths
raw_data <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\raw_data.csv", header=T, sep=";")
setwd("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\cropped_data")

#cropping NIR bands
par(mfrow=c(4,6))
for (row in (1:nrow(raw_data))) {
  e <- extent(650000, 750000, -2130000, -2040000) #predefined new extent
  crop_nir <- crop(raster(as.character(raw_data[row, "NIR"])), extent(e))
  plot(crop_nir)
  name <- paste0('crop_NIR_', raw_data[row, "ï..DATE"], '.tif')
  writeRaster(crop_nir, filename = name , format="GTiff",overwrite=TRUE)
}

#cropping GREEN bands
par(mfrow=c(4,6))
for (row in (1:nrow(raw_data))) {
  e <- extent(650000, 750000, -2130000, -2040000)
  crop_green <- crop(raster(as.character(raw_data[row, "GREEN"])), extent(e))
  plot(crop_green)
  name <- paste0('crop_GREEN_', raw_data[row, "ï..DATE"], '.tif')
  writeRaster(crop_green, filename = name , format="GTiff",overwrite=TRUE)
  print(paste0("processed image: ", name))
}


#--------------------------------------------------------------------------------------------------------
# 2.) NDWI
#---------------------------------------------------------------------------------------------------------
#setting working directory
setwd("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\NDWI")

#importing csv with path names to referring Landsat images for NDWI
csv_NDWI<- read.csv(file="C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\NDWI.csv", header=T, sep=";")

#function NDWI
NDWI <- function(green,nir) {   #bands 3 and 5
  ndwi <- (green-nir)/(green+nir)
  return(ndwi)
}

#calculating NDWI for each image
par(mfrow=c(4,6))
for (row in (1:nrow(csv_NDWI))) {
  nir <- brick(raster(as.character(csv_NDWI[row,"NIR"])))
  green <- brick(raster(as.character(csv_NDWI[row,"GREEN"])))
  result <- NDWI(green, nir)
  plot(result)
  name <- paste0('NDWI_', csv_NDWI [row, "ï..DATE"], '.tif')
  writeRaster(result, filename = name , format="GTiff",overwrite=TRUE)
  print(paste0("processed image: ", name))
}


#-------------------------------------------------------------------------------------------------------
# 4.) extracting water area for each month (based on NDWI)
#-------------------------------------------------------------------------------------------------------
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
