##########################################################################################
#### Lake Poopo, Bolivia: Raster Plots
#### author: Magdalena Halbgewachs
#### April 2019
##########################################################################################

##########################################################################################
#### Background information

### Study area: Lake Poopó, Bolivia
### content:
### 1.) Plot NDWI images
### 2.) Plot classified (binary) images and bathymetric lines
### 3.) Plot Change Detection

### used datasets:
### - previous calculated NDWI for April and July 1989 - 2018
### - previous classified images for April and July 1989 - 2018
### - bathymetric lines of Lake Poopó

##########################################################################################

#loading required packages
library(ggplot2)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)

##########################################################################################
# 1.) NDWI 
##########################################################################################
NDWI <- "D:/01_Uni/02_Master/MB1_Digital Image Analysis and GIS/00_final_project/01_Landsat/NDWI"
all_NDWI <- list.files(NDWI, full.names = TRUE, pattern = ".tif$")
all_NDWI

#----------------------------------------------------------------------------------------
# Create a raster stack of the NDWI time series
NDWI_stack <- stack(all_NDWI)
# view names for each raster layer
names(NDWI_stack)
# gsub to modify label names
rasterNames  <- gsub("NDWI_","", names(NDWI_stack))

#----------------------------------------------------------------------------------------
#levelplot
cols <- colorRampPalette(brewer.pal(11,"Spectral"))    #  number of colors

levelplot(NDWI_stack,
          main="NDWI\nLake Poopó, Bolivia (1989 - 2018)",
          col.regions=cols,
          names.attr=rasterNames,                     # using new defined names
          scales=list(draw=FALSE ))                  # remove axes labels & ticks


##########################################################################################
# 2.) CLASSIFICATION AND BATHYMETRY 
##########################################################################################
class <- "D:/01_Uni/02_Master/MB1_Digital Image Analysis and GIS/00_final_project/01_Landsat/classification"
all_class <- list.files(class, full.names = TRUE, pattern = ".tif$")
all_class

#----------------------------------------------------------------------------------------
# Create a raster stack of the NDWI time series
class_stack <- stack(all_class)
# view names for each raster layer
names(class_stack)
# gsub to modify label names
rasterNames  <- gsub("class_","", names(class_stack))

#----------------------------------------------------------------------------------------
#load bathymetric lines
bath.shp <- readOGR("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\Lake_Poopo\\bathymetry_qgis.shp")
crs(bath.shp)       #check coordinate system of both data sets
crs(class_stack)

#----------------------------------------------------------------------------------------
#levelplot
cols <- rasterTheme(region = brewer.pal("Blues", n=3))
cuts <- c(0.1, 0.5, 1.1)

levelplot(class_stack,
          main="Lake area\nLake Poopó, Bolivia (1989 - 2018)",
          par.settings=cols,
          at=cuts,
          names.attr=rasterNames,
          kolorkey=F,
          scales=list(draw=FALSE ))+
  layer(sp.polygons(bath.shp), packets = (1:22))


##########################################################################################
# 3.) CHANGE DETECTION
##########################################################################################
minus <- "D:/01_Uni/02_Master/MB1_Digital Image Analysis and GIS/00_final_project/01_Landsat/change_minus"
all_minus <- list.files(minus, full.names = TRUE, pattern = ".tif$")
all_minus

#----------------------------------------------------------------------------------------
# Create a raster stack of the NDWI time series
minus_stack <- stack(all_minus)
# view names for each raster layer
names(minus_stack)
# gsub to modify label names
rasterNames  <- gsub("change_","", names(minus_stack))

#----------------------------------------------------------------------------------------
#levelplot
cols <- colorRampPalette(brewer.pal(9,"RdBu"))

levelplot(minus_stack,
          main="Loss and gain of water area\nLake Poopó, Bolivia (1989 - 2018)",
          col.regions=cols,
          names.attr=rasterNames,
          scales=list(draw=FALSE ))


########################################################################################
#----------------------------------------------------------------------------------------
#animation
urb_anim <- tm_shape(bath.shp)+tm_polygons(bath.shp) +
  tm_raster(class_stack$class_1989_04)+
  tm_facets(along = "DATE", free.coords = FALSE)
urb_anim  

install.packages("animate_raster")
devtools::install_github("16EAGLE/moveVis")
setwd("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann")
install.packages("moveVis-0.9.9.tar.gz", repos = NULL)
install.packages("moveVis")
library(moveVis)
install.packages("animate_raster")
library(animate_raster)
anim_in_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\classification"
anim_out_folder <- "D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat"
anim_list <- list.files(anim_in_folder, pattern = "class", full.names = T, no.. = T)

#------------------------------
library(animation)
ani.options(interval=.05)

saveGIF({
  for (i in 1:length(anim_list)) {
    rc <- anim_list
    plot(rc, col=c("green3", "white"), legend=FALSE, main = paste("Day", i))
  }
}) 





animate_raster(anim_list, out_dir=anim_out_folder, out_format="gif", layer_type="gradient")

library(raster)
anim_brick <- brick(system.file(anim_in_folder, package="raster"))
animate(anim_brick, n=22)
