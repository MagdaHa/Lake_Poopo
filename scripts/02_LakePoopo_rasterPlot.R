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
#loading all NDWI images with the pattern TIF, stored as list
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
#loading all classified images with the pattern TIF, stored as list
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
#loading all change detection images with the pattern TIF, stored as list
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
