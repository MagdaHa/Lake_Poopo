######################
#### Raster Plots ####
######################

library(ggplot2)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)

######################################################################################
################################## NDWI ##############################################
######################################################################################
NDWI <- "D:/01_Uni/02_Master/MB1_Digital Image Analysis and GIS/00_final_project/01_Landsat/NDWI"
all_NDWI <- list.files(NDWI, full.names = TRUE, pattern = ".tif$")
all_NDWI

#------------------------------------------------------------------
# Create a raster stack of the NDWI time series
NDWI_stack <- stack(all_NDWI)
# view names for each raster layer
names(NDWI_stack)
# use gsub to modify label names
rasterNames  <- gsub("NDWI_","", names(NDWI_stack))

#----------------------------------------------------------------------
#levelplot with one header and legend
cols <- colorRampPalette(brewer.pal(11,"Spectral"))    # number of colors (9 maybe better)

levelplot(NDWI_stack,
          main="NDWI\nLake Poopó, Bolivia (1989 - 2018)",
          col.regions=cols,
          names.attr=rasterNames,      # using new defined names
          scales=list(draw=FALSE )) # remove axes labels & ticks


######################################################################################
################################## CLASSIFICATION ####################################
######################################################################################
class <- "D:/01_Uni/02_Master/MB1_Digital Image Analysis and GIS/00_final_project/01_Landsat/classification"
all_class <- list.files(class, full.names = TRUE, pattern = ".tif$")
all_class

#------------------------------------------------------------------
# Create a raster stack of the NDWI time series
class_stack <- stack(all_class)
# view names for each raster layer
names(class_stack)
# use gsub to modify label names.that we'll use for the plot 
rasterNames  <- gsub("class_","", names(class_stack))

#----------------------------------------------------------------------
#levelplot with one header and legend
#or
cols <- rasterTheme(region = brewer.pal("Blues", n=3))
cuts <- c(0.1, 0.5, 1.1)

levelplot(class_stack,
          main="Lake area\nLake Poopó, Bolivia (1989 - 2018)",
          par.settings=cols,
          at=cuts,
          names.attr=rasterNames, # using new defined names
          kolorkey=F,
          scales=list(draw=FALSE )) # remove axes labels & ticks


######################################################################################
############################# CHANGE DETECTION: MINUS ################################
######################################################################################
minus <- "D:/01_Uni/02_Master/MB1_Digital Image Analysis and GIS/00_final_project/01_Landsat/change_minus"
all_minus <- list.files(minus, full.names = TRUE, pattern = ".tif$")
all_minus

#------------------------------------------------------------------
# Create a raster stack of the NDWI time series
minus_stack <- stack(all_minus)
# view names for each raster layer
names(minus_stack)
# use gsub to modify label names.that we'll use for the plot 
rasterNames  <- gsub("change_minus_","", names(minus_stack))

#----------------------------------------------------------------------
#levelplot with one header and legend
cols <- colorRampPalette(brewer.pal(9,"RdBu"))    # number of colors

levelplot(minus_stack,
          main="Loss and gain of water area\nLake Poopó, Bolivia (1989 - 2018)",
          col.regions=cols,
          names.attr=rasterNames,      # using new defined names
          #col=cols,
          #col = c("white", "blue"),
          #par.settings=GrTheme(),
          scales=list(draw=FALSE )) # remove axes labels & ticks

######################################################################################
################################# BATHYMETRY  ########################################
######################################################################################
bath.shp <- readOGR("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\Lake_Poopo\\bathymetry_qgis.shp")
crs(bath.shp)
crs(class_stack)

mapTheme <- rasterTheme(region=brewer.pal(8,"Blues"))
names(class_stack)
rasterNames  <- gsub("class_","", names(class_stack))
levelplot(class_stack,par.settings=mapTheme, alpha=0.2,
          main="Lake area extent and bathymetry lines\n Lake Poopó (1989 - 2018)",
          scales=list(draw=F),
          names.attr=rasterNames) +
  layer(sp.polygons(bath.shp), packets = (1:22))


#-----------------------------------------
#animation
urb_anim <- tm_shape(bath.shp)+tm_polygons(bath.shp) +
  tm_raster(class_stack$class_1989_04)+
  tm_facets(along = "DATE", free.coords = FALSE)
urb_anim  
