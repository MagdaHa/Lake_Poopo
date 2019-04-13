###############################
##### Accuracy Assessment #####
###############################
library(raster)
library(rgdal)
library(caret)

#loading classified image from April 2009
class_0409 <- brick("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\classification\\class_LT05_L1TP_233073_20090417_20161027_01_T1.tif")
plot(class_0409)

#loading validation areas
val_shp <- readOGR("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\Lake_Poopo\\validation.shp")
plot(val_shp, add=T)

unique_classes <- unique(val_shp$classname)

#extractingn classified values from raster and append to shapefile
set.seed(25)
xy_val <- lapply(unique_classes, function(class){
  class_data <- subset(val_shp, classname==class)
  classpts <- spsample(class_data, type="random", n=100)
  classpts$class <- rep(class, length(classpts))
  return(classpts)
})

#rbind the two dataframes into one object
xy_val <- do.call("rbind", xy_val)
plot(xy_val, add=T)

#extract predictions
pred <- extract(class_0409$class_LT05_L1TP_233073_20090417_20161027_01_T1, xy_val, cellnumbers=T)

#remove duplicate cells and cell colums
dup <- duplicated(pred)
pred <- pred[!dup,"class_LT05_L1TP_233073_20090417_20161027_01_T1"]
obs <- xy_val$class[!dup]

#convert int to factors
valFactor <- factor(pred, levels = c(0, 1), labels = c("no_water", "water"))

confusionMatrix(obs, reference = valFactor)



