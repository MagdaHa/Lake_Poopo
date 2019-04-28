##########################################################################################
#### Lake Poopo, Bolivia: Accuracy Assessment
#### author: Magdalena Halbgewachs
#### April 2019
##########################################################################################

##########################################################################################
#### Background information

### Study area: Lake Poopó, Bolivia

### used dataset:
### - classified image from April 2009 as reference data set
### - validation areas
##########################################################################################

#loading required packages
library(raster)
library(rgdal)
#install.packages("caret")
library(caret)

#----------------------------------------------------------------------------------------
#loading data
#loading classified image from April 2009
class_0409 <- brick("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\classification\\class_200904.tif")
plot(class_0409)

#loading validation areas/shape
val_shp <- readOGR("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\Lake_Poopo\\validation.shp")
plot(val_shp, add=T)

#stores unique values of the raster layer (class_0904)
unique_classes <- unique(val_shp$classname)
unique_classes

##----------------------------------------------------------------------------------------
#extracting classified values from raster and append to shapefile
set.seed(25)                                              #reproducibility of the random point function
xy_val <- lapply(unique_classes, function(class){         #for each class the function should be executed
  class_data <- subset(val_shp, classname==class)         #one class per run
  classpts <- spsample(class_data, type="random", n=100)  #100 points in selected subset represent random sample points
  classpts$class <- rep(class, length(classpts))          #writes the class name in the new column "class"
  return(classpts)
})

#----------------------------------------------------------------------------------------
#rbind the two dataframes into one object
xy_val <- do.call("rbind", xy_val)
plot(xy_val, add=T)

#extract predictions
pred <- extract(class_0409$class_200904, xy_val, cellnumbers=T)

#remove duplicate cells and cell columns
dup <- duplicated(pred)
pred <- pred[!dup,"class_200904"]
obs <- xy_val$class[!dup]

#convert integer to factors
valFactor <- factor(pred, levels = c(0, 1), labels = c("no_water", "water"))

#----------------------------------------------------------------------------------------
#gives back the confusion matrix
confusionMatrix(obs, reference = valFactor)



