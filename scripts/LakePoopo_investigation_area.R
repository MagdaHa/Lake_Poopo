#install.packages("leaflet")
library(leaflet)
library(tmap)
library(sp)
library(raster)
library(rgdal)

tmap_mode("view")

bath.shp <- readOGR("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\Lake_Poopo\\bathymetry_qgis.shp")

e <- extent(650000, 750000, -2130000, -2040000)
coords <- matrix(c(78.46801, 19.53407,
                   78.46801, 19.74557,
                   78.83157, 19.74557,
                   78.83157, 19.53407,
                   78.46801, 19.53407), 
                ncol = 2, byrow = TRUE)

P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(Ps1, axes = TRUE)



# Constant fill
data(World)
tm_shape(World) + tm_fill("darkolivegreen3") + tm_format("World", title="A green World")

# Borders only
tm_shape(World) + tm_borders()+ tm_shape(bath.shp)

