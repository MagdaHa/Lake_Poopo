##########################################################################################
#### Lake Poopo, Bolivia: Investigation area
#### author: Magdalena Halbgewachs
#### April 2019
##########################################################################################

### inspiration: http://keithnewman.co.uk/r/maps-in-r.html

##########################################################################################
#### Background information

### Study area: Lake Poopó, Bolivia

##########################################################################################

#loading required packages
library(maps)
library(mapdata)
library(leaflet)

#----------------------------------------------------------------------------------------
map <- map('worldHires','Bolivia', col = 'grey', fill=T)
m <- leaflet(map) %>%
  setView(lng = -66.856825, lat = -18.735300, zoom = 5) %>%
  addMarkers(lng=-67.056825, lat=-18.735300, popup="Lake Poopó") %>%
  addPolygons(color="red", weight = 1, smoothFactor = 0.5, 
            opacity = 1.0, fillOpacity = 0.1)
m %>% addTiles()







