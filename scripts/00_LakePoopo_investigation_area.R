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

#----------------------------------------------------------------------------------------
#mapping, version 1
map('worldHires','Bolivia', col = 'grey', fill=T)
title('Lake Poopó, Bolivia')
points(-67.056825,-18.735300,col= 'blue',pch=16, size=2)
map('lakes', add=T, fill=T, col = 'blue', boundary='blue')
#map('rivers', add=T)

#----------------------------------------------------------------------------------------
#mapping, version 2
map('worldHires',
    c('Bolivia', 'Peru'), col='grey', fill=T)
title('Lake Poopó, Bolivia')
points(-67.056825,-18.735300,col= 'blue',pch=16, size=2)
map('lakes', add=T, fill=T, col = 'blue', boundary='blue')


