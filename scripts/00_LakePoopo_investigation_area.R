#------------geht http://keithnewman.co.uk/r/maps-in-r.html
library(maps)       # Provides functions that let us plot the maps
library(mapdata)
map('worldHires','Bolivia', col = 'grey')
points(-67.056825,-18.735300,col=2,pch=16)
title('Lake Poopó, Bolivia')
map('lakes', add=T, fill=T, col = 'blue', boundary='blue')


map('rivers', add=T)


