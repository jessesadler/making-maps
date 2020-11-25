# tmap

library(tmap)
data(Europe) # load the shapefile
qtm(Europe) # quick visualization of the shapefile

# The main plotting method, the equivalent to ggplot2’s ggplot, consists of elements that start with tm_. The first element to start with is tm_shape, which specifies the shape object. Next there is one or more of layers to be drawn from this shape file.

# Need to have data in spatial data frames

europe_map <- tm_shape(Europe) +
  tm_polygons() +
  tm_shape(routes) +
  tm_lines()


xlim <- c(-2, 40)
ylim <- c(38, 55)
map("world", col="#969696", fill=TRUE, bg="#f7f7f7", lwd=0.05, xlim=xlim, ylim=ylim)