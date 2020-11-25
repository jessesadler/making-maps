## Low Countries maps ##

library(sf)
library(ggplot2)

mints <- st_read("data/mint-authorities.geojson")

ggplot() + geom_sf(data = mints)

lowcountries <- st_read("data/lowcountries/Low Countries.shp")
ggplot() + geom_sf(data = localities)


localities <- st_read("data/lowcountries-localities/Low Countries - Localities.shp")
