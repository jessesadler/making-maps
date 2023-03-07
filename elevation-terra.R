## Elevation maps with terra package ##

# Based on https://gist.github.com/Pakillo/c0bd8f6e96e87625e715d3870522653f

library(sf)
library(terra)
library(elevatr)
library(ggplot2)
library(marmap)

## Define area
coords <- data.frame(x = c(-80.6, -80.6, -80.1, -80.1),
                     y = c(37.4, 37.1, 37.4, 37.1))
coords_sf <- st_as_sf(coords, coords = c("x", "y"), crs = 4326)

## Download elevation data
elev_ras <- elevatr::get_elev_raster(coords_sf, z = 13)
elev <- rast(elev_ras)


## Calculate hillshade
slopes <- terrain(elev, "slope", unit = "radians")
aspect <- terrain(elev, "aspect", unit = "radians")
hs <- shade(slopes, aspect)

## Plot hillshading as basemap ##
# (here using terra::plot, but could use tmap)
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = FALSE,
     xlim = c(-80.6, -80.1), ylim = c(37.1, 37.4))
# overlay with elevation
plot(elev, col = terrain.colors(25), alpha = 0.5, legend = FALSE,
     axes = FALSE, add = TRUE)
# add contour lines
contour(elev, col = "grey40", add = TRUE)


## With ggplot ##
elev_df <- as.data.frame(elev_ras, xy = TRUE)
names(elev_df) <- c("lng", "lat", "alt")

# Plot
ggplot() +
  geom_tile(data = elev_df, 
            aes(x = lng, y = lat, fill = alt)) +
  scale_fill_etopo() +
  theme_minimal()
