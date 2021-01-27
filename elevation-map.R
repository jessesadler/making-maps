## Elevation map ##

library(raster)
library(rayshader)
library(sf)
library(rnaturalearth)
library(tidyverse)

# Raster elevation data resources
# - Natural Earth gray shaded relief: https://www.naturalearthdata.com/downloads/10m-raster-data/10m-shaded-relief/
# - General Bathymetric Chart of the Oceans (GEBCO): https://www.gebco.net
# - SRTM data: http://srtm.csi.cgiar.org
# - GADM data: https://gadm.org/index.html

# Download elevation data
elev_raster <- raster("data-raw/SR_HR/SR_HR.tif")

# Crop
elev_raster <- crop(elev_raster, extent(c(-11, 24, 34, 58)))

plot(elev_raster)
# Find extent
extent(elev_raster)


# Rayshader ---------------------------------------------------------------

elmat <- raster_to_matrix(elev_raster)

elmat %>%
  sphere_shade(texture = "bw") %>%
  plot_map()

elmat %>%
  sphere_shade(texture = "bw") %>% 
  add_water(detect_water(elmat), color = "bw") %>% 
  plot_map()

elmat %>%
  sphere_shade() %>%
  add_water(detect_water(elmat)) %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()

# ggplot2 -----------------------------------------------------------------

elev_df <- elev_raster %>% 
  rasterToPoints() %>% 
  data.frame()

names(elev_df) <- c("lng", "lat", "alt")

# Plot
ggplot() +
  geom_raster(data = elev_df, aes(lng, lat, fill = alt), alpha = 0.9) +
  scale_fill_gradientn(colors = gray.colors(100)) + 
  theme_void()

## Add oceans, rivers, and lakes

rivers <- st_read("data-raw/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
lakes <- st_read("data-raw/ne_10m_lakes/ne_10m_lakes.shp")
oceans <- st_read("data-raw/ne_10m_ocean/ne_10m_ocean.shp")

# Plot

# White water, gray land
ggplot() +
  geom_raster(data = elev_df, aes(lng, lat, fill = alt), alpha = 0.6) +
  scale_fill_gradientn(colors = gray.colors(50, start = 0.5)) + 
  geom_sf(data = oceans, color = NA, fill = gray(1)) + 
  geom_sf(data = rivers, color = gray(1), size = 0.2) + 
  geom_sf(data = lakes, color = NA, fill = gray(1)) + 
  coord_sf(xlim = c(-11, 24), ylim = c(34, 58), expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")

# Gray water, white land
ggplot() +
  geom_raster(data = elev_df, aes(lng, lat, fill = alt), alpha = 0.6) +
  scale_fill_gradientn(colors = gray.colors(50, start = 0.7, end = 1)) + 
  geom_sf(data = oceans, color = NA, fill = gray(0.8)) + 
  geom_sf(data = rivers, color = gray(0.8), size = 0.2) + 
  geom_sf(data = lakes, color = NA, fill = gray(0.8)) + 
  coord_sf(xlim = c(-11, 24), ylim = c(34, 58), expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")


# Function ----------------------------------------------------------------

elevation_map <- function(elevation,
                          oceans = NULL,
                          rivers = NULL,
                          lakes = NULL,
                          bounds,
                          gray_land = TRUE,
                          land_alpha = 0.6,
                          river_size = 0.2) {
  # Crop raster
  elev_raster <- raster::crop(elevation, raster::extent(bounds))
  # Cast raster to data frame
  elev_df <- elev_raster %>% 
    raster::rasterToPoints() %>% 
    data.frame()
  
  names(elev_df) <- c("lng", "lat", "alt")
  
  if (isTRUE(gray_land)) {
    start <- 0.5
    end <- 0.9
    water <- gray(1)
  } else {
    start <- 0.7
    end <- 1
    water <- gray(0.8)
  }
  
  p <- ggplot() +
    geom_raster(data = elev_df, aes(lng, lat, fill = alt), alpha = land_alpha) +
    scale_fill_gradientn(colors = gray.colors(50, start = start, end = end))
  
  if (!is.null(oceans)) {
    p <- p + 
      geom_sf(data = oceans, color = NA, fill = water)
  }
  
  if (!is.null(rivers)) {
    p <- p + 
      geom_sf(data = rivers, color = water, size = river_size)
  }
  
  if (!is.null(lakes)) {
    p <- p + 
      geom_sf(data = lakes, color = NA, fill = water)
  }
  
  p + 
    coord_sf(xlim = bounds[1:2], ylim = bounds[3:4], expand = FALSE, datum = NA) + 
    theme_void() + 
    theme(legend.position = "none")
}

# Hill shading ------------------------------------------------------------

# This works best in smaller areas.

# Slope and hill shade
slope_raster <- terrain(elev_raster, opt = "slope")
aspect_raster <- terrain(elev_raster, opt = "aspect")
hill_raster <- hillShade(slope_raster, aspect_raster, 40, 270)

hill_df <- hill_raster %>% 
  rasterToPoints() %>% 
  data.frame()

names(hill_df) <- c("lng", "lat", "hill")
