## Tuscany ##

library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(opencage)
library(ggrepel)

# Raster and cast to data frame
# All of Italy
italy_raster <- raster("data-raw/italy-elev/ITA_alt.gri") %>% 
  crop(extent(c(10, 12.2, 42.6, 44.4)))

# Central Italy
italy_raster <- raster("data-raw/srtm_39_04/srtm_39_04.tif") %>% 
  crop(extent(c(10, 12.2, 42.6, 45)))

italy_df <- italy_raster %>% 
  rasterToPoints() %>% 
  data.frame()

names(italy_df) <- c("lng", "lat", "alt")

# Rivers, lakes, oceans data
rivers <- st_read("data-raw/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
rivers_europe <- st_read("data-raw/ne_10m_rivers_europe/ne_10m_rivers_europe.shp")
rivers <- bind_rows(rivers, rivers_europe)
lakes <-  st_read("data-raw/ne_10m_lakes/ne_10m_lakes.shp")
oceans <- st_read("data-raw/ne_10m_ocean/ne_10m_ocean.shp")

# Cities and geocode
cities <- c("Lucca", "Pisa", "Pistoia", "Volterra", "San Gimignano",
            "Siena", "Florence", "Arezzo", "Cortona", "Prato")

# Use bounds to ensure geocoding is accurate
cities_df <- oc_forward_df(placename = cities,
                           bounds = oc_bbox(10, 42, 12, 45))

ggplot() +
  geom_raster(data = italy_df, aes(lng, lat, fill = alt), alpha = 1) +
  scale_fill_gradientn(colors = gray.colors(50, start = 1, end = 0.1)) + 
  geom_sf(data = oceans, color = NA, fill = gray(0.8)) + 
  geom_sf(data = rivers, color = gray(0.8), size = 0.5) + 
  geom_sf(data = lakes, color = gray(0.8), fill = gray(0.8)) + 
  geom_point(data = cities_df, aes(x = oc_lng, y = oc_lat), size = 1) + 
  geom_text_repel(data = cities_df, 
                  aes(x = oc_lng, y = oc_lat, label = placename),
                  size = 3) +
  coord_sf(xlim = c(10, 12.2),
           ylim = c(42.6, 44.2),
           expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")

ggsave("img/tuscany.png")
