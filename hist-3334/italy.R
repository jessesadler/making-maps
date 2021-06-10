## Map of Italy ##

library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(opencage)
library(ggrepel)

# Load data ---------------------------------------------------------------

# Download data
# path <- here("data-raw", "italy-elev")
# dir_create(path)
# italy_raster <- getData("alt", country = "ITA", mask = FALSE, path = path)

# Best to keep CRS as 4326. It is not very different from st_crs(3034), and
# it maintains rectangualar shape of the raster, so that more regions can be
# plotted.

# Raster and cast to data frame
italy_raster <- raster("data-raw/italy-elev/ITA_alt.gri")
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
cities <- c("Milan", "Genoa", "Venice", "Florence", "Pisa", "Rome", "Naples",
            "Messina", "Padua", "Bologna", "Gaeta", "Amalfi", "Bari", "L'Aquila",
            "Ancona", "Lucca", "Siena", "Perugia", "Palermo")

cities <- c("Milan", "Genoa", "Venice", "Florence", "Pisa", "Rome", "Naples",
            "Messina", "Padua", "Bologna", "Gaeta", "Turin", "Parma", "Pavia",
            "Bergamo", "Brescia", "Verona", "Mantua", "Trent", "Vicenza",
            "Ancona", "Lucca", "Siena", "Perugia", "Palermo")

# Use bounds to ensure geocoding is accurate
cities_df <- oc_forward_df(placename = cities,
                           bounds = oc_bbox(6, 36, 19, 47))

bounds <- st_bbox(italy_raster)

ggplot() +
  geom_raster(data = italy_df, aes(lng, lat, fill = alt), alpha = 1) +
  scale_fill_gradientn(colors = gray.colors(50, start = 1, end = 0.4)) + 
  geom_sf(data = oceans, color = NA, fill = gray(0.8)) + 
  geom_sf(data = rivers, color = gray(0.8), size = 0.2) + 
  geom_sf(data = lakes, color = gray(0.8), fill = gray(0.8)) + 
  geom_point(data = cities_df, aes(x = oc_lng, y = oc_lat), size = 1) + 
  geom_text_repel(data = cities_df, 
                  aes(x = oc_lng, y = oc_lat, label = placename),
                  size = 2) +
  coord_sf(xlim = c(bounds[1], bounds[3]),
           ylim = c(bounds[2], bounds[4]),
           expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")

ggsave(paste0("img/italy-4326-",
              lubridate::today(), ".png"))
