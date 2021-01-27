## Maps for History 3334: The Renaissance ##

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

ggsave(paste0("img/italy-4326-2-",
              lubridate::today(), ".png"))


# Mediterranean -----------------------------------------------------------

med_raster <- raster("data-raw/SR_HR/SR_HR.tif") %>% 
  crop(extent(c(-11, 38, 29, 48)))

med_df <- med_raster %>% 
  rasterToPoints() %>% 
  data.frame()

names(med_df) <- c("lng", "lat", "alt")

bounds_med <- st_bbox(med_raster)

# Mediterranean in 1050
cities <- c("Genoa", "Venice", "Pisa", "Amalfi", "Bari", "L'Aquila",
            "Mahdia", "Alexandria", "Zara", "Constantinople")
cities_df <- oc_forward_df(placename = cities,
                           bounds = oc_bbox(-11, 29, 38, 48))

# Western Mediterranean in 13th century
cities <- c("Genoa", "Pisa", "Naples", "Barcelona", "Tunis", "Messina",
            "Marseille", "Aigues-Mortes", "Valencia", "Gibraltar", "Venice")

cities_df <- oc_forward_df(placename = cities,
                           bounds = oc_bbox(-11, 29, 16, 48))

med_raster <- raster("data-raw/SR_HR/SR_HR.tif") %>% 
  crop(extent(c(-11, 16.5, 33, 47)))

med_df <- med_raster %>% 
  rasterToPoints() %>% 
  data.frame()

names(med_df) <- c("lng", "lat", "alt")

bounds_med <- st_bbox(med_raster)

# Plot without cities
ggplot() +
  geom_raster(data = med_df, aes(lng, lat, fill = alt), alpha = 0.7) +
  scale_fill_gradientn(colors = gray.colors(50, start = 0.4, end = 1)) + 
  geom_sf(data = oceans, color = NA, fill = gray(0.8)) + 
  geom_sf(data = rivers, color = gray(0.8), size = 0.2) + 
  geom_sf(data = lakes, color = gray(0.8), fill = gray(0.8)) + 
  coord_sf(xlim = c(bounds_med[1], bounds_med[3]),
           ylim = c(bounds_med[2], bounds_med[4]),
           expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")

ggsave(paste0("img/mediterranean",
              "-", lubridate::today(), ".png"))

# Plot with cities
ggplot() +
  geom_raster(data = med_df, aes(lng, lat, fill = alt), alpha = 0.7) +
  scale_fill_gradientn(colors = gray.colors(50, start = 0.4, end = 1)) + 
  geom_sf(data = oceans, color = NA, fill = gray(0.8)) + 
  geom_sf(data = rivers, color = gray(0.8), size = 0.2) + 
  geom_sf(data = lakes, color = gray(0.8), fill = gray(0.8)) + 
  geom_point(data = cities_df, aes(x = oc_lng, y = oc_lat), size = 1) + 
  geom_text_repel(data = cities_df, 
                  aes(x = oc_lng, y = oc_lat, label = placename),
                  size = 3) +
  coord_sf(xlim = c(bounds_med[1], bounds_med[3]),
           ylim = c(bounds_med[2], bounds_med[4]),
           expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")

ggsave("img/mediterranean-cities-1050.png")
ggsave("img/west-mediterranean-1300.png")
