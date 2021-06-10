## Mediterranean ##

library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(opencage)
library(ggrepel)


# Load and crop data -----------------------------------------------------

# Bounding box
bbox <- c(-15, 42, 25, 52)
bbox_sp <- extent(bbox)
bbox_sf <- st_bbox(bbox_sp, crs = st_crs(4326)) %>% 
  st_as_sfc() # Needs to be sfc object for st_intersection()

rivers <- st_read("data-raw/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp") %>% 
  st_intersection(bbox_sf)
lakes <-  st_read("data-raw/ne_10m_lakes/ne_10m_lakes.shp") %>% 
  st_intersection(bbox_sf)
oceans <- st_read("data-raw/ne_10m_ocean/ne_10m_ocean.shp") %>% 
  st_intersection(bbox_sf)

med_raster2 <- raster("data-raw/SR_HR/SR_HR.tif") %>% 
  crop(bbox_sp)

# Transform CRS -----------------------------------------------------------

set_crs <- st_crs(3035)

# Transform sf objects
rivers_proj <- st_transform(rivers, crs = set_crs)
lakes_proj <- st_transform(lakes, crs = set_crs)
oceans_proj <- st_transform(oceans, crs = set_crs)

# Transform raster and cast to data frame
med_raster_proj <- projectRaster(med_raster, crs = set_crs$proj4string)

med_df_proj <- med_raster_proj %>% 
  rasterToPoints() %>% 
  data.frame()

names(med_df_proj) <- c("lng", "lat", "alt")

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
  coord_sf(datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")

ggplot() +
  geom_raster(data = med_df_proj, aes(lng, lat, fill = alt), alpha = 0.7) +
  scale_fill_gradientn(colors = gray.colors(50, start = 0.4, end = 1)) + 
  geom_sf(data = oceans_proj, color = NA, fill = gray(0.8)) + 
  geom_sf(data = rivers_proj, color = gray(0.8), size = 0.2) + 
  geom_sf(data = lakes_proj, color = gray(0.8), fill = gray(0.8)) + 
  coord_sf(datum = NA) + 
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
