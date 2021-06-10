## Van der Meulen locations ##

library(sf)
library(tidyverse)
library(raster)
library(opencage)
library(ggrepel)

# Load data ---------------------------------------------------------------
# All data is st_crs(4326)

rivers <- st_read("data-raw/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
lakes <-  st_read("data-raw/ne_10m_lakes/ne_10m_lakes.shp")
oceans <- st_read("data-raw/ne_10m_ocean/ne_10m_ocean.shp")

elev_raster <- raster("data-raw/SR_HR/SR_HR.tif") %>% 
  crop(extent(c(-3, 14, 44, 58)))

cities <- c("Antwerp", "Bremen", "Cologne", "Frankfurt", "Strasbourg", "Leiden")

# Use bounds to ensure geocoding is accurate
cities_df <- oc_forward_df(placename = cities,
                           bounds = oc_bbox(0, 48, 11.5, 55))

# Change CRS --------------------------------------------------------------

set_crs <- st_crs(3034)

# Transform sf objects
rivers_proj <- st_transform(rivers, crs = set_crs)
lakes_proj <- st_transform(lakes, crs = set_crs)
oceans_proj <- st_transform(oceans, crs = set_crs)

# Transform raster and cast to data frame
elev_raster_proj <- projectRaster(elev_raster, crs = set_crs$proj4string)

elev_df_proj <- elev_raster_proj %>% 
  rasterToPoints() %>% 
  data.frame()

names(elev_df_proj) <- c("lng", "lat", "alt")

# Cast to sf, transform, and get coordinates for plotting text
cities_proj <- st_as_sf(cities_df, coords = c("oc_lng", "oc_lat"), crs = 4326) %>% 
  st_transform(crs = set_crs) %>% 
  mutate(lng = st_coordinates(.)[ , 1],
         lat = st_coordinates(.)[ , 2])

bounds <- st_bbox(c(xmin = 0,
                    xmax = 11.5,
                    ymax = 48,
                    ymin = 55),
                  crs = st_crs(4326)) %>% 
  st_as_sfc() %>% 
  st_transform(crs = set_crs) %>% 
  st_bbox()


ggplot() +
  geom_raster(data = elev_df_proj, aes(lng, lat, fill = alt), alpha = 0.6) +
  scale_fill_gradientn(colors = gray.colors(50, start = 0.6, end = 1)) + 
  geom_sf(data = oceans_proj, color = NA, fill = gray(0.8)) + 
  geom_sf(data = rivers_proj, color = gray(0.8), size = 0.2) + 
  geom_sf(data = lakes_proj, color = gray(0.8), fill = gray(0.8)) + 
  geom_sf(data = cities_proj, size = 1) + 
  geom_text_repel(data = cities_proj, 
                  aes(x = lng, y = lat, label = placename),
                  size = 3) + 
  coord_sf(xlim = c(bounds[1], bounds[3]),
           ylim = c(bounds[2], bounds[4]),
           expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")

ggsave(paste0("img/vdm-locations-",
              st_crs(cities_proj)$epsg, "-",
              lubridate::today(), ".png"))
