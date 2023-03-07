## Making maps with R ##

library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)

# Load the data -----------------------------------------------------------
# See rnaturalearth-data.R on downloading the data.

coastlines <- st_read("data-raw/ne_coastline/ne_10m_coastline.shp")
countries <- st_read("data-raw/ne_lakes/ne_10m_lakes.shp")
land <- st_read("data-raw/ne_land/ne_10m_land.shp")
rivers <- st_read("data-raw/ne_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
lakes <-  st_read("data-raw/ne_lakes/ne_10m_lakes.shp")
oceans <- st_read("data-raw/ne_ocean/ne_10m_ocean.shp")


# Coastline and countries plots -------------------------------------------

# Make a plot with coastlines
ggplot() + 
  geom_sf(data = coastlines, color = gray(0.3)) + 
  geom_sf(data = rivers, color = gray(0.7)) + 
  geom_sf(data = lakes, fill = gray(0.7), color = gray(0.7)) + 
  coord_sf(xlim = c(-10, 20), ylim = c(34, 58), datum = NA) + 
  theme_void()


# Make a plot with land with gray background
ggplot() + 
  geom_sf(data = land, fill = gray(1), color = gray(1)) + 
  geom_sf(data = rivers, color = gray(0.8)) + 
  geom_sf(data = lakes, fill = gray(0.8), color = gray(0.8)) + 
  coord_sf(xlim = c(-11, 24), ylim = c(34, 58), expand = FALSE, datum = NA) + # Western Europe
  theme_void() + 
  theme(plot.background = element_rect(fill = gray(0.8), color = gray(0.8)),
        panel.background = element_rect(fill = gray(0.8)))



# Make map of main rivers -------------------------------------------------

# Create bounding box
# Change this to change size of map
europe_bbox <- st_bbox(c(xmin = -11, xmax = 24,
                  ymin = 34, ymax = 58),
                crs = st_crs(4326)) %>% 
  st_as_sfc()

rivers_europe <- st_intersection(rivers, europe_bbox) %>% 
  filter(featurecla != "Lake Centerline") %>% 
  dplyr::select(name, name_en, scalerank, ne_id, geometry) %>% 
  mutate(label = if_else(scalerank < 5, name_en, NA_character_),
         length = st_length(geometry)) %>% 
  arrange(desc(length))

# Remove duplicated labels
rivers_europe$label[duplicated(rivers_europe$label)] <- NA

rivers_main_v <- c("Rhine", "Danube", "Oder", "Loire", "Maas", "Po", "Rhône", "Seine")

rivers_main <- filter(rivers_europe, name_en %in% rivers_main_v) %>% 
  mutate(label = name_en)

rivers_main$label[duplicated(rivers_main$label)] <- NA

lakes_europe <- st_intersection(lakes, europe_bbox) %>% 
  filter(!is.na(name)) %>% 
  dplyr::select(name, featurecla, name_en, scalerank, ne_id, geometry)


ggplot() + 
  geom_sf(data = countries, fill = gray(1), color = gray(1)) + 
  geom_sf(data = rivers_main, color = gray(0.8)) + 
  geom_sf_text(data = rivers_main, aes(label = label)) + 
  geom_sf(data = lakes_europe, fill = gray(0.8), color = gray(0.8)) + 
  # Western Europe: same as europe_bbox
  coord_sf(xlim = c(-11, 24), ylim = c(34, 58), expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = gray(0.8), color = gray(0.8)),
        panel.background = element_rect(fill = gray(0.8)))


# Remake map on Goldthwaite (2009) pg 127  --------------------------------

library(opencage)
library(ggrepel)

cities <- c("London", "Amsterdam", "Bruges", "Antwerp", "Cologne", "Lübeck",
            "Danzig", "Paris", "Frankfurt", "Nürnberg", "Cracow", "Besançon",
            "Augsburg", "Geneva", "Burgos", "Lisbon", "Seville", "Cadiz",
            "Cordova", "Malaga", "Valencia", "Alicante", "Palma", "Tunis",
            "Barcelona", "Montpellier", "Marseille", "Avignon", "Milan",
            "Genoa", "Venice", "Florence", "Pisa", "Rome", "Naples", "Messina")

# Use bounds to ensure geocoding is accurate
cities_df <- oc_forward_df(placename = cities,
                           bounds = oc_bbox(-11, 34, 24, 58))

ggplot() + 
  geom_sf(data = countries, fill = gray(1), color = gray(1)) + 
  geom_sf(data = rivers_main, color = gray(0.8)) + 
  geom_sf_text(data = rivers_main, aes(label = label), size = 2) + 
  geom_sf(data = lakes_europe, fill = gray(0.8), color = gray(0.8)) + 
  # Western Europe: same as europe_bbox
  geom_point(data = cities_df, aes(x = oc_lng, y = oc_lat)) + 
  geom_text_repel(data = cities_df, 
                  aes(x = oc_lng, y = oc_lat, label = placename),
                  size = 2) +
  coord_sf(xlim = c(-11, 24), ylim = c(34, 58), expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = gray(0.8), color = gray(0.8)),
        panel.background = element_rect(fill = gray(0.8)))


# Add elevation -----------------------------------------------------------

elev_raster <- raster("data-raw/SR_HR/SR_HR.tif") %>% 
  crop(extent(c(-11, 24, 34, 58)))
elev_df <- elev_raster %>% 
  rasterToPoints() %>% 
  data.frame()

names(elev_df) <- c("lng", "lat", "alt")

# White ocean
ggplot() +
  geom_raster(data = elev_df, aes(lng, lat, fill = alt), alpha = 0.6) +
  scale_fill_gradientn(colors = gray.colors(50, start = 0.5)) + 
  geom_sf(data = oceans, color = NA, fill = gray(1)) + # Need water
  geom_sf(data = rivers, color = gray(1)) + 
  geom_sf(data = lakes_europe, color = NA, fill = gray(1)) + 
  # Western Europe: same as europe_bbox
  geom_point(data = cities_df, aes(x = oc_lng, y = oc_lat), size = 0.5) + 
  geom_text_repel(data = cities_df, 
                  aes(x = oc_lng, y = oc_lat, label = placename),
                  size = 2) +
  coord_sf(xlim = c(-11, 24), ylim = c(34, 58), expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")
  

# Gray ocean
ggplot() +
  geom_raster(data = elev_df, aes(lng, lat, fill = alt), alpha = 0.6) +
  scale_fill_gradientn(colors = gray.colors(50, start = 0.7, end = 1)) + 
  geom_sf(data = oceans, color = NA, fill = gray(0.8)) + 
  geom_sf(data = rivers, color = gray(0.8), size = 0.2) + 
  geom_sf(data = lakes, color = gray(0.8), fill = gray(0.8)) + 
  # Western Europe: same as europe_bbox
  geom_point(data = cities_df, aes(x = oc_lng, y = oc_lat), size = 0.5) + 
  geom_text_repel(data = cities_df, 
                  aes(x = oc_lng, y = oc_lat, label = placename),
                  size = 2) +
  coord_sf(xlim = c(-11, 24), ylim = c(34, 58), expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")

ggsave(paste0("img/elevation-map-", lubridate::today(), ".png"))


# Low Countries -----------------------------------------------------------

lowcountries_bbox <- st_bbox(c(xmin = -2, xmax = 8,
                               ymin = 49, ymax = 53.5),
                             crs = st_crs(4326)) %>% 
  st_as_sfc()

rivers_lc <- st_intersection(rivers, lowcountries_bbox) %>% 
  filter(featurecla != "Lake Centerline") %>% 
  dplyr::select(name, name_en, scalerank, ne_id, geometry) %>% 
  mutate(length = st_length(geometry)) %>% # Used to get correct placement of Maas
  arrange(desc(length))

rivers_lc$name_en[duplicated(rivers_lc$name_en)] <- NA

lakes_lc <- st_intersection(lakes, lowcountries_bbox) %>% 
  dplyr::select(name, featurecla, name_en, scalerank, ne_id, geometry)

ggplot() + 
  geom_sf(data = countries, fill = gray(1), color = gray(1)) + 
  geom_sf(data = rivers_lc, color = gray(0.8)) + 
  geom_sf_text(data = rivers_lc, aes(label = name_en)) + 
  geom_sf(data = lakes_lc, fill = gray(0.8), color = gray(0.8)) + 
  # Western Europe: same as lowcounrties_bbox
  coord_sf(xlim = c(-2, 8), ylim = c(49, 53.5), expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = gray(0.8), color = gray(0.8)),
        panel.background = element_rect(fill = gray(0.8)))
