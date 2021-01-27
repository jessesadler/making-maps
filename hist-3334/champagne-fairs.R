## Champagne fairs ##

library(sf)
library(tidyverse)
library(raster)
library(opencage)
library(ggrepel)
library(ggforce)
library(glue)
library(scales)
library(ggtext)

# Load data ---------------------------------------------------------------
# All data is st_crs(4326)

rivers <- st_read("data-raw/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
lakes <-  st_read("data-raw/ne_10m_lakes/ne_10m_lakes.shp")
oceans <- st_read("data-raw/ne_10m_ocean/ne_10m_ocean.shp")

elev_raster <- raster("data-raw/SR_HR/SR_HR.tif") %>% 
  crop(extent(c(-11, 24, 34, 58)))

cities <- c("London", "Bruges", "Ypres", "Ghent", "Troyes", "Provins",
            "Lagny-sur-Marne", "Bar-sur-Aube", "Paris", "Frankfurt", "Augsburg",
            "Geneva", "Marseille", "Avignon", "Milan", "Genoa", "Venice",
            "Florence", "Pisa", "Rome", "Naples")

# Use bounds to ensure geocoding is accurate
cities_df <- oc_forward_df(placename = cities,
                           bounds = oc_bbox(-11, 34, 24, 58))

region <- c(NA, rep("Flanders", times = 3), rep("Champagne", times = 4),
             rep(NA, times = 6), rep("Northern Italy", times = 5), NA, NA)
region <- as_factor(region)

cities_df <- add_column(cities_df, region = region)

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

# Build styled title

hex <- scales::hue_pal()(length(levels(region)))
styled_title <- glue("
      **Champagne Fairs: 1180–1300**  
      <span style='font-size:11pt'>
      <span style='color:{hex[[1]]};'>Flanders</span>, 
      <span style='color:{hex[[2]]};'>Champagne</span>, and
      <span style='color:{hex[[3]]};'>Northern Italy</span>
      </span>")

ggplot() +
  geom_raster(data = elev_df_proj, aes(lng, lat, fill = alt), alpha = 0.6) +
  scale_fill_gradientn(colors = gray.colors(50, start = 0.6, end = 1)) + 
  geom_sf(data = oceans_proj, color = NA, fill = gray(0.8)) + 
  geom_sf(data = rivers_proj, color = gray(0.8), size = 0.2) + 
  geom_sf(data = lakes_proj, color = gray(0.8), fill = gray(0.8)) + 
  geom_sf(data = cities_proj, aes(color = region), size = 0.5) + 
  geom_text_repel(data = cities_proj, 
                  aes(x = lng, y = lat, label = placename, color = region),
                  size = 2) + 
  # Highlight Champagne region
  geom_mark_ellipse(data = filter(cities_proj, region == "Champagne"),
               aes(x = lng, y = lat, group = region),
               color = NA, fill = hex[[2]], alpha = 0.2) + 
  coord_sf(xlim = c(2100000, 4800000),
           ylim = c(1050000, 3400000),
           expand = FALSE, datum = NA) + 
  # Have NA be black instead of gray
  scale_color_discrete(na.value = "black") + 
  labs(title = styled_title) + 
  theme_void() + 
  theme(legend.position = "none",
        plot.title = element_markdown())

ggsave(paste0("img/champagne-fairs-",
              st_crs(cities_proj)$epsg, "-",
              lubridate::today(), ".png"))

