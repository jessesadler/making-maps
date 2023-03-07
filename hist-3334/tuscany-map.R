## Tuscany ##

library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(opencage)
library(ggrepel)
sf_use_s2(FALSE)

# Raster and cast to data frame
# All of Italy
italy_raster <- rast("data-raw/italy-elev/ITA_alt.gri") %>% 
  crop(ext(c(10, 12.2, 42.6, 44.4)))

# Central Italy
italy_raster <- rast("data-raw/srtm_39_04/srtm_39_04.tif") %>% 
  crop(ext(c(10, 12.2, 42.6, 45)))

# Cast to data frame
italy_df <- as.data.frame(italy_raster, xy = TRUE)
names(italy_df) <- c("lng", "lat", "alt")

# lowest altitude is 0
italy_df <- italy_df |> 
  mutate(alt = if_else(alt < 0, 0, alt))

# Rivers, lakes, oceans data
# bbox for cropping
bbox <- st_bbox(c(xmin = 10, xmax = 12.2, ymin = 42.6, ymax = 44.4),
                crs = st_crs(4326)) |> 
  st_as_sfc()

rivers <- st_read("data-clean/rivers.geojson") |> 
  st_intersection(bbox)
rivers_europe <- st_read("data-clean/rivers_europe.geojson") |> 
  st_intersection(bbox)
rivers <- bind_rows(rivers, rivers_europe)

lakes <- st_read("data-clean/lakes.geojson") |> 
  st_intersection(bbox)

# Cities and geocode
cities <- c("Lucca", "Pisa", "Pistoia", "Volterra", "San Gimignano",
            "Siena", "Florence", "Arezzo", "Cortona", "Prato")

# Use bounds to ensure geocoding is accurate
cities_df <- oc_forward_df(placename = cities,
                           bounds = oc_bbox(10, 42, 12, 45))


# Black and white plot ----------------------------------------------------

ggplot() +
  geom_raster(data = italy_df, aes(lng, lat, fill = alt), alpha = 1) +
  scale_fill_gradientn(colors = gray.colors(50, start = 1, end = 0.1)) + 
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
  theme(legend.position = "none",
        # Better to have background be water color than use ocean data
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = gray(0.8)),
        panel.border = element_blank())

ggsave("img/tuscany.png")


# Palette plot ------------------------------------------------------------
library(tidyterra)

ggplot() +
  geom_raster(data = italy_df, aes(lng, lat, fill = alt), alpha = 1) +
  tidyterra::scale_fill_hypso_tint_c(palette = "wiki-2.0_hypso") + 
  geom_sf(data = rivers, color = "white", size = 0.5) + 
  geom_sf(data = lakes, color = "white", fill = "white") + 
  geom_point(data = cities_df, aes(x = oc_lng, y = oc_lat), size = 1) + 
  geom_text_repel(data = cities_df, 
                  aes(x = oc_lng, y = oc_lat, label = placename),
                  size = 3) +
  coord_sf(xlim = c(10, 12.2),
           ylim = c(42.6, 44.2),
           expand = FALSE, datum = NA) + 
  theme_void() + 
  theme(legend.position = "none")

ggsave("img/tuscany-wikipal.png")