## Topographical elevation maps ##
## Map of Italy ##

# Source: https://milospopovic.net/crisp-topography-map-with-r/

# Packages
library(elevatr)
library(terra)
library(tidyverse) 
library(sf)
library(giscoR)
library(marmap)

# Get country map ---------------------------------------------------------
prj <- "+proj=longlat +datum=WGS84 +no_defs"


country_sf <- giscoR::gisco_get_countries(
  year = "2016",
  epsg = "4326",
  resolution = "10",
  country = "Italy")

country_transformed <- st_transform(country_sf, crs = prj)

country_elevation <- get_elev_raster(
  locations = country_sf, 
  z = 8, 
  clip = "locations")

country_elevation_df <- as.data.frame(country_elevation, xy = TRUE, na.rm = TRUE)

names(country_elevation_df) <- c("lng", "lat", "alt")

head(country_elevation_df)

ggplot() +
  geom_tile(data = country_elevation_df, 
            aes(x = lng, y = lat, fill = alt)) +
  scale_fill_etopo() +
  coord_sf(crs = prj)+
  theme_minimal() +
  theme(text = element_text(color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "white", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
        plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
        plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank()) +
  labs(x = "", 
       y = NULL, 
       title = "Topographic map of ITALY", 
       subtitle = "", 
       caption = "©2022 Milos Popovic (https://milospopovic.net)")
