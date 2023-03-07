## Downloading rnaturalearth data ##

library(rnaturalearth)
library(here)
library(fs)
library(sf)
library(dplyr)

# Best list of rnaturalearth data: https://github.com/ropensci/rnaturalearth
# Resolutions: small (110), medium (50), large (10)

# Countries ---------------------------------------------------------------

dir_create(here("data-raw/", "ne_countries"))
ne_download(scale = 10,
            category = "cultural",
            type = "countries",
            destdir = here("data-raw/", "ne_countries"),
            load = FALSE)

countries <- st_read(here("data-raw/", "ne_countries", "ne_10m_admin_0_countries.shp"))

countries
dplyr::glimpse(countries)
plot(st_geometry(countries))


# Coastline ---------------------------------------------------------------

dir_create(here("data-raw/", "ne_coastline"))
ne_download(scale = 10,
            category = "physical",
            type = "coastline",
            destdir = here("data-raw/", "ne_coastline"),
            load = FALSE)

coastline <- st_read(here("data-raw/", "ne_coastline", "ne_10m_coastline.shp"))

coastline
plot(st_geometry(coastline))


# Land --------------------------------------------------------------------

dir_create(here("data-raw/", "ne_land"))
ne_download(scale = 10,
            category = "physical",
            type = "land",
            destdir = here("data-raw/", "ne_land"),
            load = FALSE)

land <- st_read(here("data-raw/", "ne_land", "ne_10m_land.shp"))

land
plot(st_geometry(land))


# Ocean -------------------------------------------------------------------

dir_create(here("data-raw/", "ne_ocean"))
ne_download(scale = 10,
            category = "physical",
            type = "ocean",
            destdir = here("data-raw/", "ne_ocean"),
            load = FALSE)

ocean <- st_read(here("data-raw/", "ne_ocean", "ne_10m_ocean.shp"))

ocean
plot(st_geometry(ocean))


# Rivers ------------------------------------------------------------------

# Use scale rank to have stroke weight variable
dir_create(here("data-raw/", "ne_rivers"))
ne_download(scale = 10,
            category = "physical",
            type = "rivers_lake_centerlines_scale_rank",
            destdir = here("data-raw/", "ne_rivers"),
            load = FALSE)

rivers <- st_read(here("data-raw/", "ne_rivers",
                       "ne_10m_rivers_lake_centerlines_scale_rank.shp"))

rivers
glimpse(rivers)
plot(rivers["strokeweig"])

# Clean data
rivers <- rivers |> 
  select(featurecla, scalerank, rivernum, name, name_en, strokeweig, geometry)

# Save as geojson
st_write(rivers, here("data-clean", "rivers.geojson"), delete_dsn = TRUE)


# European rivers ---------------------------------------------------------

dir_create(here("data-raw/", "ne_rivers_europe"))
ne_download(scale = 10,
            category = "physical",
            type = "rivers_europe",
            destdir = here("data-raw/", "ne_rivers_europe"),
            load = FALSE)

rivers_europe <- st_read(here("data-raw/", "ne_rivers_europe", "ne_10m_rivers_europe.shp"))

rivers_europe
dplyr::glimpse(rivers_europe)
plot(st_geometry(rivers_europe))

# Clean data
rivers_europe <- rivers_europe |> 
  select(featurecla, scalerank, rivernum, name, name_en, strokeweig, geometry) |> 
  filter(!is.na(strokeweig)) # Remove where strokeweig is NA

# Save as geojson
st_write(rivers_europe, here("data-clean", "rivers_europe.geojson"), delete_dsn = TRUE)

# Lakes -------------------------------------------------------------------

dir_create(here("data-raw/", "ne_lakes"))
ne_download(scale = 10,
            category = "physical",
            type = "lakes",
            destdir = here("data-raw/", "ne_lakes"),
            load = FALSE)

lakes <- st_read(here("data-raw/", "ne_lakes", "ne_10m_lakes.shp"))

lakes
dplyr::glimpse(lakes)
plot(st_geometry(lakes))

# Clean data
lakes <- lakes |> 
  select(featurecla, scalerank, name, name_en, ne_id, geometry)

st_write(lakes, here("data-clean", "lakes.geojson"))



# Rivers and lakes ggplot -------------------------------------------------
library(ggplot2)
sf_use_s2(FALSE)

# Load data if needed
coastline <- st_read(here("data-raw/", "ne_coastline", "ne_10m_coastline.shp"))
rivers <- st_read(here("data-clean/", "rivers.geojson"))
rivers_europe <- st_read(here("data-clean/", "rivers_europe.geojson"))
lakes <- st_read(here("data-clean", "lakes.geojson"))

bbox <- st_bbox(rivers_europe) |> 
  st_as_sfc()

rivers <- st_intersection(rivers, bbox) |> 
  rbind(rivers_europe)
lakes <- st_intersection(lakes, bbox)
coastline <- st_intersection(coastline, bbox)

ggplot() + 
  geom_sf(data = coastline, color = gray(0.3), linewidth = 0.3) + 
  geom_sf(data = rivers, color = gray(0.8), aes(linewidth = strokeweig), show.legend = FALSE) + 
  geom_sf(data = lakes, color = gray(0.8), fill = gray(0.8)) + 
  scale_linewidth(range = c(0.1, 1)) + 
  theme_void()
