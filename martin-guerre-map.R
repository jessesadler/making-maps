## Return of Martin Guerre map ##

# Recreate the map on page 9 of
# Natalie Zemon Davis, *The Return of Martin Guerre*
# (Cambridge: Harvard University Press, 1983)

library(opencage) # Development version: https://github.com/ropensci/opencage
library(sf)
library(raster)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(here)
library(fs)
library(rayshader)


# Geocode places ----------------------------------------------------------

places <- c("Gimont", "Lombez", "Le Pin", "Sajas", "Pouy-de-Touges", "Rieux",
            "Mane", "Toulouse", "St-Ybars", "Le Fossat", "Le Carla", "Pailhès",
            "Le Mas d'Azil", "Artigat", "Pamiers", "Foix")

# Bounds to get correct places
xmin <- 0.8
xmax <- 1.75
ymin <- 42.9
ymax <- 43.7

bbox <- oc_bbox(xmin, ymin, xmax, ymax)

geo_places <- oc_forward_df(placename = places, countrycode = "FR", bounds = bbox)

sf_places <- st_as_sf(geo_places, coords = c("oc_lng", "oc_lat"), crs = 4326)

# Check that locations are correct
mapview::mapview(sf_places)


# Prep location data for plotting -----------------------------------------

# Larger cities in caps
# Highlight Artigat in red
geo_places <- geo_places %>% 
  mutate(placename = if_else(placename %in% c("Toulouse", "Pamiers", "Foix"),
                             toupper(placename),
                             placename),
         color = if_else(placename == "Artigat", "red", "black"))


# GIS data ----------------------------------------------------------------

# Create directory for elevation data download
path <- here("data-raw", "martin-guerre-elev")
dir_create(path)

# Download elevation data from http://srtm.csi.cgiar.org with raster
elev_raster <- getData("SRTM", lon = 1.25, lat = 43.3,
                       path = path)
# River data: http://gaia.geosci.unc.edu/rivers/
rivers <- st_read(here("data-raw", "europe", "eurivs.shp"))


# Crop GIS data -----------------------------------------------------------

bbox_sf <- st_bbox(c(xmin = xmin, xmax = xmax,
                     ymin = ymin, ymax = ymax),
                   crs = st_crs(4326)) %>% 
  st_as_sfc()

rivers <- st_intersection(rivers, bbox_sf)

# Find extent
extent(elev_raster)
elev_crop <- crop(elev_raster, extent(c(xmin, xmax, ymin, ymax)))
plot(elev_crop)


# Hill shade --------------------------------------------------------------

# Slope and hill shade
slope_raster <- terrain(elev_crop, opt = "slope")
aspect_raster <- terrain(elev_crop, opt = "aspect")
hill_raster <- hillShade(slope_raster, aspect_raster, 40, 270)

# cast raster to data frame for plotting
hill_df <- hill_raster %>% 
  rasterToPoints() %>% 
  data.frame()

names(hill_df) <- c("lng", "lat", "hill")


# Plot --------------------------------------------------------------------

ggplot() +
  geom_raster(data = hill_df, aes(lng, lat, fill = hill), alpha = 1) +
  scale_fill_gradientn(colors = gray.colors(100)) + 
  geom_sf(data = rivers, aes(size = a_WIDTH), color = "lightblue1") + 
  scale_size(range = c(0.1, 0.8)) + # river size by width
  geom_sf(data = sf_places, size = 1) + 
  geom_text_repel(data = geo_places, aes(x = oc_lng, y = oc_lat,
                                         label = placename, color = color),
                  family = "Times New Roman",
                  size = 3) + 
  scale_colour_identity() + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE, datum = NA) + 
  labs(title = "The Return of Martin Guerre",
       subtitle = "Recreating the map from Natalie Zemon Davis' book",
       caption = "NZD, The Return of Martin Guerre (1983), pg 9") + 
  theme_void() + 
  theme(legend.position = "none",
        text = element_text("Times New Roman"))

ggsave("img/martin-guerre-hill-shade-2.png")


# Rayshader ---------------------------------------------------------------

elmat <- raster_to_matrix(elev_crop)

elmat %>%
  sphere_shade(sunangle = 45, texture = "imhof1") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>% 
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))

render_label(elmat, lat = 43.13547, long = 1.4402561, text = "Artigat",
             extent = attr(elev_crop, "extent"),
             altitude = 12000, zscale = 50, clear_previous = TRUE)

render_label(elmat, lat = 43.60446, long = 1.4442469, text = "Toulouse",
             extent = attr(elev_crop, "extent"),
             altitude = 12000, zscale = 50)

render_label(elmat, lat = 43.37672, long = 1.0200673, text = "Sajas",
             extent = attr(elev_crop, "extent"),
             altitude = 12000, zscale = 50)

render_snapshot("img/martin-guerre-3d.png",
                title_text = "Return of Martin Guerre")



# Create gif --------------------------------------------------------------

path <- here("img", "martin-guerre-gif")
n_frames <- 60
# frame transition variables
thetavalues <- -90 + 45 * cos(seq(0, 2*pi, length.out = n_frames))

raymat <- ray_shade(elmat)
ambmat <- ambient_shade(elmat)

# generate .png frame images
img_frames <- paste0(path, "martin-guerre",
                     seq_len(n_frames), ".png")
for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  elmat %>%
    sphere_shade(sunangle = 45, texture = "imhof1") %>% 
    add_shadow(raymat, 0.5) %>% 
    add_shadow(ambmat, 0) %>% 
    plot_3d(elmat, zscale = 10, fov = 0, phi = 45, zoom = 0.75,
            theta = thetavalues[i])
  
  render_label(elmat, lat = 43.13547, long = 1.4402561, text = "Artigat",
               extent = attr(elev_crop, "extent"),
               altitude = 12000, zscale = 50, clear_previous = TRUE)
  
  render_label(elmat, lat = 43.60446, long = 1.4442469, text = "Toulouse",
               extent = attr(elev_crop, "extent"),
               altitude = 12000, zscale = 50)
  
  render_label(elmat, lat = 43.37672, long = 1.0200673, text = "Sajas",
               extent = attr(elev_crop, "extent"),
               altitude = 12000, zscale = 50)
  
  render_snapshot(img_frames[i], title_text = "Return of Martin Guerre")
  rgl::clear3d()
}

# build gif
magick::image_write_gif(magick::image_read(img_frames), 
                        path = paste0(path, "img/martin-guerre.gif"), 
                        delay = 6/n_frames)
