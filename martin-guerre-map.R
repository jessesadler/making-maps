## Return of Martin Guerre map ##

# Recreate the map on page 9 of
# Natalie Zemon Davis, *The Return of Martin Guerre*
# (Cambridge: Harvard University Press, 1983)

library(opencage)
library(sf)
library(elevatr)
library(raster)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(rayshader)
library(here)


# Geocode places ----------------------------------------------------------

places <- c("Gimont", "Lombez", "Le Pin", "Sajas", "Pouy-de-Touges", "Rieux",
            "Mane", "Toulouse", "St-Ybars", "Le Fossat", "Le Carla", "Pailhès",
            "Le Mas d'Azil", "Artigat", "Pamiers", "Foix")

# Bounds to get correct places
xmin <- 0.8
xmax <- 1.75
ymin <- 42.9
ymax <- 43.7

bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
bbox_oc <- oc_bbox(xmin, ymin, xmax, ymax)

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


# Elevation data ----------------------------------------------------------

bbox_sf <- st_bbox(bbox, crs = st_crs(4326)) %>% 
  st_as_sfc()

elev_raster <- get_elev_raster(bbox_sf, z = 10)

# Find actual extent of raster
extent(elev_raster)

# River data: http://gaia.geosci.unc.edu/rivers/
rivers <- st_read(here("data-raw", "europe", "eurivs.shp")) %>% 
  st_crop(st_bbox(extent(elev_raster)))

# Check bboxes
extent(elev_raster)
st_bbox(rivers_crop)

# Hill shade --------------------------------------------------------------

# Slope and hill shade
slope_raster <- terrain(elev_raster, opt = "slope")
aspect_raster <- terrain(elev_raster, opt = "aspect")
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

elmat <- raster_to_matrix(elev_raster)

elmat |>
  height_shade() |>
  plot_3d(elmat, zscale = 10, fov = 0,
          theta = 180, # rotation
          phi = 45, # viewing angle
          zoom = 0.75, windowsize = c(1000, 800))

rgl::rgl.close()

# Calculate rayshade and ambshade
raymat <- ray_shade(elmat)
ambmat <- ambient_shade(elmat)

# Just height_shade
elmat %>%
  height_shade() %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))

rgl::rgl.close()

# Height and rayshade
elmat %>%
  height_shade() %>%
  add_shadow(raymat) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))

rgl::rgl.close()

# Height and ambient shade
elmat %>%
  height_shade() %>%
  add_shadow(ambmat, 0.5) %>% 
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))

rgl::rgl.close()

# Height, ambient, and ray
elmat %>%
  height_shade() %>%
  add_shadow(raymat, 0.7) %>%
  add_shadow(ambmat, 0.5) %>% 
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))


# Sphere shade
elmat %>%
  sphere_shade() %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))

rgl::rgl.close()



# Overlays: Rivers and points ---------------------------------------------

elmat %>%
  height_shade() %>%
  add_shadow(ambmat, 0.5) %>%
  add_overlay(generate_line_overlay(rivers,
                                    extent = attr(elev_raster, "extent"),
                                    heightmap = elmat, linewidth = 3,
                                    color = "lightblue")) %>% 
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))

rgl::rgl.close()


elmat %>%
  height_shade() %>%
  add_shadow(ambmat, 0.5) %>%
  add_overlay(generate_line_overlay(rivers,
                                    extent = attr(elev_raster, "extent"),
                                    heightmap = elmat, linewidth = 3)) %>% 
  plot_map()

base_map <- elmat %>%
  height_shade() %>%
  add_shadow(ambmat, 0.5) %>% 
  add_shadow(raymat, 0.8)

rivers_layer <- generate_line_overlay(rivers,
                                      extent = attr(elev_raster, "extent"),
                                      heightmap = elmat,
                                      linewidth = 10,
                                      data_column_width = "a_WIDTH",
                                      color = "#454b87")

# Points
pts <- data.frame(place = c("Artigat", "Toulouse", "Sajas"),
                  lat = c(43.13547, 43.60446, 43.37672),
                  long = c(1.4402561, 1.4442469, 1.0200673))

pts_sf <- st_as_sf(pts, coords = c("long", "lat"), crs = 4326)

pts_layer <- generate_label_overlay(pts_sf,
                                    extent = attr(elev_raster, "extent"),
                                    heightmap = elmat,
                                    data_label_column = "place",
                                    point_size = 4,
                                    text_size = 4)

base_map %>% 
  add_overlay(rivers_layer, alphalayer = 0.7) %>% 
  add_overlay(pts_layer) %>% 
  plot_map()

# Render ------------------------------------------------------------------

# Only need height shade with render_highquality
elmat %>%
  height_shade() %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))

render_highquality(
  "img/nzd_highres.png", 
  parallel = TRUE, # use parallel processing
  samples = 300,
  light = FALSE, # use environmental lighting instead
  environment_light = "env/phalzer_forest_01_4k.hdr", # hdr file for lighting
  intensity_env = 1.5, #  how intense the lighting is
  rotate_env = 180, # Where light comes from, default is NNE
  # Set width and height of image:
  # This is where we increase the resolution by increasing the number of points plotted.
  width = 3000, 
  height = 3000
)

rgl::rgl.close()

# With labels
elmat %>%
  height_shade() %>%
  add_shadow(ambmat, 0.5) %>% 
  add_overlay(generate_line_overlay(rivers,
                                    extent = attr(elev_raster, "extent"),
                                    heightmap = elmat, linewidth = 10,
                                    data_column_width = "a_WIDTH",
                                    color = "#454b87")) %>% 
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))

render_points(extent = attr(elev_raster, "extent"),
              lat = c(43.13547, 43.60446, 43.37672),
              long = c(1.4402561, 1.4442469, 1.0200673),
              heightmap = elmat,
              size = 5,
              zscale = 10)

render_label(elmat, lat = 43.13547, long = 1.4402561, text = "Artigat",
             extent = attr(elev_raster, "extent"),
             zscale = 10, # same zscale as plot3d
             altitude = 1000, # Amount above the ground
             alpha = 0)

render_label(elmat, lat = 43.60446, long = 1.4442469, text = "Toulouse",
             extent = attr(elev_raster, "extent"),
             zscale = 10,
             altitude = 1000,
             alpha = 0)

render_label(elmat, lat = 43.37672, long = 1.0200673, text = "Sajas",
             extent = attr(elev_raster, "extent"),
             zscale = 10,
             altitude = 1000,
             alpha = 0)

render_snapshot("img/martin-guerre-3d.png",
                title_text = "Return of Martin Guerre",
                width = 1000, 
                height = 1000)


render_highquality(
  "nzd_highres_pts.png", 
  parallel = TRUE, # use parallel processing
  samples = 300,
  light = FALSE, # use environmental lighting instead
  environment_light = "env/phalzer_forest_01_4k.hdr", # hdr file for lighting
  intensity_env = 1.5, #  how intense the lighting is
  rotate_env = 180, # Where light comes from, default is NNE
  # Set width and height of image:
  # This is where we increase the resolution by increasing the number of points plotted.
  width = 3000, 
  height = 3000,
  point_radius = 10,
  text_size = 100
)

rgl::rgl.close()

# With points

lat <- c(43.13547, 43.60446, 43.37672)
long <- c(1.4402561, 1.4442469, 1.0200673)

elmat %>%
  height_shade() %>%
  add_shadow(ambmat, 0.5) %>% 
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135,
          phi = 45, zoom = 0.75, windowsize = c(1000, 800))

render_points(extent = attr(elev_raster, "extent"),
              heightmap = elmat,
              lat = lat, long = long,
              size = 10)

render_highquality(
  "img/nzd_highres_pts.png", 
  parallel = TRUE, # use parallel processing
  samples = 300,
  light = FALSE, # use environmental lighting instead
  environment_light = "env/phalzer_forest_01_4k.hdr", # hdr file for lighting
  intensity_env = 1.5, #  how intense the lighting is
  rotate_env = 180, # Where light comes from, default is NNE
  # Set width and height of image:
  # This is where we increase the resolution by increasing the number of points plotted.
  width = 3000, 
  height = 3000,
  point_radius = 10
)


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
               extent = attr(elev_raster, "extent"),
               altitude = 12000, zscale = 50, clear_previous = TRUE)
  
  render_label(elmat, lat = 43.60446, long = 1.4442469, text = "Toulouse",
               extent = attr(elev_raster, "extent"),
               altitude = 12000, zscale = 50)
  
  render_label(elmat, lat = 43.37672, long = 1.0200673, text = "Sajas",
               extent = attr(elev_raster, "extent"),
               altitude = 12000, zscale = 50)
  
  render_snapshot(img_frames[i], title_text = "Return of Martin Guerre")
  rgl::clear3d()
}

# build gif
magick::image_write_gif(magick::image_read(img_frames), 
                        path = paste0(path, "img/martin-guerre.gif"), 
                        delay = 6/n_frames)
