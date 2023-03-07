# Creating High-Quality 3D Visuals with Rayshader
# url: https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
# Data for Grand Canyon boundary: https://irma.nps.gov/DataStore/Reference/Profile/2209617
# Environmental lighting hdr files: https://polyhaven.com/
# hdr file used for environmental lighting: https://polyhaven.com/a/phalzer_forest_01

library(sf)
library(elevatr)
library(rayshader)
library(MetBrewer)

# Boundary data
data <- st_read("data-raw/grca_tracts/GRCA_boundary.shp")

# Get raster of elevation data
gcnp_elev <- get_elev_raster(data, z = 10, clip = "location")

# Convert to matrix
mat <- raster_to_matrix(gcnp_elev)

# Create color palette
colors <- met.brewer("Demuth")
pal <- grDevices::colorRampPalette(colors)(256)

# First 3d plot
mat |>
  height_shade(texture = pal) |>
  plot_3d(heightmap = mat)

# Deal with window size

# Dynaimcally set window height and width based on object size
w <- nrow(mat)
h <- ncol(mat)

# Scale the dimensions so we can use them as multipliers
wr <- w / max(c(w, h))
hr <- h / max(c(w, h))


# Plot --------------------------------------------------------------------

mat |>
  height_shade(texture = pal) |>
  plot_3d(heightmap = mat, 
          windowsize = c(800 * wr, 800 * hr), 
          solid = FALSE, # removes gray base from image
          zscale = 10, # decreases the height scale: default is 1
          phi = 90, # azimuth angle, or the angle at which you are viewing the scene
          zoom = 0.5, 
          theta = 0) # rotation of the scene


# Render High Quality -----------------------------------------------------

render_highquality(
  "gcnp_highres.png", 
  parallel = TRUE, # use parallel processing
  samples = 300,
  light = FALSE, # use environmental lighting instead
  environment_light = "env/phalzer_forest_01_4k.hdr", # hdr file for lighting
  intensity_env = 1.5, #  how intense the lighting is
  rotate_env = 180, # Where light comes from, default is NNE
  # Set width and height of image:
  # This is where we increase the resolution by increasing the number of points plotted.
  width = round(6000 * wr), 
  height = round(6000 * hr)
)


# Annotation --------------------------------------------------------------

library(magick)
library(glue)
library(ggplot2)

img <- image_read("img/gcnp_highres.png")
text_color <- colors[1]

# Use gravity to align text to cardinal directions

# Title
img <- image_annotate(img, "A Portrait of", font = "Elstob",
                       color = text_color, size = 125, gravity = "north",
                       location = "+0+200")
# Subtitle
img <- image_annotate(img, "Grand Canyon National Park", weight = 700, 
                       font = "Elstob", location = "+0+400",
                       color = text_color, size = 200, gravity = "north")

# Square miles, converted from square meters
area <- as.numeric(st_area(data)) / 2.59e6

# Elevation range, converted to feet from meters
elev_range <- (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) * 3.281

# Area
img <- image_annotate(img, glue("Area: {scales::label_comma()(round(area))} sq mi"),
                       font = "Elstob", location = "+1200-1000",
                       color = text_color, size = 110, gravity = "west")

# Elevation range
img <- image_annotate(img, glue("Elevation Range: {scales::label_comma()(round(elev_range))} ft"),
                       font = "Elstob", location = "+1200-1300",
                       color = text_color, size = 110, gravity = "west")

# Inset map
states <- spData::us_states
spot <- st_buffer(st_centroid(data), 100000)
text_color <- colors[length(colors)]


ggplot() + 
  geom_sf(data = states, fill = "transparent", color = text_color, size = 0.2) + 
  geom_sf(data = spot, fill = NA, color = colors[2]) +
  theme_void() + 
  coord_sf(crs = 3347)

ggsave(filename = "img/gcnp_inset.png", w = 4 * 1.25, h = 3 * 1.25)

img <- image_composite(img, image_read("img/gcnp_inset.png"),
                        offset = "+1200-1000", gravity = "east")

# Caption
img <- image_annotate(img, glue("Graphic by Spencer Schien", 
                                  "Data from AWS Terrain Tiles and USGS"), 
                       font = "Elstob", location = "+0+50",
                       color = alpha(text_color, .5), size = 75, gravity = "south")

# Save image
image_write(img, "img/gcnp_fully_annotated.png")
