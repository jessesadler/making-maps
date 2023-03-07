## Switzerland Hillshade ##
# Dominic Royé, Hillshade effects
# Source: https://dominicroye.github.io/en/2022/hillshade-effects/

# packages
library(sf)
library(elevatr)
library(tidyverse)
library(terra)
library(whitebox)
library(ggnewscale)
library(tidyterra)
library(giscoR)
library(units)

# Boundaries of Switzerland
suiz <- gisco_get_countries(country = "Switzerland", resolution = "03")

# Import the lakes boundaries
suiz_lakes <- st_read("data-raw/switzerland_lakes/22_DKM500_GEWAESSER_PLY.shp")

# Filter out lakes with an area smaller than 50km squared and lakes in Italy
suiz_lakes <- mutate(suiz_lakes, areakm = set_units(SHP_AREA, "m2") %>% 
                       set_units("km2")) %>% 
  filter(areakm > set_units(50, "km2"),
         !NAMN1 %in% c("Lago di Como / Lario",
                       "Lago d'Iseo",
                       "Lago di Garda"))

# Get elevation data
mdt <- get_elev_raster(suiz, z = 8)

# convert to terra and mask area of interest
mdt <- rast(mdt) %>% 
  mask(vect(suiz))

# reproject crs to lakes data
suiz <- st_transform(suiz, st_crs(suiz_lakes))


# convert the raster into a data.frame of xyz
mdtdf <- as.data.frame(mdt, xy = TRUE)
names(mdtdf)[3] <- "alt"


# Relief map with ggplot2 -------------------------------------------------

ggplot() +
  geom_raster(data = mdtdf,
              aes(x, y, fill = alt)) +
  geom_sf(data = suiz_lakes,
          fill = "#c6dbef", 
          colour = NA) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = 0.5,
                                 title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")


# Adding hillshade --------------------------------------------------------

# Azimuth: Angle of orientation of the light source
# Elevation: Angle of the height of the light source

# Estimate the slope from dem data with terra
sl <- terrain(mdt, "slope", unit = "radians")

# estimate the aspect or orientation
asp <- terrain(mdt, "aspect", unit = "radians")

# calculate the hillshade effect with 45º of elevation
hill_single <- shade(sl, asp, 
                     angle = 45, 
                     direction = 300,
                     normalize = TRUE)

# Combine the relief and shadow effect ------------------------------------

# Use ggnewscale to add multiple scales

# convert the hillshade to xyz
hilldf_single <- as.data.frame(hill_single, xy = TRUE)

ggplot() +
  geom_raster(data = hilldf_single,
              aes(x, y, fill = lyr1),
              show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = mdtdf,
              aes(x, y, fill = alt),
              alpha = .7) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  geom_sf(data = suiz_lakes,
          fill = "#c6dbef", colour = NA) +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")


# Multidirectional shadows ------------------------------------------------

# pass multiple directions to shade()
hillmulti <- map(c(270, 15, 60, 330), function(dir){ 
  shade(sl, asp, 
        angle = 45, 
        direction = dir,
        normalize= TRUE)}
)

# create a multidimensional raster and reduce it by summing up
hillmulti <- rast(hillmulti) %>% sum()

# convert the hillshade to xyz
hillmultidf <- as.data.frame(hillmulti, xy = TRUE)

# map
ggplot() +
  geom_raster(data = hillmultidf,
              aes(x, y, fill = sum),
              show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = mdtdf,
              aes(x, y, fill = alt),
              alpha = .7) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  geom_sf(data = suiz_lakes,
          fill = "#c6dbef", colour = NA) +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "top")


# Multidirectional shadows with whitebox ----------------------------------


