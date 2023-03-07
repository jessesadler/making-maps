library(rayshader)
library(elevatr)
library(raster)

brasil <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_BRA_0_sf.rds"))

dem <- get_elev_raster(brasil, z = 6)

brasil_dem <- mask(dem, brasil)
brasil_mat <- raster_to_matrix(brasil_dem)

brasil_mat %>%
  height_shade() %>%
  plot_3d(brasil_mat,
          windowsize = c(800, 800),
          zscale = 20,
          zoom = 0.75,
          phi = 89,
          theta = 0,
          fov = 0,
          background = "white")

render_highquality(filename = "brasil.png", samples = 100, width = 6000, height = 6000)
