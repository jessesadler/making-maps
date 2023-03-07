library(stars)

tif <- system.file("tif/L7_ETMs.tif", package = "stars")

r <- read_stars(tif)

# Access the array
dim(r[[1]])

st_dimensions(r)

st_bbox(r)
