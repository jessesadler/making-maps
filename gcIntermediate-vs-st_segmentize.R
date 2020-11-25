library(sp)
library(geosphere)
library(sf)
library(mapview)

# Bremen to Naples
line_geosphere <- gcIntermediate(p1 = c(8.8016936, 53.07930), p2 = c(14.2681244, 40.85177),
                                 98, addStartEnd = TRUE, sp = TRUE)
line_sfc <- st_linestring(rbind(c(8.8016936, 53.07930), c(14.2681244, 40.85177))) %>% 
  st_sfc(crs = 4326)

# Distance to find right amount of steps
distCosine(p1 = c(8.8016936, 53.07930), p2 = c(14.2681244, 40.85177))/100
st_length(line_sfc)/100

# segment length of 14.5km leads to 100 segments
line_segmentize <- st_segmentize(line_sfc, dfMaxLength = units::set_units(14.5, km))

st_coordinates(line_segmentize)
coords <- coordinates(line_geosphere)
coords_matrix <- coords[[1]][[1]]

# Show that the coordinates are basically the same
round(st_coordinates(line_segmentize)[ , 1:2] - coords_matrix, 7)

# Show on interactive map
mapview(line_geosphere, color = "black") + mapview(line_segmentize, color = "orange")

# Segmentize is same with different CRS, but distance is different
# Change CRS but not transform to keep same coordinates
line_sfc2 <- line_sfc
st_crs(line_sfc2) <- st_crs(4668)
line_segmentize2 <- st_segmentize(line_sfc2, dfMaxLength = units::set_units(14.5, km))

# Different distance
st_length(line_sfc2) - st_length(line_sfc)

# Coordinates of great circles are the same
all.equal(st_coordinates(line_segmentize), st_coordinates(line_segmentize2))