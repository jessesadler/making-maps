## Romania Hillshade ##
# Use of tidyterra package to simulate Switzerland hillshade
# Source: https://dieghernan.github.io/202210_tidyterra-hillshade/

library(geodata)
library(terra)
library(tidyterra)
library(ggplot2)
library(dplyr)
library(scales)

# Download data
elev <- elevation_30s("ROU", path = here::here("data-raw"))
# Read in data if already downloaded
elev <- rast("data-raw/ROU_elv_msk.tif")

# Rename object
names(elev) <- "alt"

# Get rid of negative values
elev <- elev |> 
  mutate(alt = if_else(alt < 0, 0, alt))

# Quick look
autoplot(elev) +
  theme_minimal()


# Hillshading -------------------------------------------------------------

slope <- terrain(elev, "slope", unit = "radians")
aspect <- terrain(elev, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 270)

# normalize names
names(hill) <- "shades"

# Palette for hillshade
pal_greys <- hcl.colors(1000, "Grays")

ggplot() +
  geom_spatraster(data = hill) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA)



# Avoid use of scale_fill_() ----------------------------------------------
# Create same plot as above, but using fill in geom_spatraster()
# intsead of scale_fill_gradientn(colors = pal_greys, na.value = NA)

# Use a vector of colors

index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)


# Get cols
vector_cols <- pal_greys[index]

# Need to avoid resampling
# and dont use aes

hill_plot <- ggplot() +
  geom_spatraster(
    data = hill, fill = vector_cols, maxcell = Inf,
    alpha = 1
  )

hill_plot


# Selecting colors --------------------------------------------------------


# Regular gradient
grad <- hypso.colors(10, "dem_poster")

autoplot(elev) +
  scale_fill_gradientn(colours = grad, na.value = NA)


# Hypso gradient
# distance between colors is different depending of the type of color.
grad_hypso <- hypso.colors2(10, "dem_poster")


autoplot(elev) +
  scale_fill_gradientn(colours = grad_hypso, na.value = NA)

# Use tidyterra::scale_fill_hypso_tint_c() to take advantage of uneven colors


# Limits for fill scale ---------------------------------------------------
# Upper and lower bounds of fill scale for ggplot and scale_fill_hypso_tint_c()

elev_limits <- minmax(elev) %>% as.vector()

# Rounded to lower and upper 500
elev_limits <- c(floor(elev_limits[1] / 500), ceiling(elev_limits[2] / 500)) * 500

# Make sure min value is 0
elev_limits <- pmax(elev_limits, 0)


# Plot function: trying colors --------------------------------------------

# Base plot
elevt_test <- ggplot() +
  geom_spatraster(data = elev)

# Create a helper function

plot_pal_test <- function(pal) {
  elevt_test +
    scale_fill_hypso_tint_c(
      limits = elev_limits,
      palette = pal
    ) +
    ggtitle(pal) +
    theme_minimal()
}

# Test different palettes
# tidyterra palettes: https://dieghernan.github.io/tidyterra/articles/palettes.html

plot_pal_test("etopo1_hypso")
plot_pal_test("dem_poster")
plot_pal_test("spain")
plot_pal_test("pakistan")
plot_pal_test("utah_1")
plot_pal_test("wiki-2.0_hypso")


# Final plot --------------------------------------------------------------

base_plot <- hill_plot +
  # Avoid resampling with maxcell
  geom_spatraster(data = elev, maxcell = Inf) +
  scale_fill_hypso_tint_c(
    limits = elev_limits,
    palette = "wiki-2.0_hypso",
    alpha = 0.4,
    labels = label_comma(),
    # For the legend I use custom breaks
    breaks = c(
      seq(0, 500, 100),
      seq(750, 1500, 250),
      2000
    )
  )

base_plot

# Theming

# Adjust text size
base_text_size <- 12

base_plot +
  # Change guide
  guides(fill = guide_legend(
    title = "   m.",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  )) +
  labs(
    title = "Elevation of Romania",
    subtitle = "Hillshade and hypsometric tint blend",
    caption = paste0(
      "@dhernangomez using tidyterra, ggplot2, geodata R packages.",
      " Data: Shuttle Radar Topography Mission (SRTM)"
    )
  ) +
  theme(
    plot.background = element_rect("grey97", colour = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(size = base_text_size * 0.5),
    plot.title = element_text(face = "bold", size = base_text_size * 1.4),
    plot.subtitle = element_text(
      margin = margin(b = 10),
      size = base_text_size
    ),
    axis.text = element_text(size = base_text_size * 0.7),
    legend.position = "bottom",
    legend.title = element_text(size = base_text_size * 0.8),
    legend.text = element_text(size = base_text_size * 0.8),
    legend.key = element_rect("grey50"),
    legend.spacing.x = unit(0, "pt")
  )
