install.packages(c("rnaturalearth", "rnaturalearthdata"))

# Load libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Create a data frame of your coordinates
points_df <- data.frame(
  lon = c(-72.20, -71.40, -71.15, -74.01),
  lat = c(42.55, 44.11, 44.79, 45.98),
  site = c("Harvard Forest", "White Mountains", "Second College Grant", "St. Hippolyte")
)

# Convert to sf object
points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)

# Get a basemap (countries)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Make the map
jpeg("figures/maps.jpeg")
ggplot() +

  geom_sf(data = world, fill = "seagreen") +
  geom_sf(data = points_sf, color = "black", size = 3) +
  geom_text(data = points_df,
            aes(x = lon, y = lat, label = site),
            nudge_y = 0.5,    # move labels slightly upward
            size = 3.5,
            color = "black") +
  coord_sf(xlim = c(-80, -60), ylim = c(40, 48), expand = FALSE) +
  theme_few() +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.background = element_rect(fill = "lightblue"))
dev.off()
