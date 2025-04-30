# 1. PACKAGES

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  svglite,
  tidyverse,
  sf,
  rnaturalearth,
  rnaturalearthdata,
  BiocManager,
  lwgeom,
  maps
)

sf::sf_use_s2(FALSE)

# 2. WORLD MAP (Countries)

# Load world data in WGS84 (lat/lon)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Reproject to Robinson before computing centroids
crs_robinson <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
world_robinson <- st_transform(world, crs = crs_robinson)

# Compute centroids in projected CRS (to minimize geodesic issues)
centroids_robinson <- st_centroid(world_robinson, of_largest_polygon = TRUE) # don't worry about the warning

# Extract and attach X/Y coords
# MULTIPOLYGON contains country boundaries. 
coords <- st_coordinates(centroids_robinson)
world_robinson$centroid_x <- coords[, 1]  # longitude
world_robinson$centroid_y <- coords[, 2]  # latitude 

# Optional: Add continent info (already included in 'world' data)
head(world$continent)

# 3. PLOT THE MAP

world_map <- ggplot() +
  geom_sf(data = world_robinson, aes(fill = continent), color = "white", size = 0.1) +
  scale_fill_viridis_d(option = "E", name = "Continent") +
  theme_void() +
  labs(title = "World Map by Continent")

# Show the plot
print(world_map)

# 4. SAVE THE MAP

ggsave(
  filename = "world-map.svg",
  plot = world_map,
  width = 10,
  height = 6,
  bg = "white"
)

