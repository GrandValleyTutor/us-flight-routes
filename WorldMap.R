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
  BiocManager
)

sf::sf_use_s2(FALSE)

# 2. WORLD MAP (Countries)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Optional: Add continent info (already included in 'world' data)
head(world$continent)

# 3. PLOT THE MAP

world_map <- ggplot() +
  geom_sf(data = world, aes(fill = continent), color = "white", size = 0.1) +
  scale_fill_viridis_d(option = "C", name = "Continent") +
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