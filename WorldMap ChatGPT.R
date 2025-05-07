# https://chatgpt.com/c/6815412d-33f0-800f-9d3b-b644484c8d3c

# ------------------------------------------------------------
# 0.  PACKAGES  ------------------------------------------------
# ------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, sf, rnaturalearth, rnaturalearthdata,
  stringdist,                # fuzzy-matching helper
  igraph, tidygraph, ggraph, # network + plotting
  lwgeom, maps, svglite,
  janitor,
  snakecase
)

sf::sf_use_s2(FALSE)  # faster & avoids antimeridian headaches

# ------------------------------------------------------------
# 1.  WORLD BASE MAP (Robinson)  ------------------------------
# ------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
crs_robinson <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
world_robinson <- st_transform(world, crs = crs_robinson)

centroids_robinson <- st_centroid(world_robinson, of_largest_polygon = TRUE)

centroids_df <- centroids_robinson %>%
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(iso_a3, name, continent, lon, lat)

# ------------------------------------------------------------
# 2.  READ & PREPARE ALLUVIAL DATA  ---------------------------
# ------------------------------------------------------------
flows_raw <- readr::read_csv("C:/R Projects/save-the-birds/CITES/cites_original_alluvial 2025-03-29 .csv",
                             show_col_types = FALSE)

# Clean and select key columns
flows <- flows_raw %>%
  janitor::clean_names() %>%
  rename(
    origin = origin,             # Original origin column
    destination = to,            # Assuming "to" is the destination
    weight = quantity            # Assuming "quantity" is the weight
  ) %>%
  select(origin, destination, weight) %>%
  filter(!is.na(origin) & !is.na(destination) & !is.na(weight)) # Remove NA values

# Ensure origin and destination are character type
flows <- flows %>%
  mutate(
    origin = as.character(origin),
    destination = as.character(destination)
  )

# ------------------------------------------------------------
# 3.  MATCH COUNTRIES TO CENTROIDS  ---------------------------
# ------------------------------------------------------------
# Fuzzy matching function
match_country <- function(name, centroids_df) {
  matched_name <- centroids_df$name[amatch(name, centroids_df$name, maxDist = 2, method = "jw")]
  return(ifelse(is.na(matched_name), name, matched_name))
}

# Match origin and destination to country names
flows <- flows %>%
  mutate(
    origin = sapply(origin, match_country, centroids_df),
    destination = sapply(destination, match_country, centroids_df)
  )

# Attach coordinates for origins and destinations
flows_coords <- flows %>%
  left_join(centroids_df, by = c("origin" = "name")) %>%
  rename(lon_o = lon, lat_o = lat) %>%
  left_join(centroids_df, by = c("destination" = "name")) %>%
  rename(lon_d = lon, lat_d = lat) %>%
  filter(!is.na(lon_o) & !is.na(lon_d))  # Only keep matched flows

# ------------------------------------------------------------
# 4.  CLEAN & FILTER FLOW DATA (OPTIMIZED) ---------------------
# ------------------------------------------------------------
# Remove rows where start and end points are identical
edges <- flows_coords %>%
  filter(!(lon_o == lon_d & lat_o == lat_d)) %>%
  select(from = origin, to = destination, weight, lon_o, lat_o, lon_d, lat_d)

# Limit the number of flow lines for fast rendering
max_flows <- 5000  # Adjust as needed
edges_filtered <- edges %>%
  arrange(desc(weight)) %>%
  head(max_flows)

# ------------------------------------------------------------
# 5.  PLOT NETWORK ON WORLD MAP (Robinson) (FAST) --------------
# ------------------------------------------------------------
flow_map <- ggplot() +
  # World base map
  geom_sf(data = world_robinson, fill = "black", color = "white", size = 0.2) +
  
  # Optimized flow lines (single layer with alpha gradient)
  geom_curve(
    data = edges_filtered, aes(x = lon_o, y = lat_o, xend = lon_d, yend = lat_d, alpha = weight),
    color = "#FA57B1", linewidth = 0.3, curvature = 0.2
  ) +
  
  # Control alpha for visual clarity
  scale_alpha_continuous(range = c(0.05, 0.8), guide = "none") +
  
  # Aesthetic adjustments
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#0d0d0d", color = NA),
    plot.background = element_rect(fill = "#0d0d0d", color = NA)
  ) +
  labs(title = "Global Alluvial Network (Robinson Projection)")

# Display the optimized map
print(flow_map)

# ------------------------------------------------------------
# 6.  SAVE PLOT (FAST) -----------------------------------------
# ------------------------------------------------------------
ggsave(
  filename = "global-alluvial-network-robinson-fast.svg",
  plot = flow_map,
  width = 12,
  height = 8,
  bg = "#0d0d0d"
)
