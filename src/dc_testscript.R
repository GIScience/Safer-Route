library(sf)
library(tidyverse)
library(mapview)
library(sfnetworks)
library(tidygraph)
library(dplyr)


# Read in the datasets
street_lightsDC <- st_read("data/Street_Lights.geojson")
roadsDC <- st_read("data/district-of-columbia-roads.osm.pbf", layer = 'lines')

#Filter road types
roadsDC <- roadsDC  |> 
  filter(highway %in% c("motorway", "motorway_link", "trunk_link", "trunk", "primary", "secondary", "tertiary", "residential", "primary_link", "secondary_link", "tertiary_link"))

# Coordinate transformation
street_lightsDC <- st_transform(street_lightsDC, 32618)
roadsDC <- st_transform(roadsDC, 32618)

# Drop ZM dimensions from street lights
street_lightsDC$geometry <- st_zm(street_lightsDC$geometry, what = "ZM", drop = TRUE)

# Add buffer distance based on road type
roadsDC <- roadsDC |> 
  mutate(buffer_distance = case_when(
    highway == 'motorway' ~ 30, # in meters
    highway == 'motorway_link' ~ 30,
    highway == 'trunk' ~ 23,
    highway == 'trunk_link' ~ 23,
    highway == 'primary' ~ 17,
    highway == 'primary_link' ~ 17,
    highway == 'secondary' ~ 15,
    highway == 'secondary_link' ~ 15,
    highway == 'tertiary' ~ 13,
    highway == 'tertiary_link' ~ 13,
    highway == 'residential' ~ 10,
    TRUE ~ 10  
  ))

# Keep the original geometry for later use
roadsDC_geom <- st_geometry(roadsDC)

# Create buffered geometries
roadsDC_buffered <- st_geometry(roadsDC) |> 
  st_buffer(dist = roadsDC$buffer_distance)

# Replace the geometry column
st_geometry(roadsDC) <- roadsDC_buffered

# Count the lights for each road segment
roads_with_lights <- st_join(roadsDC, street_lightsDC, join = st_intersects)

# Group by road and summarize
roads_with_light_counts <- roads_with_lights |>
  group_by(osm_id) |>
  summarise(light_count = n()) |> 
  st_drop_geometry()

# Join back to the original roads dataset
roadsDC <- left_join(roadsDC, roads_with_light_counts, by = "osm_id")

# Compute light count per area
roadsDC <- roadsDC |>
  mutate(lights_per_area = light_count / as.numeric(st_area(geometry)))

# Compute z-scores based on road types
roadsDC <- roadsDC |>
  group_by(highway) |> 
  mutate(mean_value = mean(lights_per_area, na.rm = TRUE),
         sd_value = sd(lights_per_area, na.rm = TRUE),
         brightness_zscore = (lights_per_area - mean_value) / sd_value) |>
  ungroup()

# Revert to original geometry
st_geometry(roadsDC) <- roadsDC_geom

# Create a temporary data frame with the same number of rows as roadsDC
temp_df <- data.frame(matrix(ncol = 10, nrow = nrow(roadsDC)))

# Generate random scores 1-10 for each column
for (i in 1:10) {
  temp_df[, i] <- sample(1:10, size = nrow(roadsDC), replace = TRUE)
}

# Calculate row-wise mean
row_means <- rowMeans(temp_df, na.rm = TRUE)

# Add the calculated row means as a new column to roadsDC
roadsDC$mean_safetyscore <- row_means


roadsDC <- roadsDC |> 
  select(osm_id, highway, mean_safetyscore, brightness_zscore)

# Create the map
mapview(roadsDC, zcol = "mean_safetyscore")


# Routing test and setting up graph

normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Calculate edge lengths
net <- as_sfnetwork(roadsDC, directed = FALSE) |> 
  activate("edges") |> 
  mutate(edge_len = edge_length())

# Normalize weights from 0 to 1 so that they can be equally weighted
net <- net |>
  activate("edges") |>
  mutate(
    norm_edge_len = as.numeric(normalize(edge_len)),
    norm_safety = normalize(mean_safetyscore),
    norm_brightness = normalize(brightness_zscore)
  )

# Create a composite weight
net <- net |>
  activate("edges") |>
  mutate(
    composite_weight = norm_edge_len + norm_safety + norm_brightness
  )

# Test shortest path
shortest_path <- st_network_paths(net,  from = 221, to = 110, weights = "composite_weight")

# Plot output
plot_path = function(node_path) {
  net |>
    activate("nodes") |>
    slice(node_path) |>
    plot(cex = 1.5, lwd = 1.5, add = TRUE)
}

colors = sf.colors(2, categorical = TRUE)

plot(net, col = "grey")
shortest_path |>
  pull(node_paths) |>
  walk(plot_path)

net |>
  activate("nodes") |>
  st_as_sf() |>
  slice(c(221, 110)) |>
  plot(col = colors, pch = 8, cex = 2, lwd = 2, add = TRUE)


# Our network consists of several unconnected components
with_graph(net, graph_component_count())

# Select the largest graph (9243 elements out fo 12696) - Not ideal but ok for now
connected_net = net |>
  activate("nodes") |>
  filter(group_components() == 1)

plot(net, col = colors[2])
plot(connected_net, cex = 1.1, lwd = 1.1, add = TRUE)

# Transform net to WGS for easier handling in leaflet
connected_net <- st_transform(connected_net, 4326)

#saveRDS(connected_net, file = "data/DC_connected_net.rds")

