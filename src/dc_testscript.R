# Head ---------------------------------
# purpose: Script to retrieve all relevant datasets
# author: Charlie
# revised by: Marcel
#
#
#1 Libraries ---------------------------------


library(tidyverse)
library(sf)
library(tidyverse)
library(mapview)
library(sfnetworks)
library(tidygraph)
library(osmextract)

library(tictoc)


#2 Input Data ---------------------------------

if (!dir.exists("data")) {
  dir.create("data")
}

tic("Downloaded street lights from DC")
#file.remove("data/Street_Lights.geojson")
if (!file.exists("data/Street_Lights.geojson")) {
  download.file(
    "https://opendata.arcgis.com/api/v3/datasets/6cb6520725b0489d9a209a337818fad1_90/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1",
    destfile = "data/Street_Lights.geojson",
    mode = "wb",
    quiet = T,
    timeout = 1000
  )
}
toc()



street_lightsDC <- st_read("data/Street_Lights.geojson")
#roadsDC <- st_read("data/district-of-columbia-roads.osm.pbf", layer = 'lines')

roadsDC = oe_get(
  "us/district-of-columbia", # pbf file of Washington
  quiet = FALSE, # verbose execution
  query = "SELECT *
  FROM 'lines'
  WHERE highway in
  ('footway', 'path', 'motorway', 'motorway_link', 'trunk_link', 'trunk', 'primary', 'secondary', 'tertiary', 'residential', 'primary_link', 'secondary_link', 'tertiary_link')",
  download_directory = "data"
)

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
    highway == 'path' ~ 5,
    TRUE ~ 5
  ))
# Keep the original geometry for later use
roadsDC_geom <- st_geometry(roadsDC)

# Keep the original geometry for later use
roadsDC_geom <- st_geometry(roadsDC)

# Create buffered geometries
roadsDC <- roadsDC |> st_buffer(dist=roadsDC$buffer_distance)


# Count the lights for each road segment
# TODO think this can be easier done with st_filter

roadsDC$light_count <- roadsDC |>
  st_intersects(street_lightsDC) |> lengths()

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
safety_df <- roadsDC |>
  select(osm_id) |>
  st_drop_geometry()

# Generate random scores 1-10 for each column
for (i in 1:10) {
  new_column_name <- paste("safetyscore", i, sep = "")
  safety_df[, new_column_name] <- sample(1:10, size = nrow(roadsDC), replace = TRUE)
}

# Calculate row-wise mean
row_means <- rowMeans(safety_df[, !names(safety_df) %in% "osm_id"], na.rm = TRUE)

# Add the calculated row means as a new column to roadsDC
roadsDC$mean_safetyscore <- row_means


roadsDC <- roadsDC |>
  select(osm_id, highway, mean_safetyscore, brightness_zscore)

# Create the map
mapview(roadsDC, zcol = "mean_safetyscore")


# Routing test and setting up graph
roadsDC <- st_read("data/roadsDC.gpkg")

# roadsDC <- roadsDC |>
#   select(-mean_safetyscore)

# Calculate edge lengths
net <- as_sfnetwork(roadsDC, directed = FALSE) |>
  activate("edges") |>
  mutate(edge_len = edge_length())



# Normalize weights from 0 to 1 so that they can be equally weighted
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}


net <- net |>
  activate("edges") |>
  mutate(
    norm_edge_len = as.numeric(normalize(edge_len)),
    norm_brightness = normalize(brightness_zscore),
    norm_safety = normalize(mean_safetyscore)
  )

# Create a composite weight
net <- net |>
  activate("edges") |>
  mutate(
    composite_weight = norm_edge_len + norm_safety + norm_brightness
  )

# Test shortest path
shortest_path <- st_network_paths(net,  from = 221, to = 110, weights = "composite_weight")

node_ids <- shortest_path |>
  pull(node_paths)

net_nodes <- net |>
  activate("edges") |>
  st_as_sf()

path_coords <- net_nodes[node_ids[[1]], ]

path_coords <- path_coords |>
  mutate(new_safetyscore = 7) |>
  select(new_safetyscore, osm_id) |>
  st_drop_geometry()

safety_df <- left_join(safety_df, path_coords, by = "osm_id")

row_means <- rowMeans(safety_df[, !names(safety_df) %in% "osm_id"], na.rm = TRUE)

# Add the calculated row means as a new column to roadsDC
roadsDC$mean_safetyscore <- row_means




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

saveRDS(connected_net, file = "data/DC_connected_net.rds")
st_write(roadsDC, "data/roadsDC.gpkg", append=F)
