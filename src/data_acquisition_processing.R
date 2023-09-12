# Head ---------------------------------
# purpose: Script to retrieve all relevant datasets and process them ready for the app to consume
# author: Marcel
#
#
#1 Libraries ---------------------------------

library(tidyverse)
library(sf)
library(osmdata)
library(osmextract)
library(sfnetworks)
library(tidygraph)
library(scales)

#2 Functions ---------------------------------


source("src/get_mapillary.R")

get_boundary <- function(name) {
  boundary <- opq(name) %>% #search for munich
    add_osm_feature (key = "admin_level", value = "6") %>%
    add_osm_feature (key = "name", value = name) %>%
    osmdata_sf ()

  #extract polygon
  boundary <- boundary$osm_multipolygons
  return(boundary)

}


get_roads <- function(name, boundary) {
  roads = oe_get(
    name, # oberbayern , us/district-of-columbia, based of geofabrik boundaries
    quiet = FALSE,
    query = "SELECT *
  FROM 'lines'
  WHERE highway in
  ('motorway', 'motorway_link', 'trunk_link', 'trunk', 'primary', 'secondary', 'tertiary', 'residential', 'primary_link', 'secondary_link', 'tertiary_link', 'living_street', 'unclassified')",
  download_directory = "data"
  )

  roads <- st_filter(roads,boundary)

  return(roads)

}


get_official_streetlights <- function(boundary){
  if (!file.exists("data/Street_Lights.geojson")) {
    options(timeout=500)
    download.file(
      "https://opendata.arcgis.com/api/v3/datasets/6cb6520725b0489d9a209a337818fad1_90/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1",
      destfile = "data/Street_Lights.geojson",
      mode = "wb",
      method="libcurl",
      #quiet = T,
    )
  }
  street_lights <- st_read("data/Street_Lights.geojson", quiet=T)

  street_lights <- st_filter(boundary, street_lights)
  return(street_lights)
}


link_roads_lights <- function(roads, street_lights) {
  # Coordinate transformation


  # Add buffer distance based on road type
  roads <- roads |>
    mutate(
      buffer_distance = case_when(
        highway == 'motorway' ~ 30,
        # in meters
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
        highway == 'living_street' ~ 10,
        highway == 'unclassified' ~ 10,
        TRUE ~ 5
      )
    )

  # Keep the original geometry for later use
  roads_geom <- st_geometry(roads)

  # Create buffered geometries
  roads <- roads |> st_buffer(dist = roads$buffer_distance)


  roads$light_count <- roads |>
    st_intersects(street_lights) |> lengths()

  # Compute light count per area
  roads <- roads |>
    mutate(lights_per_area = light_count / as.numeric(st_area(geometry)))

  # Compute z-scores based on road types
  roads <- roads |>
    group_by(highway) |>
    mutate(
      mean_value = mean(lights_per_area, na.rm = TRUE),
      sd_value = sd(lights_per_area, na.rm = TRUE),
      brightness_zscore = (lights_per_area - mean_value) / sd_value
    ) |>
    ungroup() |> 
    mutate(brightness_zscore_rescale = scales::rescale(brightness_zscore, to=c(1,10)))

  st_geometry(roads) <- roads_geom

  return(roads)
}


process_graph <- function(roads){
  temp_df <- data.frame(matrix(ncol = 10, nrow = nrow(roads)))

  # Generate random scores 1-10 for each column
  for (i in 1:10) {
    temp_df[, i] <- sample(1:10, size = nrow(roads), replace = TRUE)
  }

  # Calculate row-wise mean
  row_means <- rowMeans(temp_df, na.rm = TRUE)

  # Add the calculated row means as a new column to roads
  roads$mean_safetyscore <- row_means


  roads <- roads |>
    select(osm_id, highway, mean_safetyscore, brightness_zscore)

  

  # Routing test and setting up graph

  normalize <- function(x) {
    return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }

  # Calculate edge lengths
  net <- as_sfnetwork(roads, directed = FALSE) |>
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

  # select the largest component
  net = net |>
    activate("nodes") |>
    filter(group_components() == 1)

  return(net)
}

#3 setup ---------------------------------


if (!dir.exists("data")) {
  dir.create("data", )
}

config <- fromJSON("config/config.json")


#3 run acquisition ---------------------------------

munich_boundary <- get_boundary("München")
dc_boundary <- get_boundary("Washington")
ma_boundary <- get_boundary("Mannheim")


munich_roads <- get_roads("oberbayern", munich_boundary)
dc_roads <- get_roads("us/district-of-columbia", dc_boundary)
ma_roads <- get_roads("regierungsbezierk karlsruhe", ma_boundary)

munich_lights <- get_mapillary(config$mapillary_api_key, munich_boundary, "object--street-light", "id")
# returns a list of the lights and the grid with counts
dc_lights <- get_official_streetlights(dc_boundary)
ma_lights <- get_mapillary(config$mapillary_api_key, ma_boundary, "object--street-light", "id")

munich_lights_grid <- munich_lights[[2]] # just for validating amount of lights per cell
munich_lights <- munich_lights[[1]]

ma_lights_grid <- ma_lights[[2]]
ma_lights <- ma_lights[[1]]

#3 run processing ---------------------------------

ma_roads <- st_transform(ma_roads, 25832)
dc_roads <- st_transform(dc_roads, 32618)
munich_roads <- st_transform(munich_roads, 25832)

ma_lights <- st_transform(ma_lights, 25832)
dc_lights <- st_transform(dc_lights, 32618)
munich_lights <- st_transform(munich_lights, 25832)

munich_roads <- link_roads_lights(munich_roads, munich_lights)
dc_roads <- link_roads_lights(dc_roads, dc_lights)
ma_roads <- link_roads_lights(ma_roads, ma_lights)

# transform back to WGS 84
munich_roads <- st_transform(munich_roads, 4326)
dc_roads <- st_transform(dc_roads, 4326)
ma_roads <- st_transform(ma_roads, 4326)

munich_net <- process_graph(munich_roads)
dc_net <- process_graph(dc_roads)
ma_net <- process_graph(ma_roads)

boundary <- munich_boundary
net <- munich_net
roads <- munich_roads
save(boundary, net, roads, file = "data/munich.RData")

boundary <- dc_boundary
net <- dc_net
roads <- dc_roads
save(boundary, net, roads, file = "data/dc.RData")

boundary <- ma_boundary
net <- ma_net
roads <- ma_roads
save(boundary, net, roads, file = "data/ma.RData")
