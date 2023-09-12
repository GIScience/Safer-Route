# Load required libraries
library(shiny)
library(leaflet)
library(sfnetworks)
library(sf)
library(tidygraph)
library(shinyjs)
library(shinythemes)


# Read preprocessed network (make sure this file exists in your working directory)
tryCatch({
  #net <- readRDS("../data/DC_connected_net.rds")
  #roads <- st_read("../data/roads.gpkg") 
  load("data/munich.RData")
  
  safety_df <- roads |> 
    select(osm_id) |> 
    st_drop_geometry()
  
  
  
}, error = function(e) {
  stop("Failed to read preprocessed network. Make sure the file exists.")
})

for (i in 1:10) {
  new_column_name <- paste("safetyscore", i, sep = "")
  safety_df[, new_column_name] <- sample(1:10, size = nrow(roads), replace = TRUE)
}

row_means <- rowMeans(safety_df[, !names(safety_df) %in% "osm_id"], na.rm = TRUE)

roads$mean_safetyscore <- row_means

pal <- colorNumeric(palette = "viridis", domain = roads$mean_safetyscore)
pal1 <- colorNumeric(palette = "viridis", domain = roads$brightness_zscore)


# Normalize weights from 0 to 1 so that they can be equally weighted
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

runApp("src/app", launch.browser = T)