# Load required libraries
library(shiny)
library(leaflet)
library(sfnetworks)
library(sf)
library(tidygraph)

# Read preprocessed network (make sure this file exists in your working directory)
tryCatch({
  net <- readRDS("../data/DC_connected_net.rds")
  roadsDC <- st_read("../data/roadsDC.gpkg") 
  
  safety_df <- roadsDC |> 
    select(osm_id) |> 
    st_drop_geometry()
  
}, error = function(e) {
  stop("Failed to read preprocessed network. Make sure the file exists.")
})

for (i in 1:10) {
  new_column_name <- paste("safetyscore", i, sep = "")
  safety_df[, new_column_name] <- sample(1:10, size = nrow(roadsDC), replace = TRUE)
}

# Normalize weights from 0 to 1 so that they can be equally weighted
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}



# Server function
ui <- fluidPage(
  leafletOutput("mymap"),
  verbatimTextOutput("debug")
)

# Server Logic
server <- function(input, output, session) {
  
  # Initialize reactive values for user-defined points
  userPoints <- reactiveVal(data.frame(id = integer(0), lat = numeric(0), lng = numeric(0)))
  node_ids <- reactiveVal(NULL)
  safetyRating <- reactiveVal(NULL)
  
  # Initial map rendering
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      setView(lng = -77.0369, lat = 38.9072, zoom = 12)
  })
  
  # Listening for map click events
  observeEvent(input$mymap_click, {
    leafletProxy("mymap") %>% clearShapes()
    
    newPoint <- data.frame(id = nrow(userPoints()) + 1,
                           lat = input$mymap_click$lat,
                           lng = input$mymap_click$lng)
    
    if (nrow(userPoints()) >= 2) {
      userPoints(data.frame(id = integer(0), lat = numeric(0), lng = numeric(0)))
    }
    
    userPoints(rbind(userPoints(), newPoint))
  })
 
  observeEvent(input$ok, {
    safetyRating(input$safety_rating)
    removeModal()

    
    net_edges <- net |>
      activate("edges") |>
      st_as_sf() 
    
    path_edges <- net_edges[node_ids()[[1]], ]
    
    path_edges <- path_edges |> 
      mutate(new_safetyscore = safetyRating()) |> 
      select(new_safetyscore, osm_id) |> 
      st_drop_geometry()
    
    safety_df <- left_join(safety_df, path_edges, by = "osm_id")
    
    row_means <- rowMeans(safety_df[, !names(safety_df) %in% "osm_id"], na.rm = TRUE)
    
    roadsDC$mean_safetyscore <- row_means
    
    net <- as_sfnetwork(roadsDC, directed = FALSE) |> 
      activate("edges") |> 
      mutate(edge_len = edge_length())
    
    net <- net |>
      activate("edges") |>
      mutate(
        norm_edge_len = as.numeric(normalize(edge_len)),
        norm_brightness = normalize(brightness_zscore),
        norm_safety = normalize(mean_safetyscore)
      )
    
    net <- net |>
      activate("edges") |>
      mutate(
        composite_weight = norm_edge_len + norm_safety + norm_brightness
      )
    
  })
  
  
  # Main observer for userPoints
  observe({
    leafletProxy("mymap") %>% 
      clearMarkers() %>% 
      addCircleMarkers(data = userPoints(), ~lng, ~lat, radius = 5, color = "red")
    
    if(nrow(userPoints()) == 2) {
      from_point <- st_as_sf(data.frame(x = userPoints()[1, 'lng'], y = userPoints()[1, 'lat']), coords = c("x", "y"), crs = 4326)
      to_point <- st_as_sf(data.frame(x = userPoints()[2, 'lng'], y = userPoints()[2, 'lat']), coords = c("x", "y"), crs = 4326)
      from_node <- st_nearest_feature(from_point, net)
      to_node <- st_nearest_feature(to_point, net)
      
      shortest_path <- tryCatch({
        st_network_paths(net, from = from_node, to = to_node, weights = "composite_weight")
      }, warning = function(w) {
        showModal(modalDialog(
          title = "Warning",
          "The selected points are not connected. Please select two new points."
        ))
        NULL
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(shortest_path)) {
        
        node_ids(shortest_path |> pull(node_paths))
        
        
        # Extract the corresponding lat/lng from the network object
        net_nodes <- net |>
          activate("nodes") |>
          st_as_sf() 
        
        path_coords <- net_nodes[node_ids()[[1]], ]
        
        # Extract the coordinates
        path_coordinates <- st_coordinates(path_coords)
        
        # Create a LINESTRING
        path_linestring <- st_linestring(path_coordinates)
        
        # Create an sf object
        path_sf <- st_sf(geometry = st_sfc(path_linestring, crs = st_crs(path_coords)))
        
        # Now add this to the leaflet map
        leafletProxy("mymap") |>
          addPolylines(data = path_sf, color = "blue", weight = 3)
        
        
        showModal(modalDialog(
          title = "Rate the Safety of the Route",
          numericInput("safety_rating", label = "Rate the safety of the route (1-10):", value = 5, min = 1, max = 10),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Submit")
          )
        ))
        
      }
    }
  })
}

# Create Shiny app
shinyApp(ui, server)




