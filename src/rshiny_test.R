# Load required libraries
library(shiny)
library(leaflet)
library(sfnetworks)
library(sf)
library(tidygraph)

# Read preprocessed network (make sure this file exists in your working directory)
tryCatch({
  net <- readRDS("data/DC_connected_net.rds")
}, error = function(e) {
  stop("Failed to read preprocessed network. Make sure the file exists.")
})

# Server function
server <- function(input, output, session) {
  
  # Initialize reactive values for the user-defined points
  userPoints <- reactiveVal(data.frame(id = integer(0), lat = numeric(0), lng = numeric(0)))
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = -77.0369, lat = 38.9072, zoom = 12)
  })
  
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
  
  observe({
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addCircleMarkers(data = userPoints(), ~lng, ~lat, radius = 5, color = "red")
  })
  
  observeEvent(userPoints(), {
    if(nrow(userPoints()) == 2) {
      from_point <- st_as_sf(data.frame(x = userPoints()[1, 'lng'], y = userPoints()[1, 'lat']), coords = c("x", "y"), crs = 4326)
      to_point <- st_as_sf(data.frame(x = userPoints()[2, 'lng'], y = userPoints()[2, 'lat']), coords = c("x", "y"), crs = 4326)
   
      
      from_node <- st_nearest_feature(from_point, net) # change this to st_blend and see if it works
      to_node <- st_nearest_feature(to_point, net)
      
      print(paste("From node: ", from_node))
      print(paste("To node: ", to_node))
      
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
      
      if (is.null(shortest_path)) {
        userPoints(data.frame(id = integer(0), lat = numeric(0), lng = numeric(0)))  # Reset the points
      } else {
        # Extract the node paths from the shortest_path object
        node_ids <- shortest_path %>%
          pull(node_paths)
        
        # Extract the corresponding lat/lng from the network object
        net_nodes <- net %>%
          activate("nodes") %>%
          st_as_sf() 
        
        path_coords <- net_nodes[node_ids[[1]], ]
        
        # Extract the coordinates
        path_coordinates <- st_coordinates(path_coords)
        
        # Create a LINESTRING
        path_linestring <- st_linestring(path_coordinates)
        
        # Create an sf object
        path_sf <- st_sf(geometry = st_sfc(path_linestring, crs = st_crs(path_coords)))
        
        # Now add this to the leaflet map
        leafletProxy("mymap") %>%
          addPolylines(data = path_sf, color = "blue", weight = 3)
        
      }
    }
  })
}

# UI function
ui <- fluidPage(
  titlePanel("Safer Route"),
  sidebarLayout(
    sidebarPanel(
      p("Safer Route is a collaborative mapping web application designed to enhance safety awareness for pedestrians by providing insights into the perceived safety of urban roads, paths, and routes.")
    ),
    mainPanel(
      leafletOutput("mymap", height = 800)
    )
  )
)

# Create Shiny app
shinyApp(ui, server)



