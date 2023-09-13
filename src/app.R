# Load required libraries
library(shiny)
library(leaflet)
library(sfnetworks)
library(sf)
library(tidygraph)
library(shinyjs)
library(shinythemes)
library(viridisLite)


# Read preprocessed network (make sure this file exists in your working directory)
tryCatch({
  #net <- readRDS("../data/DC_connected_net.rds")
  #roads <- st_read("../data/roads.gpkg") 
  load("../data/ma.RData")
  
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



pal <- colorNumeric(palette = viridisLite::mako(9), domain = 1:10)
pal1 <- colorNumeric(palette = viridisLite::cividis(9), domain = 1:10)


# Normalize weights from 0 to 1 so that they can be equally weighted
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

ui <- fluidPage(
  theme = shinytheme("paper"),
  navbarPage( 
    "Safer-Route",
    # Title
    tabPanel(width=10,# this is page one in the nav
             "Start", # Heading of the page
             sidebarLayout(sidebarPanel(
               h3("Introduction",
                  style = "padding-bottom: 20px")
             ),
             mainPanel(h4(
               " Some text"
             )))),
    tabPanel(# this is page two in the nav
      "Mannheim",
      sidebarLayout(
        sidebarPanel(
          h3("Text and controls for DC",
             style = "padding-bottom: 20px"),
          tags$p(
            "Begin routing by clicking where you are on the map and then clicking again for your destination. After your route is generated, you can press the 'Rate Safety of Route' button to rate your route on a scale of 1-10. Keep clicking to generate a new route!"
          ),
          actionButton("show_modal_btn", "Rate Safety of Route"),
          checkboxInput("use_st_blend", "Route through open spaces", FALSE),
          
          selectInput("route_pref", "Route preference:",
                      c("Safest Route" = "safe",
                        "Most illuminated Route" = "lit",
                        "Fastest Route" = "fast")
          )
        ),
        mainPanel(
          tags$style(type = "text/css", "#mymap {height: calc(100vh - 180px) !important;}"),
          leafletOutput("mymap"),
          verbatimTextOutput("debug")
        )
      )),
    tabPanel(# this is page two in the nav
      "Washington D.C",
      sidebarLayout(
        sidebarPanel(
          h3("Text and controls for DC",
             style = "padding-bottom: 20px"),
          tags$p(
            "Begin routing by clicking where you are on the map and then clicking again for your destination. After your route is generated, you can press the 'Rate Safety of Route' button to rate your route on a scale of 1-10. Keep clicking to generate a new route!"
          ),
          actionButton("show_modal_btn", "Rate Safety of Route"),
          checkboxInput("use_st_blend", "Route through open spaces", FALSE),
        ),
        mainPanel(h4("DC map"))
      )),
    tabPanel(# this is page two in the nav
      "Munich",
      sidebarLayout(sidebarPanel(
        h3("Text and controls for Munich",
           style = "padding-bottom: 20px")
      ),
      mainPanel(h4(
        "Munich map"
      )))),
    tabPanel(# this is page two in the nav
      "Method",
      sidebarLayout(sidebarPanel(
        h3("Our method",
           style = "padding-bottom: 20px")
      ),
      mainPanel(h4(
        " Some text"
      )))),
    tabPanel(# this is page three in the nav
      "Team",
      sidebarLayout(sidebarPanel(
        h3("Who are we",
           style = "padding-bottom: 20px")
      ),
      mainPanel(h4(
        " Some text"
      ))))
  )
)


server <- function(input, output, session) {
  
  shinyjs::disable("show_modal_btn")
  
  # Initialize reactive values for user-defined points
  userPoints <-
    reactiveVal(data.frame(
      id = integer(0),
      lat = numeric(0),
      lng = numeric(0)
    ))
  node_ids <- reactiveVal(NULL)
  safetyRating <- reactiveVal(NULL)
  userPathID <- reactiveVal(NULL)
  
  bbox <- boundary |> st_bbox() |>  as.numeric()
  bbox_max <- boundary |> st_buffer(1) |> st_bbox() |> as.numeric()
  
  # Initial map rendering
  output$mymap <- renderLeaflet({
    leaflet(data = st_transform(roads, crs = 4326)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #setView(lng = -77.0369, lat = 38.9072, zoom = 12) %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      setMaxBounds(bbox_max[1], bbox_max[2], bbox_max[3], bbox_max[4]) %>%
      addPolylines(
        color = ~ pal(mean_safetyscore),
        weight = 2,
        group = "Safety Score"
      ) %>%
      addPolylines(
        color = ~ pal1(brightness_zscore_rescale),
        weight = 2,
        group = "Brightness Score"
      ) %>%
      addLayersControl(
        overlayGroups = c("Safety Score", "Brightness Score"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Safety Score", "Brightness Score")) %>%
      addLegend(
        pal = pal,
        values = 1:10,
        title = "Safety Score",
        position = "bottomright"
      ) %>%
      addLegend(
        pal = pal1,
        values = ~ 1:10,
        title = "Brightness Score",
        position = "bottomleft"
      )
  })
  
  # Listening for map click events
  observeEvent(input$mymap_click, {
    newPoint <- data.frame(
      id = nrow(userPoints()) + 1,
      lat = input$mymap_click$lat,
      lng = input$mymap_click$lng
    )
    
    print(userPoints)
    # If there are already two points
    if (nrow(userPoints()) >= 2) {
      # Remove the existing markers from the map
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        removeShape(layerId = userPathID())
      
      # Reset the userPoints data frame and add the new point
      userPoints(newPoint)
    } else {
      # If there are fewer than two points, just add the new one
      if (is.data.frame(userPoints())) {
        userPoints(rbind(userPoints(), newPoint))
      } else {
        userPoints(newPoint)
      }
    }
  })
  
  observeEvent(input$ok, {
    safetyRating(input$safety_rating)
    removeModal()
    
    
    net_edges <- net |>
      activate("edges") |>
      st_as_sf()
    
    path_edges <- net_edges[node_ids()[[1]],]
    
    path_edges <- path_edges |>
      mutate(new_safetyscore = safetyRating()) |>
      select(new_safetyscore, osm_id) |>
      st_drop_geometry()
    
    safety_df <- left_join(safety_df, path_edges, by = "osm_id")
    
    row_means <-
      rowMeans(safety_df[, !names(safety_df) %in% "osm_id"], na.rm = TRUE)
    
    roads$mean_safetyscore <- row_means
    
    net <- as_sfnetwork(roads, directed = FALSE) |>
      activate("edges") |>
      mutate(edge_len = edge_length())
    
    net_df <- net %>%
      activate("edges") %>%
      as_tibble()
    
    # Print the mean of the 'mean_safetyscore' column
    print(mean(net_df$mean_safetyscore, na.rm = TRUE))
    
    net <- net |>
      activate("edges") |>
      mutate(
        norm_edge_len = as.numeric(normalize(edge_len)),
        norm_brightness = normalize(brightness_zscore_rescale),
        norm_safety = normalize(mean_safetyscore)
      )
    
    net <- net |>
      activate("edges") |>
      mutate(composite_weight = norm_edge_len + norm_safety + norm_brightness)
    
  })
  
  
  observeEvent(input$show_modal_btn, {
    showModal(modalDialog(
      title = "Rate the Safety of the Route",
      numericInput(
        "safety_rating",
        label = "Rate the safety of the route (1-10):",
        value = 5,
        min = 1,
        max = 10
      ),
      footer = tagList(modalButton("Cancel"),
                       actionButton("ok", "Submit"))
    ))
    
    # Disable the "Rate Safety" button after showing the modal
    shinyjs::disable("show_modal_btn")
  })
  
  # Main observer for userPoints
  observe({
    leafletProxy("mymap") %>%
      addCircleMarkers(
        data = userPoints(),
        ~ lng,
        ~ lat,
        radius = 5,
        color = "red"
      )
    
    if (nrow(userPoints()) == 2) {
      points_df <- data.frame(
        X = c(userPoints()[1, 'lng'], userPoints()[2, 'lng']),
        Y = c(userPoints()[1, 'lat'], userPoints()[2, 'lat'])
      )
      
      # Convert the dataframe into an sf object
      points_sf <-
        st_as_sf(points_df, coords = c("X", "Y"), crs = 4326)
      
      net <- st_network_blend(net, points_sf)
      
      # Get the nearest features
      from_node <- st_nearest_feature(points_sf[1, ], net)
      to_node <- st_nearest_feature(points_sf[2, ], net)
      print(input$route_pref)
      
      route_weight <- switch(
        input$route_pref,
        safe={"composite_weight"},
        fast={"norm_edge_len"},
        lit={"norm_brightness"}
      )
      
      shortest_path <- tryCatch({
        st_network_paths(net,
                         from = from_node,
                         to = to_node,
                         weights = route_weight)
      }, warning = function(w) {
        showModal(
          modalDialog(
            title = "Warning",
            "The selected points are not connected. Please select two new points."
          )
        )
        NULL
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(shortest_path)) {
        node_ids(shortest_path |> pull(node_paths))
        
        # Extract the corresponding lat/lng from the network object
        net_nodes <- net %>%
          activate("nodes") %>%
          st_as_sf()
        
        path_coords <- net_nodes[node_ids()[[1]],]
        
        # Extract the coordinates
        path_coordinates <- st_coordinates(path_coords)
        
        # Create a LINESTRING
        path_linestring <- st_linestring(path_coordinates)
        
        # Create an sf object
        path_sf <-
          st_sf(geometry = st_sfc(path_linestring, crs = st_crs(path_coords)))
        
        # Assign a unique ID to the user path
        userPathID("userPath")
        
        # Remove the previous user path
        if (!is.null(userPathID()) && is.character(userPathID())) {
          leafletProxy("mymap") %>%
            removeShape(layerId = userPathID())
        }
        
        
        # Add the new user path
        leafletProxy("mymap") %>%
          addPolylines(
            data = path_sf,
            color = "blue",
            weight = 3,
            layerId = userPathID()
          )
        
        shinyjs::enable("show_modal_btn")
        
      }
      
    }
  })
}

shinyApp(ui=ui,server=server)