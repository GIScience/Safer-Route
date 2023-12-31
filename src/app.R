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
  #load("../data/ma.RData")
  load("../data/data.RData")
  
}, error = function(e) {
  stop("Failed to read preprocessed network. Make sure the file exists.")
})



pal <- colorNumeric(palette = viridisLite::mako(9), domain = 1:10)
pal1 <- colorNumeric(palette = viridisLite::cividis(9), domain = 1:10)


# safety_global <- list("ma" = roads[["ma"]] |> select(c("row_id", "mean_safetyscore")) |> st_drop_geometry(), "dc" = roads[["dc"]] |> select(c("row_id", "mean_safetyscore")) |> st_drop_geometry(), "munich" = roads[["munich"]] |> select(c("row_id", "mean_safetyscore")) |> st_drop_geometry())

# Normalize weights from 0 to 1 so that they can be equally weighted
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

create_map <- function(map_data, map_boundary) {
  
  bbox <- map_boundary |> st_bbox() |>  as.numeric()
  bbox_max <- map_boundary |> st_buffer(4) |> st_bbox() |> as.numeric()
  
  
  lmap <- renderLeaflet({
    leaflet(
      data = NULL
            ) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #setView(lng = -77.0369, lat = 38.9072, zoom = 12) %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      setMaxBounds(bbox_max[1], bbox_max[2], bbox_max[3], bbox_max[4]) 
    # %>% 
    #   addPolylines(
    #     color = ~ pal(mean_safetyscore),
    #     weight = 2,
    #     group = "Safety Score"
    #   ) %>%
    #   addPolylines(
    #     color = ~ pal1(brightness_zscore_rescale),
    #     weight = 2,
    #     group = "Brightness Score"
    #   ) %>%
    #   addLayersControl(
    #     overlayGroups = c("Safety Score", "Brightness Score"),
    #     options = layersControlOptions(collapsed = FALSE)
    #   ) %>%
    #   hideGroup(c("Safety Score", "Brightness Score")) %>%
    #   addLegend(
    #     pal = pal,
    #     values = 1:10,
    #     title = "Safety Score",
    #     position = "bottomright"
    #   ) %>%
    #   addLegend(
    #     pal = pal1,
    #     values = ~ 1:10,
    #     title = "Brightness Score",
    #     position = "bottomleft"
    #   )
  })
  return(lmap)
  
}

interactive_map <- function(input, output, map_boundary, map_net, map_roads, location_select){
  
  
  # Initialize reactive values for user-defined points
  userPoints <-
    reactiveVal(data.frame(
      Y = numeric(0),
      X = numeric(0)
    ))
  node_ids <- reactiveVal(NULL)
  safetyRating <- reactiveVal(NULL)
  userPathID <- reactiveVal(NULL)
  
  
  # Initial map rendering
  output$mymap <- create_map(map_roads, map_boundary)
  
  
  # Listening for map click events
  observeEvent(input$mymap_click, {
    newPoint <- data.frame(
      Y = input$mymap_click$lat,
      X = input$mymap_click$lng
    )
    
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
    
    # net_edges <- map_net |>
    #   activate("edges") |>
    #   st_as_sf()
    # 
    # path_edges <- net_edges[node_ids()[[1]],]
    # 
    # 
    # path_edges <- path_edges |>
    #   mutate(new_safetyscore = safetyRating()) |>
    #   select(new_safetyscore, row_id) |>
    #   st_drop_geometry()
    # 
    # safety_df <- safety_global[[location_select]]
    # safety_df <- left_join(safety_df, path_edges, by = "row_id")
    # 
    # row_means <-
    #   rowMeans(safety_df[, !names(safety_df) %in% "row_id"], na.rm = TRUE)
    # 
    # check1 <- inner_join(safety_df, path_edges, by = "row_id")
    # row_means1 <- rowMeans(check1[, !names(check1) %in% "row_id"], na.rm = TRUE)
    # 
    # map_roads$mean_safetyscore <- row_means
    # 
    # # map_net <- as_sfnetwork(map_roads, directed = FALSE) |>
    # #   activate("edges") |>
    # #   mutate(edge_len = edge_length())
    # #   
    # 
    # 
    # map_net <- map_roads |> 
    #   as_sfnetwork(directed=TRUE)
    # 
    # map_net <- map_net |>
    #   activate("edges") |>
    #   mutate(
    #     norm_weight = as.numeric(normalize(weight)),
    #     norm_safety = normalize(mean_safetyscore),
    #     norm_brightness = normalize(brightness_zscore_rescale)
    #   ) 
    # 
    # 
    # net_df <- map_net %>%
    #   activate("edges") %>%
    #   as_tibble()
    # 
    # 
    # 
    # map_net <- map_net |>
    #   activate("edges") |>
    #   mutate(composite_weight = norm_safety)
    # 
    # # Normalize weights from 0 to 1 so that they can be equally weighted
    # if (location_select == "ma") {
    #   ncol(safety_df)
    #   ncol(safety_global[["ma"]])
    #   safety_global[["ma"]] <<- safety_df
    #   net[["ma"]] <- map_net
    # } else if (location_select == "dc") {
    #   safety_global[["dc"]] <<- safety_df
    #   net[["dc"]] <- map_net
    # } else if (location_select == "munich") {
    #   safety_global[["munich"]] <<- safety_df
    #   net[["munich"]] <- map_net
    #   
    # }
  
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
        ~ X,
        ~ Y,
        radius = 5,
        color = "red"
      )
    
    
    points_df <- userPoints()
    
    if (nrow(points_df) == 2) {
      points_sf <- st_as_sf(points_df, coords = c("X", "Y"), crs = 4326)
      
      distance <- as.numeric(st_distance(points_sf[1, ], points_sf[2, ]), units = "m")
      
      if (distance >= 50000) {
        userPoints(points_df[2, , drop = FALSE])
      }
      else {
        
    # if (nrow(userPoints()) == 2) {
    #   points_df <- data.frame(
    #     X = c(userPoints()[1, 'X'], userPoints()[2, 'X']),
    #     Y = c(userPoints()[1, 'Y'], userPoints()[2, 'Y'])
    #   )
      
      # Convert the dataframe into an sf object
        #map_roads <- st_network_blend(map_net, points_sf)
        
        # Get the nearest features
        from_node <- st_nearest_feature(points_sf[1, ], map_net)
        to_node <- st_nearest_feature(points_sf[2, ], map_net)
        
        route_weight <- switch(
          input$route_pref,
          safe={"composite_weight"},
          fast={"norm_weight"},
          lit={"norm_brightness"}
        )
        
        shortest_path <- tryCatch({
          st_network_paths(map_net,
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
          net_nodes <- map_net %>%
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
    }
  })
  return(output)
}


server <- function(input, output, session) {
  observeEvent(input$location_ma, {
    location_select <- "ma"
    
    
    map_boundary <- boundaries[[location_select]]
    map_net <- net[[location_select]]
    map_roads <- roads[[location_select]]
    output <-
      interactive_map(input, output, map_boundary, map_net, map_roads, location_select)
    
  })
  
  observeEvent(input$location_dc, {
    location_select <- "dc"
    
    
    map_boundary <- boundaries[[location_select]]
    map_net <- net[[location_select]]
    map_roads <- roads[[location_select]]
    
    output <-
      interactive_map(input, output, map_boundary, map_net, map_roads, location_select)
    
  })
  
  observeEvent(input$location_munich, {
    location_select <- "munich"
    
    
    map_boundary <- boundaries[[location_select]]
    map_net <- net[[location_select]]
    map_roads <- roads[[location_select]]
    
    
    output <-
      interactive_map(input, output, map_boundary, map_net, map_roads, location_select)
    
    
  })
  
  
  # observeEvent(input$loadButton, {
  #   leafletProxy("mymap") %>%
  #     addPolylines(
  #           color = ~ pal1(brightness_zscore_rescale),
  #           weight = 2,
  #           group = "Brightness Score",
  #           data=map_roads
  #         )
  # })
  
  # starting
  
  location_select <- "ma"
  
  map_boundary <- boundaries[[location_select]]
  map_net <- net[[location_select]]
  map_roads <- roads[[location_select]]
  
  shinyjs::disable("show_modal_btn")
  
  output <-
    interactive_map(input, output, map_boundary, map_net, map_roads, location_select)
  
  
}


ui <- fluidPage(
  theme = shinytheme("paper"),
  navbarPage(
    "Safer-Route",
    tabPanel(
      "Home",
      sidebarLayout(
        sidebarPanel(
          tags$p(
            "Begin routing by clicking where you are on the map and then clicking again for your destination. After your route is generated, you can press the 'Rate Safety of Route' button to rate your route on a scale of 1-10. Keep clicking to generate a new route! This is a prototype so the rating function isn't fully implemented and loading times may vary.",
            style = "font-size: 16px"
          ),
          h5("Route Options"),
          selectInput(
            "route_pref",
            label = NULL,
            choices = c(
              "Safest Route" = "safe",
              "Most illuminated Route" = "lit",
              "Fastest Route" = "fast"
            )
          ),
          h5("Locations"),
          actionButton("location_ma", "Mannheim"),
          actionButton("location_dc", "Washington D.C."),
          actionButton("location_munich", "Munich"),
          h5("Rate safety"),
          actionButton("show_modal_btn", "Rate Safety of Route"),
          h5("Visualize Dimensions"),
        ),
        mainPanel(
          tags$style(type = "text/css", "#mymap {height: calc(100vh - 180px) !important;}"),
          leafletOutput("mymap"),
          verbatimTextOutput("debug")
        )
      )
    ),
    tabPanel(
      "Method",
      sidebarLayout(
        sidebarPanel(
          h5("Method"),
        ),
        mainPanel(
          tags$p(
            "We utilized three main datasets in the development of Safer-Route: Road segments, Street Lights, and a Network Graph (derived from roads). To retrieve this data for each city (Munich, Washington DC, Mannheim) we sourced our roads from OpenStreetMap and in R queried the OSM pbf with geofabrik. For the street light data we used official data for Washington DC and Mapillary for Munich and Mannheim. The street lights data was fused with the road dataset and subsequently transformed into a network graph. This step is crucial in incorporating illuminance information into the routing algorithm.",
            style = "font-size: 16px"
          ),
          tags$p(
            "Within our application, the primary dataset employed is the network graph. Users can interact with this dataset through the user-friendly frontend interface to generate routes. They have the option to choose from three types of route: fastest, safest, and brightest. The fastest route emphasizes road segment lengths exclusively, the safest relies on user generated scores, and the brightest is based on the density of street lamps per road segment. Users play an active role in enhancing route safety in the Safer Route app. After creating routes, they have the opportunity to rate them based on their experience. These scores then update the overall safety score for road segments therefore other users benefit from this valuable information, making the application a dynamic and collaborative platform for urban navigation.", style = "font-size: 16px")
        )
      )
    ),
    tabPanel(
      "Team",
      sidebarLayout(
        sidebarPanel(
          h3("Team"),
          p(
            "We are a group of student research assistants and PhD students at the Heidelberg Institute for Geoinformation Technology (HeiGIT)",
            style = "font-size:22px"
          ),
          tags$a(
            href = 'https://www.heigit.org',
            tags$img(src = 'small_heigit_logo.png', height = "100px"),
            style = "padding-bottom: 20px"
          )
        ),
        mainPanel(
          fluidRow(
            column(
              width = 5,
              style = "margin-top:18px",
              tags$p(
                "Maximiliane, a dedicated Geography student and research assistant at heigit, recently completed her Bachelor's degree with a thesis focusing on accessibility analysis for maternal health in Bali. Her invaluable contribution involved working on mapillary data acquisition and integration, enriching our project with her expertise in accessibility and spatial analysis.",
                style = "font-size:18px"
              )
            ),
            column(
              width = 3,
              offset = 1,
              tags$img(
                src = 'kitzinger_cut_alpha.png',
                height = 300,
                style = ""
              )
            )
          ),
          fluidRow(
            column(
              width = 5,
              style = "margin-top:18px",
              tags$p(
                "Valentin, a devoted Geography student with a fervor for cycling, plays a pivotal role as a student research assistant at heigit. His efforts are focused on supporting the geoinformation for humanitarian aid groups in projects related to forecast-based financing in Somalia and Sudan. Valentin excels in data transformation and modeling, ensuring our application can effectively serve its purpose.",
                style = "font-size:18px"
              )
            ),
            column(
              width = 3,
              tags$img(
                src = 'boehmer_cut_alpha.png',
                height = 300,
                style = ""
              )
            )
          ),
          fluidRow(
            column(
              width = 5,
              style = "margin-top:18px",
              tags$p(
                "Charles is a PhD student researching emerging vector-borne diseases in Europe. He leverages his background in urban planning to advocate for social equity and ensure a critical yet inclusive perspective to the project. Within our project, he takes charge of the backend development of the R shiny app, ensuring robust functionality and seamless user experience.",
                style = "font-size:18px"
              )
            ),
            column(
              width = 3,
              offset = 1,
              tags$img(
                src = 'hatfield_cut_alpha.png',
                height = 300,
                style = ""
              )
            )
          ),
          fluidRow(
            column(
              width = 5,
              style = "margin-top:18px",
              tags$p(
                "Marcel, a dedicated Research Associate at HeiGIt and a natural-born Geographer, took on the role of project coordinator and led the engineering efforts on the frontend. His expertise has been pivotal in shaping the project's direction and ensuring a user-friendly interface that aligns seamlessly with our vision for safe urban routing.",
                style = "font-size:18px"
              )
            ),
            column(
              width = 3,
              tags$img(
                src = 'reinmuth_cut_alpha.png',
                height = 300,
                style = ""
              )
            )
          )
        )
      )
    )
  )
)



shinyApp(ui = ui, server = server)
