# Head ---------------------------------
# purpose: Script to retrieve all relevant datasets and process them ready for the app to consume
# author: Marcel
#
#
#1 Libraries ---------------------------------

library(tidyverse)
library(httr) # requests
library(tictoc) # timing
library(jsonlite) # manage JSON format
library(geojsonsf) # geojson parser
library(sfnetworks)
library(parallel)
library(doParallel)
library(tidygraph)
library(dplyr, warn.conflicts = FALSE)

#2 Function ---------------------------------

get_graph <- function(host="localhost", port=12345, aoi_bbox, aoi, profile, no_cores) {
  #' Export, parse and postprocess a openrouteservice graph with weights
  #'
  #' From a local openrouteservice instance we export the graph with weights, parse the response json to a sf dataframe. Then we create unique (unidirectional) segment ids and bidirectional ids, as openrouteservice is a directed graph. Then we return a sf dataframe with the edges and a sfnet object.
  #'
  #' @param host location where openrouteservice is running
  #' @param port port on which openrouteservice is listening
  #' @param aoi_bbox bbox object
  #' @param aoi actual aoi geom to cut the graph TODO remove
  #' @param profile routing profile
  #' @param no_cores how many cores available to process the graph
  
  tic(paste0("##  - ", profile," ##")) # logging whole process
  tic("export of net") # logging subprocess
  
  body <- toJSON(list(bbox = fromJSON(paste0('[[',aoi_bbox$xmin,',',aoi_bbox$ymin,'],[',aoi_bbox$xmax,',',aoi_bbox$ymax,']]'))),
                 auto_unbox = T) # create request body with parameters
  url <-  paste0("http://", host,":",port,"/ors/v2/export/",profile,"/json") # api request to local ORS
  
  
  # post request ors api
  resp <- POST(
    url = url,
    encode = "raw",
    body = body,
    httr::add_headers(`accept` = 'application/json'),
    httr::content_type('application/json'),
    verbose()
  )
  
  resp_content <- httr::content(resp, as = "text") # parse response
  toc() # logging subprocess end
  
  tic("response to sf net") # next subprocess
  
  # the response is given as 2 lists :locations and edgeScores, we extract them
  nodes <- fromJSON(resp_content)$nodes # nodes ID and coordinate
  edges <- fromJSON(resp_content)$edges # edges score defined with 2 nodes
  
  # we generate the data frame with the 2 nodes which define the edge and the score
  edges_temp <- merge(edges, nodes, by.x = "fromId", by.y="nodeId") %>% rename(from_coord = location)
  edges_df <- merge(edges_temp, nodes, by.x = "toId", by.y="nodeId") %>% rename(to_coord = location)
  
  # split the coordinates of both point to longitude and latitude colums
  x <- split(edges_df, sample(rep(1:no_cores, ceiling(nrow(edges_df)/no_cores))))
  temp_res <- mclapply(x, function(edges_df){
    for (i in 1:length(edges_df$weight)) {
      edges_df$from_lon[i] <- edges_df$from_coord[i][[1]][[1]]
      edges_df$from_lat[i] <- edges_df$from_coord[i][[1]][[2]]
      edges_df$to_lon[i] <- edges_df$to_coord[i][[1]][[1]]
      edges_df$to_lat[i] <- edges_df$to_coord[i][[1]][[2]]
    }
    return(edges_df)
  }, mc.cores=no_cores)
  edges_df <- dplyr::bind_rows(temp_res)
  
  # create a geometry attribute as character
  edges_df$geom = sprintf("LINESTRING(%s %s, %s %s)",
                          edges_df$from_lon,
                          edges_df$from_lat,
                          edges_df$to_lon,
                          edges_df$to_lat)
  # add a unidirectional Id for each segement
  edges_df$unidirectId <- seq.int(nrow(edges_df))
  
  edges_df <- subset(edges_df,select = c('toId' , 'fromId','weight', "geom", "unidirectId")) %>% tibble()
  
  # create a bidirectional Id attribute for each segment regardless to its direction
  edges_map <- edges_df %>% mutate( toFrom = map2_chr( toId, fromId, ~str_flatten(sort(c(.x,.y)), collapse = "_") ) )
  edges_map_y <- edges_map %>% group_by(toFrom) %>% filter( n() == 2 ) %>% ungroup %>%   # Keep groups of size 2
    dplyr::select(toFrom) %>% distinct %>% mutate( bidirectId = 1:n() )
  
  edges_df <- left_join( edges_map, edges_map_y ) %>% dplyr::select(-toFrom)
  
  
  # bind together into one sf
  edges_sf <- edges_df %>%
    st_as_sf( wkt = "geom", crs= 4326)
  
  edges_sf <- edges_sf %>% filter(
    st_intersects(geom,
                  st_geometry(aoi),
                  sparse = F,
                  prepared = T)
  )
  
  # create a net
  net <- edges_sf %>%
    as_sfnetwork(directed=TRUE) %>%
    activate("edges") %>%
    mutate(weight = weight)
  
  
  toc() # end subprocess
  toc() # end main process
  return(list(edges_sf, net))
  #return(net)
  
  
}

