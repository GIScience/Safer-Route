
#Packages ----------------
library(httr)
library (jsonlite)
library(sf)
library(mapview)
library(dplyr)
library(osmdata)
library(tidyverse)
library(glue)
library(tictoc)


#Functions ----------

mapillary_object_parse <- function(json_string) {

    #convert string to data.frame
    df_raw <- fromJSON(json_string)
    df <- data.frame(df_raw)

    #empty vectors for latitude and longitude
    lat <- c()
    lon <- c()

    #loop over df to extract each lon and lat value and add it to lat/lon
    for (i in 1:nrow(df)){
      row <- df[i,]
      lat <- c(lat, row$data.geometry$coordinates[[1]][2])
      lon <- c(lon, row$data.geometry$coordinates[[1]][1])
    }

    # add vectors to data frame
    df$lon <- lon
    df$lat <- lat

    #select only necessary columns
    df <- df %>% select(data.id, data.object_value, lon, lat)

    #build geometry column
    df <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

    #rename columns
    df <- df %>%
      rename(id = data.id,
             object_value = data.object_value)

    return(df)
  }

bbox_to_objects <- function(bbox) {
  # take a st_bbox output, request mapbox api, parse to df, deliver back df
  #send request to API
  coords <- bbox %>% as.numeric()

  res <- VERB("GET", url = glue("https://graph.mapillary.com/map_features?access_token={api_access}&fields={fields},object_value,geometry&bbox={coords[1]},{coords[2]},{coords[3]},{coords[4]}&object_values={object_values}"))

  json_string <- content(res, 'text')

  if (fromJSON(json_string)$data %>% is_empty()) {return(data.frame())}

  #use function to get data frame
  df <- mapillary_object_parse(json_string)
  return(df)
}




#Grid Preparation ---------------
#Get OSM Data
munich <- opq("munich") %>% #search for munich
  add_osm_feature (key = "admin_level", value = "6") %>%
  add_osm_feature (key = "name", value = "München") %>%
  osmdata_sf ()

#extract polygon
munich <- munich$osm_multipolygons

#make grid
munich_grid <- st_make_grid(
    munich,
    n = c(10, 10),
    square = TRUE
  )

#convert to sf object
munich_grid_sf <- st_as_sf(munich_grid)



#Mapillary Data--------------

#define access_code
api_access <- "MLY|9986775024730224|7cba4c716e4096343f968f454f672949"

#define objects
object_values <- "object--street-light"

#define fields
fields <- "id"



#Get Mapillary Data
munich_grid_sf$count <- NA

#check if df exists already
if (exists("result_df")) {
  rm(result_df)
}
for (i in 1:nrow(munich_grid_sf)){
  tic(glue("{i}/{nrow(munich_grid_sf)}"))
  #get coordinates of bboxes
  bbox <- munich_grid_sf[i,] %>% st_bbox()

  df <- bbox_to_objects(bbox)

  if (df %>% is_empty()) {next} #check if df is empty or zero length

  if (nrow(df)==2000) {
      sub_grid <- st_make_grid(
        munich_grid_sf[i,],
        n = c(2, 2),
        square = TRUE
      )
      sub_grid <- st_as_sf(sub_grid)
      for (j in 1:nrow(sub_grid)) {
        tic(glue("sub grid yaw {j}/{nrow(sub_grid)}"))
        bbox <- sub_grid[j,] %>% st_bbox()

        sub_df <- bbox_to_objects(bbox)

        if (j == 1) {
          result_sub_df <- sub_df
        } else {
          result_sub_df <- rbind(result_sub_df, sub_df)}
        print(glue("count: {nrow(sub_df)}"))
        toc()
        }
      df <- result_sub_df

    }

  if (exists("result_df")) {
    result_df <- rbind(result_df, df)
  } else {
    result_df <- df
  }

  munich_grid_sf[i,]$count <- nrow(df)

  toc()
}

# Extract Munich of Grid
munich_df <- result_df %>% filter(st_intersects(result_df, munich, sparse=F))

#delete duplicates
unique_values <- !duplicated(munich_df$id)
munich_df <- subset(munich_df, unique_values)

#safe data frame
st_write(munich_df, "data/munich_lights.gpkg")
