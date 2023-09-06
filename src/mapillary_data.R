
#packages ----------------
library(httr)
library (jsonlite)
library(sf)
library(mapview)
library(dplyr)

#Munich ----------------
##API Request ------------------
#send request to API
res <- VERB("GET", url = "https://graph.mapillary.com/map_features?access_token=MLY|9986775024730224|7cba4c716e4096343f968f454f672949&fields=id,object_value,geometry&bbox=11.3607770000000006,48.0616243999999995,11.7229098999999994,48.2481161999999983&object_values=object--street-light")

#convert to string
json_string<- content(res, 'text')


##Process Data -------------
#convert string to data.frame
df_raw <- fromJSON(json_string)
print(df)
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
mapview(df)


#rename columns
df <- df %>% 
    rename(id = data.id,
           object_value = data.object_value)


## Clip Data to City Extent -------
shp_munich <- st_read("data/Munich_boundary.gpkg")


df_munich <- df %>% filter(st_intersects(geometry, shp_munich, sparse=F))

mapview(df_munich)
