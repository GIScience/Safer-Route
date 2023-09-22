library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(mapboxapi)
library(processx)

load("data/data.RData")


u#l = "https://raw.githubusercontent.com/bjornharrtell/flatgeobuf/3.0.1/test/data/UScounties.fgb"

#tst = st_read(url)
#tst = tst[, -5] # tippecanoe has a problem with the NAME column (not being proper utf8)
roads$ma |> names()

roads$ma |> select(c(row_id, mean_safetyscore, brightness_zscore_rescale, weight)) |> st_write("data/ma_roads.gpkg", append=F)

roads$ma |> select(c(row_id, mean_safetyscore, brightness_zscore_rescale, weight)) |> st_write("data/ma_roads.geojson", append=F)

st_write(
  roads$ma |> select(c(row_id, mean_safetyscore, brightness_zscore_rescale, weight))
  , dsn = "data/ma_roads"
  , driver = "MVT"
  , dataset_options = c(
    "FORMAT=DIRECTORY"
    , "COMPRESS=NO"
    , "MAXZOOM=17", 
      "MINZOOM=12"
  )
  , delete_dsn = TRUE
)

command = sprintf(
  "tippecanoe -zg -f -pC -e %s %s"
  , paste0(getwd(),"/data/ma_roads")
  , paste0(getwd(),"/data/ma_roads.geojson")
)
system.time({
  system(command)
})

system.time({
  processx::run(
    "tippecanoe"
    , args = c("-z 14", "-f", "-pC", "-e",  paste0(getwd(),"/data/ma_roads"), paste0(getwd(),"/data/ma_roads.geojson"))
  )
})

p = processx::process$new(
  "tippecanoe"
  , args = c("-z 10", "-f", "-pC", "-e",  paste0(getwd(),"/data/ma_roads"), paste0(getwd(),"/data/ma_roads.geojson"))
)
p$is_alive()



leafgrid = htmlDependency(
  name = "Leaflet.VectorGrid"
  , version = "1.3.0"
  , src = c(href = "https://unpkg.com/leaflet.vectorgrid@latest/dist/")
  , script = "Leaflet.VectorGrid.bundled.js"
)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

tiledDataDependency <- function(tiles_dir) {
  htmltools::htmlDependency(
    name = basename(tiles_dir),
    version = "0.0.1",
    src = c(file = tiles_dir)
  )
}


leaflet() %>%
  addTiles() %>%
  registerPlugin(leafgrid) %>%
  registerPlugin(tiledDataDependency(paste0(getwd(),"/data/ma_roads"))) %>%
  setView(lng = -105.644, lat = 51.618, zoom = 3) %>%
  onRender(
    paste0(
      "function(el, x) {
                var pbfLayer = L.vectorGrid.protobuf('lib/"
      , basename(paste0(getwd(),"/data/ma_roads"))
      , "-0.0.1/{z}/{x}/{y}.pbf');
                pbfLayer.addTo(this);
            }"
    )
  ) %>%
  leafem::addMouseCoordinates()


system("tippecanoe -zg \
           -o ma_roads.mbtiles \
           --coalesce-densest-as-needed \
           --extend-zooms-if-still-dropping \
           data/ma_roads.geojson")

library(mvtview)
library(rdeck)

# Fire up the server
serve_mvt("ma_roads.mbtiles", port = 8765)


leaflet() %>%
  addTiles() %>%
  registerPlugin(leafgrid) %>%
  registerPlugin(tiledDataDependency(paste0(getwd(),"/data/ma_roads"))) %>%
  setView(lng = -105.644, lat = 51.618, zoom = 3) %>%
  onRender(
    
    paste0(
      "function(el, x) {
                var pbfLayer = L.vectorGrid.protobuf('http://0.0.0.0:8765/ma_roads/{z}/{x}/{y}.vector.pbf');
                pbfLayer.addTo(this);
            }"
    )
  ) %>%
  leafem::addMouseCoordinates()

library(jsonlite)
tile_json <- fromJSON("http://0.0.0.0:8765/ma_roads.json")


rdeck(
  initial_bounds = structure(tile_json$bounds, crs = 4326, class = "bbox") # using the tilejson
) |>
  add_mvt_layer(
    data = "http://0.0.0.0:8765/ma_roads/{z}/{x}/{y}.vector.pbf",
    get_fill_color = scale_color_linear(
      weigth,
      limits = c(1,7) # without the tilejson metadata we have no defaults available for the attribute range
    ),
    opacity = 0.6
  )
