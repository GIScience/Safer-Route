# Setup

This doc guides you through the setup of the Safer Route application.

## Mapillary API

Get your API key here: [https://www.mapillary.com/developer/api-documentation?locale=de_DE](https://www.mapillary.com/developer/api-documentation?locale=de_DE)

put it into a `config/config.json` file:

```json
{
  "mapillary_api_key": "your key here"
}
```


## Network graph via openrouteservice


Run openrouteservice locally to export the pedestrian graph
```

wget http://download.geofabrik.de/europe/germany/baden-wuerttemberg/karlsruhe-regbez-latest.osm.pbf -N --directory-prefix=data/ -O geofabrik_karlsruhe-regbez-latest.osm.pbf

wget http://download.geofabrik.de/europe/germany/bayern/oberbayern-latest.osm.pbf -N --directory-prefix=data/ -O geofabrik_oberbayern_latest.osm.pbf

wget http://download.geofabrik.de/north-america/us/district-of-columbia-latest.osm.pbf -N --directory-prefix=data/ -O geofabrik_district-of-columbia-latest.osm.pbf

osmium extract -b 8.412699,49.409633,8.590426,49.591490 data/geofabrik_karlsruhe-regbez-latest.osm.pbf -o ma.pbf --overwrite     

osmium extract -b 11.35963,48.06082,11.72441,48.25068 data/geofabrik_oberbayern_latest.osm.pbf -o munich.pbf --overwrite     

osmium extract -b -77.12119,38.79080,-76.90884,38.99678 data/geofabrik_district-of-columbia-latest.osm.pbf -o ma.pbf --overwrite    

osmium merge osm_file_ma.pbf osm_file_munich.pbf osm_file_dc.pbf -o osm_file.pbf --overwrite

```

Fire up your local openrouteservice container

```
ORS_UID=${UID} ORS_GID=${GID} docker-compose -f ors-docker-compose.yml up
```

Run the script to export the graph for the cities

```
Rscript extract_network_grap.R
```

## Data acquisition and processing


The rest of the data acquisiont happens in a different script. 

Run the script `src/data_acquisition_processing.R`

A `data/` folder will be created which contains all raw and processed data. 
The processed data is ready to be consumed by the shiny app:

* munich.Rdata
* dc.Rdata
* ma.Rdata



## Deploy webapp via rocker

Create directories to be mounted by the docker container

**Folders**

```sh
mkdir -p shiny_logs shiny_apps/data
```

* `shiny_logs` store the logs of our shiny application

* `shiny_apps` holds the app.R as well as the data it is consuming



**Run the docker container**

The docker container is based of rocker/geospatial:ubuntugis. 
We install some libraries used in acquiring, processing and visualizing the data. (L#4)
And we install the shiny server. (L#10)

All of this happens in the [Dockerfile](Dockerfile).


```sh
RUID="$(id -u)" docker-compose up
``` 

Copy the shiny application and the data to the volume to be served.

Copy the App components
```sh
cp -r src/app shiny_apps/.
cp  src/app.R shiny_apps/.

```

Copy the .RData files

```
cp  data/*.RData shiny_apps/data/.
```

Your shiny_apps folder should look like this:

```sh
$ tree shiny_apps/
shiny_apps/
├── app
│   ├── server.R
│   └── ui.R
├── app.R
└── data
    ├── dc.RData
    ├── ma.RData
    └── munich.RData

2 directories, 6 files

```


The shiny server runs at port 3838. See the shiny application here:


http://localhost:3838



