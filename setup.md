# Setup

This doc guides you through the setup of the Safer Route application.

## Mapillary API

Get your API key here:

put it into `config/config.json`:

```json
{
  "mapillary_api_key": "your key here"
}
```

## Data acquisition and processing

Run the script `src/data_acquisition_processing.R`

A `data/` folder will be created which contains all raw and processed data. 
The processed data is ready to be consumed by the shiny app:

* munich.Rdata
* dc.Rdata



## Deploy webapp via rocker

Create directories to be mounted by the docker container

**Folders**

```sh
mkdir -p shiny_logs shiny_apps/dc
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

Move the shiny application and the data to the volume to be served.

```sh
cp src/app.R shiny_apps/dc/app.R
```

```
cp data shiny_apps/dc
```

The shiny server runs at port 3838. See the shiny application here:


http://localhost:3838/dc_test/



