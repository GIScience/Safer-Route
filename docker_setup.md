# Docker setup

Create directories to be mounted by the docker container

**Folders**

`mkdir -p shiny_logs shiny_apps/dc_test`

In `shiny_logs` logs of our shiny application is stored.

In `shiny_apps` our app.R as well as data is stored.



**Run the docker container**

The docker container is based of rocker/geospatial:ubuntugis. We install some libraries used in acquiring, processing and visualizing the data. 
And we install the shiny server.

All of this happens in the [Dockerfile](Dockerfile).


`RUID="$(id -u)" docker-compose up` 

Move the shiny application and the data to the volume to be served.

`cp src/rshiny_test.R shiny_apps/dc_test/app.R`

`cp data shiny_apps/.`

The shiny server runs at port 3838. See the shiny application here:

http://localhost:3838/dc_test/



