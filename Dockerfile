FROM rocker/geospatial:ubuntugis

# install additional libs
RUN install2.r tidygraph sfnetworks leaflet osmextract osmdata tictoc shinythemes

ENV S6_VERSION=v2.1.0.2
ENV SHINY_SERVER_VERSION=latest
ENV PANDOC_VERSION=default

RUN /rocker_scripts/install_shiny_server.sh

EXPOSE 3838

CMD ["/init"]
