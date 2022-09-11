rm(list=ls())

## get the list from the FloodAPI
download.file("http://environment.data.gov.uk/flood-monitoring/id/stations?_limit=10000&_view=full","flood_stations_full.json")

## get the list from the Hydrology api
download.file("https://environment.data.gov.uk/hydrology/id/measures.csv","hydrology_measures.csv")

