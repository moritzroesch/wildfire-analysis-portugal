# Leaflet
library(sf)
library(tidyverse)
library(leaflet)

wildfire <- st_read("data/burned_area_2017_2021.gpkg")
wildfire <- st_transform(wildfire, 4326)

m <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = wildfire,
              color = "black",
              opacity = 0.5,
              fillColor = wildfire$year)
m  
  