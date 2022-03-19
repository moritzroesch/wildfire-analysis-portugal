# Leaflet
library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)

wildfire <- st_read("data/burned_area_2017_2021.gpkg")
wildfire <- st_transform(wildfire, 4326)
prt <- st_read("data/prt.gpkg")
prt <- st_transform(prt, 4326)


pal <- colorBin("Set1",
                domain = as.numeric(wildfire$year),
                bins = length(unique(wildfire$year)))

m <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = -7.95, lat = 39.83, zoom = 6) %>% 
  addPolygons(data = wildfire,
              color = ~pal(as.numeric(year)),
              opacity = 1,
              fillColor = ~pal(as.numeric(year)),
              fillOpacity = 1) %>% 
  addPolygons(data = prt,
              color = "black",
              opacity = 1,
              fillOpacity = 0,
              weight = 1,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              popup = ~htmlEscape(NAME_1))
m  
  
