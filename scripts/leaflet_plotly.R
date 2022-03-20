# Leaflet
library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)

wildfire <- st_read("data/burned_area_2017_2021.gpkg")
wildfire <- st_transform(wildfire, 4326)
wildfire$year <- as.numeric(wildfire$year)
prt <- st_read("data/prt.gpkg")
prt <- st_transform(prt, 4326)

wildfire <- wildfire %>% 
  filter(year >= "2020")


bins <- c(min(wildfire$year):max(wildfire$year))
pal <- colorFactor("RdYlBu", domain = as.factor(wildfire$year))


m <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = -7.95, lat = 39.83, zoom = 6) %>% 
  addPolygons(data = wildfire,
              color = ~pal(as.factor(year)),
              opacity = 1,
              fillColor = ~pal(as.factor(year)),
              fillOpacity = 1) %>% 
  addPolygons(data = prt,
              color = "black",
              opacity = 1,
              fillOpacity = 0,
              weight = 1,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              popup = ~htmlEscape(NAME_1)) %>% 
  addLegend(data = wildfire,
            position = "bottomright",
            pal = pal,
            values = ~as.factor(year),
            title = "Fire season")
m  


# plotly
library(sf)
library(tidyverse)
library(plotly)

wildfire <- st_read("data/burned_area_2017_2021.gpkg")
wildfire <- st_transform(wildfire, 4326)
wildfire$year <- as.numeric(wildfire$year)
prt <- st_read("data/prt.gpkg")
prt <- st_transform(prt, 4326)

fig <- plot_ly(
  x = ~wildfire$date,
  y = ~wildfire$area_ha,
  type = "bar",
  color = I("red"),
  hovertemplate = paste('<b>Date</b>: %{x}',
                        '<br><b>Burned area</b>: %{y:.2f} ha</br>'))
fig <- fig %>% 
  layout(title = str_c("Burned area in (ha)"),
         yaxis = list(title = "Burned Area (in ha)"),
         xaxis = list(title = "Date"))
fig
