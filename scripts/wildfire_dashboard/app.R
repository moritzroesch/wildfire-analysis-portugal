## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: app.R
##
## Purpose of script: Creates shiny app for interactive leaflet map of burned
##                    areas in Portugal during fire seasons 2017-2021
##
## Sections:  Load data 
##              - load created wildfire vector data
##
##            UI
##              - construction of dashboard ui
##
##            server
##              - populate ui with input of server
##
## Author: Moritz Rösch
##
## Date: 2022-03-19
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# FILE IS NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Upload maybe needed, shp need to be in dir of app.R

# Packages ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(plotly)
library(sf)
library(tidyverse)




# Load data ---------------------------------------------------------------

wildfire <- st_read("data/burned_area_2017_2021.gpkg")
wildfire <- st_transform(wildfire, 4326) # reproject to WGS84 for leaflet
wildfire$year <- as.numeric(wildfire$year)
prt <- st_read("data/prt.gpkg")
prt <- st_transform(prt, 4326)

# Define color palette for year map
pal <- colorFactor("RdYlBu", domain = as.factor(wildfire$year))


# UI ----------------------------------------------------------------------

# Define UI for shiny application that contains interactive leaflet map and plotly graph

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = str_c("Burned area after wildfires in Portugal during fire seasons ",
                  min(wildfire$year), "-",
                  max(wildfire$year)),
    titleWidth = 800
  ),
  dashboardSidebar(
    column(12,
      fluidRow(
        sliderInput(inputId = "fire_season",
                    label = h3("Select fire season:"),
                    min = min(wildfire$year),
                    max = max(wildfire$year),
                    value = c(min(wildfire$year), max(wildfire$year)),
                    sep="",
                    step = 1)
      ),
      fluidRow(
        pickerInput(inputId = "region",
                    label = h3("Select region:"),
                    choices = prt$NAME_1,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE,
                    selected = prt$NAME_1)
      )
    )
  ),
  dashboardBody(
    fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
    fluidRow(box(width = 12, plotlyOutput(outputId = "myplot")))
  )
)


# Define server logic ----
server <- function(input, output){
  
  # Reactive filtering of wildfires by date
  wildfire_input <- reactive({
    wildfire %>% 
      filter(year >= input$fire_season[1]) %>% 
      filter(year <= input$fire_season[2])
  })
  
  # Reactive selection of region
  region_input <- reactive({
    prt %>% 
      filter(NAME_1 %in% input$region)
  })
  
    
  output$mymap <- renderLeaflet({
    
    
    # Define color palette for year map
    pal <- colorFactor("RdYlBu", domain = as.factor(wildfire_input()$year))
    
    # Define boundary of selected region polygons
    bbox <- region_input() %>% 
      st_bbox() %>% 
      as.character()
    
    leaflet() %>% 
      addTiles() %>% 
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
      addPolygons(data = wildfire_input(),
                  color = ~pal(as.factor(year)),
                  opacity = 1,
                  fillColor = ~pal(as.factor(year)),
                  fillOpacity = 1) %>% 
      addPolygons(data = region_input(),
                  color = "black",
                  opacity = 1,
                  fillOpacity = 0,
                  weight = 1,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~htmlEscape(NAME_1)) %>% 
      addLegend(data = wildfire_input(),
                position = "bottomright",
                pal = pal,
                values = ~as.factor(year),
                title = "Fire season")
    })
  
  #ERROR Polygon by clicking 
  #click on polygon
  #observe({ 
  #  event <- input$map_shape_click
  #  print(event$id)
  #  
  #  updateSelectInput(session,
  #                    inputId = "region",
  #                    label = "region",
  #                    choices = prt$NAME_1,
  #                    selected = event$id)
  #})
  
  output$myplot <- renderPlotly({
    
    # region subset
    
    
    plot_ly(
      x = ~wildfire_input()$date,
      y = ~wildfire_input()$area_ha,
      type = "bar",
      color = I("red"),
      hovertemplate = paste('<b>Date</b>: %{x}',
                            '<br><b>Burned area</b>: %{y:.2f} ha</br>')) %>% 
      layout(title = str_c("Burned area in (ha)"),
             yaxis = list(title = "Burned Area (in ha)"),
             xaxis = list(title = "Date"))
    
  })
}



# Run the app ----
shinyApp(ui = ui, server = server)

