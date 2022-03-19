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



# Packages ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
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
        selectInput(inputId = "region",
                    label = h3("Select region:"),
                    choices = prt$NAME_1)
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
  
  wildfire_input <- reactive({
    wildfire %>% 
      filter(year >= input$fire_season[1]) %>% 
      filter(year <= input$fire_season[2]) #%>% # interactive filtering of wildfire season by date range
    #group_by(input$group) %>% 
    #summarize(area_ha = sum(area_ha))# summarize the area by the group argument
  })
  
  # ERROR POLYGON SELECTION (maybe multiple)
  #region_input <- reactive({
  #  prt %>% 
  #    filter(NAME1 == input$region)
  #})
  
    
  output$mymap <- renderLeaflet(
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -7.95, lat = 39.83, zoom = 6) %>% 
      addPolygons(data = wildfire_input(),
                  color = ~pal(as.factor(year)),
                  opacity = 1,
                  fillColor = ~pal(as.factor(year)),
                  fillOpacity = 1) %>% 
      addPolygons(data = prt,
                  color = "black",
                  opacity = 1,
                  fillOpacity = 0,
                  weight = 1,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~htmlEscape(NAME_1)) %>% 
      #addPolygons(data = region_input(),
      #            color = "Red",
      #            opacity = 1,
      #            fillOpacity = 0,
      #            weight = 2,
      #            label = ~htmlEscape(NAME1)) %>% 
      addLegend(data = wildfire_input(),
                position = "bottomright",
                pal = pal,
                values = ~as.factor(year),
                title = "Fire season")
  )
  
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
}



# Run the app ----
shinyApp(ui = ui, server = server)

