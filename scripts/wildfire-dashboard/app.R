## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: app.R
##
## Purpose of script: Creates shiny app for interactive leaflet map and 
##                    plotly bar plot of burned areas in Portugal during
##                    fire seasons 2017-2021
##
## Sections:  Load data 
##              - load created wildfire and portugal vector data
##
##            UI
##              - construction of dashboard ui
##              - definition of user input for reactive leaflet and plotly
##
##            server
##              - Reactive filtering of wildfires by year input
##              - Reactive selection of region input
##              - Creation of static Leaflet basemap
##              - adding of reactive processes to leaflet proxy map
##              - creation of reactive plotly bar plot based on xear and
##                region input
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
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(plotly)
library(sf)
library(tidyverse)



# Load data ---------------------------------------------------------------

# When running the app, working directory changes to wildfire-dashboard
wildfire <- st_read("data/burned_area_2017_2021_WGS84.gpkg")
prt <- st_read("data/prt_WGS84.gpkg")



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
                    value = c(min(wildfire$year), min(wildfire$year)),
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




# Server ------------------------------------------------------------------

# Define server logic

server <- function(input, output){
  
  # Reactive filtering of wildfires by date input
  wildfire_input <- reactive({
    wildfire %>% 
      filter(year >= input$fire_season[1]) %>% 
      filter(year <= input$fire_season[2])
  })
  
  
  # Reactive selection of region input
  region_input <- reactive({
    prt %>% 
      filter(NAME_1 %in% input$region)
  })
  
  
  # Create static leaflet basemap  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles()
    })
  
  
  # Add reactive part of leaflet (filterd by year or region) to leaflet proxy map
  observe({
      
    # Define boundary of selected region polygons
    bbox <- region_input() %>% 
      st_bbox() %>% 
      as.character()
    
    # Define color palette for year map
    pal <- colorFactor("RdYlBu", domain = as.factor(wildfire_input()$year))
  
    # User input defined leaflet map 
    leafletProxy("mymap") %>% 
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
                  popup = ~htmlEscape(NAME_1))
  })
  
  
   # Add new legend and remove old one 
  observe({
    
    # Define color palette for legend
    pal <- colorFactor("RdYlBu", domain = as.factor(wildfire_input()$year))
    
    leafletProxy("mymap") %>% 
      clearControls() %>% 
      addLegend(data = wildfire_input(),
                position = "bottomright",
                pal = pal,
                values = ~as.factor(year),
                title = "Fire season",
                opacity = 1)
  })

 
  # Generation of bar plot based on user input
  output$myplot <- renderPlotly({
    
    # Define color palette for bar color
    pal <- colorFactor("RdYlBu", domain = as.factor(wildfire_input()$year))
    
    # region subset
    sf_use_s2(FALSE) # switch off spherical geometry (s2) to solve intersection error
    wildfire_region <- st_intersection(wildfire_input(), region_input())
    
    plot_ly(
      x = ~wildfire_region$date,
      y = ~wildfire_region$area_ha,
      color = ~as.factor(wildfire_region$year),
      colors = pal(input$fire_season[1]:input$fire_season[2]),
      type = "bar",
      hovertemplate = paste('<b>Date</b>: %{x}',
                            '<br><b>Burned area</b>: %{y:.2f} ha</br>')) %>% 
      layout(title = str_c("Burned area (ha) in ",
                           if(identical(region_input()$NAME_1, prt$NAME_1)){
                             "Portugal"} else {region_input()$NAME1}),
             yaxis = list(title = "Burned Area (in ha)"),
             xaxis = list(title = "Date"))
    
  })
}



# Run the app ----
shinyApp(ui = ui, server = server)


