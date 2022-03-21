## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: app.R
##
## Purpose of script: Creates shiny app for interactive leaflet map and 
##                    plotly bar plot of burned areas in Portugal during
##                    fire seasons 2017-2021
##
## Sections:  Load and manipulate data 
##              - load created wildfire and portugal vector data
##              - create list for input selection of regions
##              - union portugal polygons
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

# Check for installed packages and install if required
list_of_packages = c("shiny", "shinydashboard", "shinyWidgets",
                     "leaflet", "htmltools", "plotly", "sf",
                     "lwgeom", "tidyverse")

lapply(list_of_packages, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x))

# load packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(plotly)
library(sf)
library(lwgeom)
library(tidyverse)



# Load and manipulate data -------------------------------------------------

# When running the app, working directory changes to wildfire-dashboard
wildfire <- st_read("Data/burned_area_2017_2021_WGS84.gpkg")
prt <- st_read("Data/prt_WGS84.gpkg")

# Construct named list of all subdivisions for user input
region_list <- c("Portugal", prt$NAME_1)

# Generate unified polygon of Portugal
prt_uni <- st_union(prt) %>% 
  st_sf() %>% 
  mutate(NAME_1 = "Portugal")

# define color palette function for leaflet and pltoly to access
pal <- colorFactor("Set1", domain = as.factor(wildfire$year))



# UI ----------------------------------------------------------------------

# Define UI for shiny application that contains interactive leaflet map and plotly graph

ui <- dashboardPage(
  skin = "red",
  # Header
  dashboardHeader(
    title = str_c("Burned areas after wildfires in Portugal during fire seasons ",
                  min(wildfire$year), "-",
                  max(wildfire$year)),
    titleWidth = 800
  ),
  # Define Sidebar input options
  dashboardSidebar(
    column(12,
      fluidRow(
        # Slider input with default selcted to 2017-2018 (for efficiency reasons when starting the app)
        sliderInput(inputId = "fire_season",
                    label = h3("Select fire season:"),
                    min = min(wildfire$year),
                    max = max(wildfire$year),
                    value = c(min(wildfire$year), min(wildfire$year) + 1),
                    sep="",
                    step = 1)
      ),
      fluidRow(
        # Picker input to select region, default is set to whole Portugal
        pickerInput(inputId = "region",
                    label = h3("Select region:"),
                    choices = region_list,
                    options = list(`actions-box` = TRUE))
      )
    )
  ),
  # Main app content with leaflet map and plotly bar plot
  dashboardBody(
    fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
    fluidRow(box(width = 12, plotlyOutput(outputId = "myplot")))
  )
)



# Server ------------------------------------------------------------------

# Define server logic

server <- function(input, output){
  
  # Reactive selection of region input
  region_input <- reactive({
    if (input$region == "Portugal"){
      prt_uni # Prtugal bounadry geometry
    } else {
      prt %>% 
        filter(NAME_1 == input$region) # single region geometry
    }
  })
  
  # Reactive filtering of wildfires by date input and clip to region_input
  wildfire_input <- reactive({
    sf_use_s2(FALSE) # turn s2 processing in sf package for intersection function
    if (input$region == "Portugal"){
      wildfire %>% 
        filter(year >= input$fire_season[1]) %>% # filter by selected dates
        filter(year <= input$fire_season[2]) %>% 
        mutate(area_ha = st_area(.) / 10000) # recalculate burned area in for clip
    } else {
      wildfire %>% 
        filter(year >= input$fire_season[1]) %>% # filter by selected dates
        filter(year <= input$fire_season[2]) %>% 
        st_intersection(region_input()) %>% # clip to selected region
        mutate(area_ha = st_area(.) / 10000) # recalculate burned area in for clip
    }
  })
  
  # Create sum of area for leaflet label
  burned <- reactive({
    round(sum(wildfire_input()$area_ha), 2)
  })
  
  # Create static leaflet basemap  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>% 
      addPolygons(data = prt,
                  color = "black",
                  opacity = 1,
                  fillOpacity = 0,
                  weight = 1,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~htmlEscape(NAME_1)) %>%
      addPolygons(data = region_input(),
                  color = "black",
                  opacity = 1,
                  fillOpacity = 0,
                  weight = 1,
                  highlightOptions = highlightOptions(color = "grey",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = HTML(str_c("<b>", region_input()$NAME_1, "</b>", # creates HTML label with summed burned area
                                     "<br><i>Total area burned: </i>", burned(), " ha</br>")),
                  layerId = "sel_region") %>%
      addLayersControl(baseGroups = c("OpenStreetMap", # control widget for switching betwenn basemaps
                                      "Esri WorldImagery",
                                      "Esri WorldTopoMap")) %>% 
      addMeasure(position = "topleft", # adds measurement tool
                 primaryLengthUnit = "meters",
                 primaryAreaUnit = "hectares")
  })
  
  
  # Add reactive part of leaflet (filtered by year or region) to leaflet proxy map
  observe({
      
    # Define boundary of selected region polygons
    bbox <- region_input() %>% 
      st_bbox() %>% 
      as.character()
  
    # User input defined (filtered by year and region) leaflet map of burned areas
    m_proxy <- leafletProxy("mymap") %>% 
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
    addPolygons(data = wildfire_input(),
                color = ~pal(as.factor(year)),
                opacity = 1,
                fillColor = ~pal(as.factor(year)),
                fillOpacity = 1)
    
    # Removal of previous polygon if new region is selected
    if (input$region != "Portugal"){
      m_proxy %>% 
        removeShape(layerId = "sel_region") %>% 
        addPolygons(data = region_input(),
                    color = "grey",
                    opacity = 1,
                    fillOpacity = 0,
                    weight = 3,
                    highlightOptions = highlightOptions(color = "grey",
                                                        weight = 4,
                                                        bringToFront = TRUE),
                    label = HTML(str_c("<b>", region_input()$NAME_1, "</b>",
                                       "<br><i>Total area burned: </i>", burned(), " ha</br>")),
                    layerId = "sel_region")
    }
  })
  
  
   # Updating the legend if new input is selected
  observe({
    
    leafletProxy("mymap") %>% 
      clearControls() %>% 
      addLegend(data = wildfire_input(),
                position = "bottomright",
                pal = pal,
                values = ~as.factor(year),
                title = "Fire season",
                opacity = 1)
  })

 
  # Generation of bar plot based on user input (filtered by year and region)
  output$myplot <- renderPlotly({
    
    # Create HEX colors for years
    plt_pal <- setNames(pal(unique(wildfire$year)),
                        nm = unique(wildfire$year)) # set year names to each color
    plt_pal_index_vec <- setNames(1:5, nm = unique(wildfire$year)) # create index vector for subsetting colors
    s_date <- plt_pal_index_vec[as.character(input$fire_season[1])] # define hex color for start date
    e_date <- plt_pal_index_vec[as.character(input$fire_season[2])] # define hex color for en date
    
    plot_ly(
      x = ~wildfire_input()$date,
      y = ~wildfire_input()$area_ha,
      color = ~as.factor(wildfire_input()$year),
      colors = plt_pal[s_date:e_date],
      type = "bar",
      hovertemplate = paste('<b>Date</b>: %{x}',
                            '<br><b>Burned area</b>: %{y:.2f} ha</br>')) %>% 
      layout(title = str_c("Burned area (ha) in ", input$region),
             yaxis = list(title = "Burned Area (in ha)"),
             xaxis = list(title = "Date"))
    
  })
}



# Run the app ----
shinyApp(ui = ui, server = server)


