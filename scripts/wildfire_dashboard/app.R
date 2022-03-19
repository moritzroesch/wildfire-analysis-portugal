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
library(plotly)
library(sf)
library(tidyverse)




# Load data ---------------------------------------------------------------

wildfire <- st_read("data/burned_area_2017_2021.gpkg")





# UI ----------------------------------------------------------------------

# Define UI for shiny application that contains interactive leaflet map and plotly graph

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = str_c("Wildfires and burned area in Portugal during fire seasons (",
                  min(as.numeric(wildfire$year)), "-",
                  max(as.numeric(wildfire$year)), ")")
  ),
  dashboardSidebar(
    column(12,
      fluidRow(
        sliderInput(inputId = "fire_season",
                    label = h3("Select fire season:"),
                    min = min(as.numeric(wildfire$year)),
                    max = max(as.numeric(wildfire$year)),
                    value = c(min(as.numeric(wildfire$year)), max(as.numeric(wildfire$year))),
                    sep="",
                    step = 1)
      ),
      fluidRow(
        selectInput(inputId = "group",
                    label = h3("Group by:"),
                    choices = list("Day" = "date", "Month" = "month", "Year" = "year"),
                    selected = "year")
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
  
  data_input <- reactive({
    wildfire %>% 
      filter(year >= input$fire_season[1]) %>% 
      filter(year <= input$fire_season[2]) #%>% # interactive filtering of wildfire season by date range
    #group_by(input$group) %>% 
    #summarize(area_ha = sum(area_ha))# summarize the area by the group argument
  })
  
  output$mymap <- renderLeaflet(
    leaflet() %>% 
      #setView() %>% 
      addTiles() %>% 
      addPolygons(data = wildfire,
                  color = "white",
                  fillOpacity = 0.8,
                  fillColor = data_input()$year)
  )
    
 
}

#x <- wildfire %>% 
#  filter(year >= 2017) %>% 
#  filter(year <= 2017) %>% # interactive filtering of wilfire season by date range
#  group_by(month) %>% 
#  summarize(area_ha = sum(area_ha))
#View(head(x))

# Run the app ----
shinyApp(ui = ui, server = server)

