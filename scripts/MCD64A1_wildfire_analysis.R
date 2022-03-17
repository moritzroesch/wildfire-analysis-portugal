## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: MCD64A1_wildfire_analysis.R
##
## Purpose of script: Burnt area analysis of fire seasons 2017-2021 in Portugal
##
## Sections:  Load data stacks
##              - loading of pre-processed MODIS time series stacks
##
##            Raster time series to sf
##              - convert yearly MCD64A1 stacks to sf object
##              
##
##
## Author: Moritz Rösch
##
## Date: 2022-03-17
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##



# Packages ----------------------------------------------------------------

library(terra)
library(sf)
library(tidyverse)



# Load data stacks --------------------------------------------------------

years <- as.character(c(2017:2021))
data <- list()
for (i in years){
  path_to_stack <- list.files(str_c("data", i, sep = "/"),
                              pattern = "burnday_\\d{4}.tif",
                              full.names = TRUE)
  r <- rast(path_to_stack)
  names(r) <- c("June_06", "July_07", "August_08", "September_09") # rename bands to months
  data[str_c("burnday_", i)] <- r # store burnday stacks in data list for further analyzation
  rm(r)
}
data



# Raster time series to sf -------------------------------------------------

for (i in 1:length(data)){
  r <- data[[i]]
  burn_poly <- r %>% 
    as.polygons() %>% # polygonize each layer of stack
    st_as_sf() %>% # convert to sf object
    filter(!across(1, ~.==-2)) # removes rows with unburned (0), fill (-1) and water (-2) polygons in first column
  burn_poly
  
  
}

st_write(spatvec, "data/spatvec_undis.gpkg")
