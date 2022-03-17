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
months <- c("June", "July", "August", "September")
data <- list()
for (i in years){
  path_to_stack <- list.files(str_c("data", i, sep = "/"),
                              pattern = "burnday_\\d{4}.tif",
                              full.names = TRUE)
  r <- rast(path_to_stack)
  names(r) <- months # rename bands to months
  data[str_c("burnday_", i)] <- r # store burnday stacks in data list for further analyzation
  rm(r)
}
data



# Raster time series to sf -------------------------------------------------

poly_yearly_list <- list() # list for storing the yearly burned area polygons
# Polygonizing all monthly burned areas takes a couple minutes, feel free to grab a coffee ;)
for (i in 1:length(data)){
  
  # select time series stack and year variable
  r <- data[[i]]
  year <- years[i]
  
  # loop over every band (month) and create polygons from raster
  poly_monthly_list <- list()
  for (band in 1:nlyr(r)){
    month <- months[band] # extract month
    
    poly <- as.polygons(r[[band]], dissolve = FALSE) # polygonize each layer of stack
    # dissolve was set to FALSE since it produces false values in polygons (always takes values of first band (June) of SpatRaster, although only one band (except first run alsways not June) exist

    poly <- poly %>% 
      st_as_sf() %>% # convert to sf object
      filter(!across(1, ~.%in% c(-2,-1,0))) %>% # removes rows with unburned (0), fill (-1) and water (-2) polygons in first column
      rename(julian_day = 1) %>% # rename first column to julian day
      group_by(julian_day) %>%
      summarize() %>%  # summarize/dissolve by julian day
      mutate(month = month, year = year) %>%
      relocate(geometry, .after = last_col()) # relocate geometr column to end of dataframe
      
    poly_monthly_list[[band]] <- poly
    rm(poly)
    print(str_c("Generated sf object for", month, year, sep = " "))
  }
 
  poly_yearly_list[[year]] <- do.call(rbind, poly_monthly_list) # combine all monthly sf objects to one yearly sf object and store in list
  
  # combine all yearly polygons and write layer
  if (i == length(data)){
    burned <- do.call(rbind, poly_yearly_list)
    st_write(burned, str_c("data/burned_area_",
                           years[1], "_", years[length(years)], ".gpkg"), append = FALSE)
    # Clear duplicates in env
    rm(poly_monthly_list, poly_yearly_list)
  }
}