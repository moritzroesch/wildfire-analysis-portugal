## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: MODIS_wildfire_analysis.R
##
## Purpose of script: Burnt area analysis of fire seasons 2017-2021 in Protugal
##
## Sections:  Load data stacks
##              - loading of pre-processed MODIS data stacks
##
##            Normalized Burn Ratio (NBR) calculation
##              - calculate NBR for yearly MODIS data
##
## Author: Moritz Rösch
##
## Date: 2022-03-15
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##



# Packages ----------------------------------------------------------------

library(terra)
library(tidyverse)



# Load data stacks --------------------------------------------------------

years <- as.character(c(2017:2021))
data <- list()
for (i in years){
  path_to_stack <- list.files(str_c("data/", i),
                              pattern = "MOD09A1_\\d{4}\\.\\d{2}\\.\\d{2}_stack.tif",
                              full.names = TRUE)
  data[str_c("MOD09A1_", i)] <- rast(path_to_stack) # load and store MOD09A1 stack in list
}
data



# NBR calculation ---------------------------------------------------------




