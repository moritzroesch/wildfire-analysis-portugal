## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: MODIS_wildfire_analysis.R
##
## Purpose of script: Burnt area analysis of fire seasons 2017-2021 in Protugal
##
## Sections:  Load data stacks
##              - loading of pre-processed MODIS timeseries stacks
##
##
## Author: Moritz Rösch
##
## Date: 2022-03-16
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##



# Packages ----------------------------------------------------------------

library(terra)
library(tidyverse)



# Load data stacks --------------------------------------------------------

years <- as.character(c(2017:2021))
data <- list()
for (i in years){
  path_to_stack <- list.files(str_c("data", i, sep = "/"),
                              pattern = "burnday_\\d{4}.tif",
                              full.names = TRUE)
  r <- rast(path_to_stack)
  names(r) <- c("06_June", "07_July", "08_August", "09_September") # rename bands to months
  data[str_c("burnday_", i)] <- r # store burnday stacks in data list for further analyzation
  rm(r)
}
data
