## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: MODIS_download_preproc.R
##
## Purpose of script: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
##
## Sections:  XXXXX
##              - XXXXX
##
##            XXXX
##              - XXXXX
##
## Author: Moritz Rösch
##
## Date: 2022-03-14
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


# Packages ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(MODIStsp) # tools for downloading and preprocessing MODIS data
# install.packages("MODIStsp") for any problems with installation on LINUX systems
# refer to Section 2.2 of documentation:
# https://cran.r-project.org/web/packages/MODIStsp/vignettes/MODIStsp.html


# Get country vector data -------------------------------------------------

prt <- raster::getData("GADM", country = "PRT", level = 1) # get Portugal spatial data
prt <- prt %>% 
  st_as_sf() %>% 
  filter(!NAME_1 %in% c("Azores", "Madeira")) %>% # select only mainland portugal
  st_transform(32629) %>% # reproject to local UTM zone
  st_write("data/prt.gpkg", append = FALSE)

# MODIStsp GUI ------------------------------------------------------------

MODIStsp() # if needed, install all package depencies from error message

