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
source("scripts/utils/funcs.R")



# Get country vector data -------------------------------------------------

prt <- raster::getData("GADM", country = "PRT", level = 1) # get Portugal spatial data
prt <- prt %>% 
  st_as_sf() %>% 
  filter(!NAME_1 %in% c("Azores", "Madeira")) %>% # select only mainland portugal
  st_transform(32629) %>% # reproject to local UTM zone
  st_write("data/prt.gpkg", append = FALSE)



# MODIStsp download and processing ----------------------------------------

# MODIStsp() allows to automatically download and pre-process any MODIS product
# available (see https://cran.r-project.org/web/packages/MODIStsp/vignettes/MODIStsp.html).
# With help of the MODIStsp GUI a .json file was created defining the product
# and processing parameters. This sections loops over them......



opts_file <- "auxiliary/MOD09A1_PRT_setup.json" # MODIStsp setup file with pre-defined processing parameters
acq_dates <- c("2021.10.08", "2020.10.07") # MODIS Terra acquisition dates
out_folder <- str_c(getwd(),"/data") # full path to output folder for processed MODIS products and raw data 

# Run function to enter your NASA Earthdata login credentials:
# No account? Register under: https://urs.earthdata.nasa.gov/
earthdata_credentials()

for (i in 1:length(acq_dates)){
  
  # Acquisition date selection
  acq_date <- acq_dates[i] # set acquisition date
  year <- str_sub(acq_date, 1, 4) # define year variable
  
  # Set paths and create directories
  out_folder_year <- str_c(out_folder, "/", year) # define output folder
  out_folder_year_raw <- str_c(out_folder, "/", year, "/raw") # define output folder for raw HDF files
  if (file.exists(out_folder_year) == FALSE){ # create new folders if not already done
    dir.create(out_folder_year)
    dir.create(out_folder_year_raw)
  }
  
  # Launch MODIStsp download and processing
  MODIStsp(gui = FALSE,
           opts_file = opts_file,
           start_date = acq_date,
           end_date = acq_date,
           user = credentials[["user"]],
           password = credentials[["password"]],
           out_folder = out_folder_year,
           out_folder_mod = out_folder_year_raw,
           parallel = TRUE)
  print(str_c("MOD09A1 download and processing for ", acq_date, " done."))
}