## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: MCD64A1_download_preproc.R
##
## Purpose of script: Downloading and preprocessing of MODIS/Terra+Aqua
#                     Burned Area Monthly L3 Global 500 m SIN Grid (MCD64A1)
##
## Sections:  Get country vector data
##              - download, filter and reproject vector data for Portugal
##
##            MODIStsp download and processing
##              - define pre-required variables for MODIStsp function
##              - query NASA earthdata credentials
##              - automated download and pre-processing of MCD64A1 product for
##                five years of fire season (2017-2021) in Portugal
##
##            MODIS data stacking
##              - pulling of data from MODIStsp folder structures
##              - creation of raster stacks
##
## Author: Moritz Rösch
##
## Date: 2022-03-16
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##



# Packages ----------------------------------------------------------------

library(sf)
library(terra)
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

# MODIStsp() allows the automatically download and pre-processing for any MODIS product
# available (see https://cran.r-project.org/web/packages/MODIStsp/vignettes/MODIStsp.html).
# With help of the MODIStsp GUI a .json file (opts_file) was created defining the product
# and processing parameters. Following processing steps are automatically applied
# to the selected MODIS product (for all details see "auxiliary/MOD09A1_PRT_setup.json"):
#   - Selection of MOD09A1 8-days surface reflection product (500m)
#   - subset to B1 (Red), B2 (NIR), B3 (Blue), B4 (Green), B6 (SWIR)
#   - mosaicing of Terra MODIS scenes to cover the whole AOI of Portugal
#   - Clip to bounding box of prt.gpkg
#   - reprojection to local WGS 84 / UTM zone 29N (EPSG:32629)

opts_file <- "auxiliary/MOD09A1_PRT_setup.json" # MODIStsp setup file with pre-defined processing parameters
acq_dates <- c("2017.10.08", "2018.10.08", "2019.10.08", "2020.10.07","2021.10.08")  # MODIS Terra acquisition dates
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



# MODIS data stacking -----------------------------------------------------

years <- as.character(c(2017:2021))
MODIS_stacks <- list()
for (i in years){
  
  # move into directory of year
  out_folder_bands <- str_c(out_folder, "/", i, "/Surf_Ref_8Days_500m_v6")
  bands_list <- list()
  for (j in list.files(out_folder_bands)){
    path_to_band <- str_c(out_folder_bands, "/", j)
    r <- rast(list.files(path_to_band, full.names = TRUE)) # load single MODIS band
    bands_list[j] <- r
  }
  r_stack <- rast(bands_list) #stack all MODIS bands of one scene
  MODIS_stacks[i] <- r_stack
  writeRaster(r_stack, str_c(out_folder, "/", i, "/MOD09A1_", 
                             str_subset(acq_dates, pattern = i),
                             "_stack.tif"),
              overwrite = TRUE)
}
