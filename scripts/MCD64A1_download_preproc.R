## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: MCD64A1_download_preproc.R
##
## Purpose of script: Downloading and pre-processing of MODIS/Terra+Aqua
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
##            Read/write timeseries stacks
##              - read created time series stacks
##              - write them as .tif files for further analysis
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
# to the selected MCD64A1 product (for all details see "auxiliary/MCD64A1_PRT_setup.json"):
#   - select MODIS/Terra+Aqua Burned Area Monthly L3 Global 500 m SIN Grid (MCD64A1)
#   - subset to monthly burned_day band for Portugal fire season (June to September)
#   - mosaic of MODIS scenes to cover the whole AOI of Portugal
#   - Clip to bounding box of prt.gpkg
#   - reprojection to local WGS 84 / UTM zone 29N (EPSG:32629)

opts_file <- "auxiliary/MCD64A1_PRT_setup.json" # MODIStsp setup file with pre-defined processing parameters
out_folder <- "data" # full path to output folder for processed MODIS products and raw data 
years <- as.character(c(2017:2021))
fire_season <- c("06.01", "09.30") # set Portugal fire season start and end date in format MM.DD
vector_path <- "data/prt.gpkg" # path to vectorlayer of Portugal

# Run function to enter your NASA Earthdata login credentials:
# No account? Register under: https://urs.earthdata.nasa.gov/
earthdata_credentials()


# Data is stored in directories for each year. Within these, a folder named after
# the selected processing vector layer (i.e. prt) contains all bands/monthly burn-
# day layers and a created time series RasterStack of all bands.

for (i in 1:length(years)){
  
  # Construct yearly fire season start and end date
  start_date <- str_c(years[i], fire_season[1], sep = ".")
  end_date <- str_c(years[i], fire_season[2], sep = ".")

  
  # Set paths and create directories
  out_folder_year <- str_c(out_folder, years[i], sep = "/") # define output folder
  out_folder_year_raw <- str_c(out_folder, years[i], "raw", sep = "/") # define output folder for raw HDF files
  if (file.exists(out_folder_year) == FALSE){ # create new folders if not already done
    dir.create(out_folder_year)
    dir.create(out_folder_year_raw)
  }
  
  # Launch MODIStsp download and processing
  MODIStsp(gui = FALSE,
           opts_file = opts_file,
           start_date = start_date,
           end_date = end_date,
           user = credentials[["user"]], # retrieve inserted user credentials
           password = credentials[["password"]],
           spatmeth = "file", # select processing extent defined by vector file
           spafile = vector_path, # path to vectordata (Portugal)
           out_folder = out_folder_year,
           out_folder_mod = out_folder_year_raw,
           parallel = TRUE)
  print(str_c("MCD64A1 download and processing for", years[i], "done.", sep = " "))
}



# Read/write timeseries stacks --------------------------------------------

data_list <- list()
for (i in years){
  
  # move into directory of timeseries stack
  out_folder_ts <- str_c(out_folder, i, "prt", "Burned_Monthly_500m_v6", 
                            "Time_Series", "RData", "Terra", "Burn_Date", sep = "/")
  
  # load .RData workspace with stored timeseries stack
  load(list.files(out_folder_ts, pattern = ".RData", full.names = TRUE)) # loads raster_ts file
  data_list[str_c("burnday_", i)] <- raster_ts
  # write out burnday timeseries stack as .tif
  writeRaster(raster_ts, str_c(out_folder, "/", i, "/burnday_", i, ".tif"), overwrite = TRUE)
  rm(raster_ts)
}