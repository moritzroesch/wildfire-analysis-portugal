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



# MODIStsp download and processing ----------------------------------------

opts_file <- "auxiliary/MOD09A1_PRT_setup.json" # MODIStsp setup file with pre-defined processing parameters
acq_dates <- c("2021.10.08", "2020.10.07") # MODIS Terra acquisition dates

# The personal credentials are retrieved from a hidden file.
# Enter your NASA Earthdata personal credentials (format string) here to run the script:
user <- "moritz.roesch@stud-mail.uni-wuerzburg.de" # NASA Earthdata username
password <- "Ea2021Gle!" # NASA Earthdata password

for (i in 1:length(acq_dates)){
  acq_date <- acq_dates[i] # set acquisition date
  year <- str_sub(acq_date, 1, 4) # define year variable
  out_folder <- str_c("C:/Users/Nutzer/OneDrive/Dokumente/Projects/wildfire-analysis-portugal/wildfire-analysis-portugal/data/",
                      year) # define output folder
  
  # Launch download and processing
  MODIStsp(gui = FALSE,
           opts_file = opts_file,
           start_date = acq_date,
           end_date = acq_date,
           user = user,
           password = password,
           out_folder = out_folder,
           parallel = TRUE)
  print(str_c("MOD09A1 download and processing for ", acq_date, " done."))
}






