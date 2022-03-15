## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: funcs.R
##
## Purpose of script: Outsourced functions for wildfire analysis project
##
## Funtions:  earthdata_credentials
##              - queries user credentials for NASA Earthdata account
##
##            NBR
##              - calculates Normalized Burn Ratio
##
## Author: Moritz Rösch
##
## Date: 2022-03-14
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##



# earthdata_credentials ---------------------------------------------------

earthdata_credentials <- function(){
  # Query user credentials for a NASA Earthdata account.
  
  credentials <<- list()
  credentials["user"] <<- readline(prompt = "Enter NASA Earthdata username/e-mail: ")
  credentials["password"] <<- readline(prompt = "Enter NASA Earthdata password: ")
}



# NBR ---------------------------------------------------------------------

NBR <- function(r, NIR, SWIR, write_ras = FALSE, output_filename = NULL){
  # Calculates Normalized Burn Ratio (NBR) of a raster stack based on defined
  # band numbers for NIR and SWIR bands.
  
  # r:                SpatRaster or RasterBrick,
  #                   Raster stack with at least two bands (e.g., NIR and SWIR).
  # NIR:              numeric,
  #                   NIR-band number.
  # SWIR:             numeric,
  #                   SWIR-band number.
  # write_raster:     bool, default = FALSE,
  #                   If TRUE, writeRaster function writes raster to directory of
  #                   output_filename.
  # output_filename:  string
  #                   Define path and filename to write output raster layer.
}

