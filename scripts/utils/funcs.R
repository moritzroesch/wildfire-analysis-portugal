## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Script name: funcs.R
##
## Purpose of script: Outsourced functions for wildfire analysis project
##
## Functions:  earthdata_credentials
##              - queries user credentials for NASA Earthdata account
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
  if (credentials["user"] == "" & credentials["password"] == ""){
    stop("Username and password missing.")
  } else if (credentials["password"] == ""){
    stop("Password missing.")
  } else if (credentials["user"] == ""){
    stop("Username missing.")
  }
}
