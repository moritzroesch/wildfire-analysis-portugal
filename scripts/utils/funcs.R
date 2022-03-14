
# earthdata_credentials ---------------------------------------------------
# Query user credentials for a NASA Earthdata account.

earthdata_credentials <- function(){
  credentials <- list()
  credentials["user"] <<- readline(prompt = "Enter NASA Earthdata username/e-mail: ")
  credentials["password"] <<- readline(prompt = "Enter NASA Earthdata password: ")
}

