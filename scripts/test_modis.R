library(terra)

r <- rast("C:/Users/Nutzer/OneDrive/Dokumente/Projects/wildfire_analysis/data/MODIS_EE/MOD09A1.A2021169.h29v12.061.2021178064809.hdf")
r


s <- sds("C:/Users/Nutzer/OneDrive/Dokumente/Projects/wildfire_analysis/data/MODIS_EE/MOD09A1.A2021169.h29v12.061.2021178064809.hdf")
s

writeRaster(s, "C:/Users/Nutzer/OneDrive/Dokumente/Projects/wildfire_analysis/data/MODIS_EE/MOD09A1.A2021169.h29v12.061.2021178064809_sds.tif")
