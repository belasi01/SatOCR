# Code to manipulate L2 satellite OBPG data in R
library(raster)
#f="~/Google Drive/Projets_Recherche/PatyMatrai_NASA_NAtl/NASA NAtl-Arctic_group/Globcolour/xyz/20150712/V2012194114200.L2_SNPP_OC.nc"
#prodname="chlor_a"

L2dat2xyz.OBPG.L2.OC <- function(f, prodname) {
  prod <- raster(f, varname = paste("geophysical_data/", prodname,sep="")) 
  longitude <- raster(f, varname = "navigation_data/longitude") 
  latitude <- raster(f, varname = "navigation_data/latitude") 
  
  ## Select pixel with valid data
  ix = which(!is.na(values(prod)))
}


