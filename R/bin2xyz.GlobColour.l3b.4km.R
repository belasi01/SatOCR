#'
#' Read L3b data from GlobColour project (4.6 km resolution) and return a text string in xyz format
#' suitable for GMT mapping tools
#'
#'@param filename is the file name string
#'@param prodname if the string of the product name you want to retreive (e.g. "CHL1_mean")
#'
#'@return Returns a text string in xyz format
#'
#'@author Simon Bélanger
bin2xyz.GlobColour.l3b.4km <- function(filename, prodname) {

  # Open the NetCDF file
  nc <- nc_open(filename)
  idxrow <- ncvar_get(nc, "row")+1
  idxcol <- ncvar_get(nc, "col")+1
  val <- ncvar_get(nc, prodname)
  nc_close(nc)

  # From http://menugget.blogspot.com/2012/04/working-with-globcolour-data.html
  Re <- 6378.145
  Nlat <- 4320

  dr <- (pi*Re)/Nlat
  dPHI <- pi/Nlat

  PHI <- -(pi/2)+(1:Nlat)*dPHI-(dPHI/2)

  p <- 2*pi*Re*cos(PHI)
  Nlon <- round(p/dr, 0)
  dlon <- p/Nlon
  dphi <- (2*pi)/Nlon
  Ntot <- sum(Nlon)
  lat <- PHI*180/pi

  g.row <- idxrow
  g.col <- idxcol
  g.lat  <-  lat[g.row]
  Nlon_row <- Nlon[g.row]
  g.lon <- (360*(g.col-0.5)/Nlon_row) - 180

  Nlon_row <- Nlon[g.row]
  g.width <- 360/Nlon_row
  g.height <- 180/Nlat

  return(paste(g.lon,g.lat,val))
}
