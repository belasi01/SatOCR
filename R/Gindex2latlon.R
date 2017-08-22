#' Convert Global index to lat/long
#'
#' @param Gindex is the global index from the 4.6km ISIN grid
#'
#' @return Returns a data frame with the lat and long in degree
#'
#' @author Simon Bélanger

Gindex2latlon <- function(Gindex){

  # Constant values
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

  # retreive the row and column index from Gindex
  FirstRowIndex=rep(NA,Nlat)
  FirstRowIndex[1]=1
  for (i in 2:Nlat) {
    FirstRowIndex[i]=FirstRowIndex[i-1]+Nlon[i-1]
  }
  x = matrix(FirstRowIndex, nrow=length(Gindex), ncol=length(FirstRowIndex), byrow = T) - Gindex
  x[x<0]<-NA
  idxrow <- apply(x,1,which.min) - 1
  idxcol <- Gindex - FirstRowIndex[idxrow]

  g.row <- idxrow
  g.col <- idxcol
  g.lat  <-  lat[g.row]
  Nlon_row <- Nlon[g.row]
  g.lon <- (360*(g.col-0.5)/Nlon_row) - 180

  return(data.frame(lat=g.lat, lon=g.lon))

}
