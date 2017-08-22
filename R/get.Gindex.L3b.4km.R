#' Convert the row/column of a pixel into an index of the 4.6 km Global ISIN grid
#' (see Globcolour Product User's Documentation for more details on the ISIN grid)
#'
#'@param ixlat is the row number of the pixel
#'@param ixlon is the column number of the pixel
#'
#'@return The global index is return, which starts at 1 in the south pole and
#'finishes at 23761676 at the North pole. Note that the ISIN grid is not a regular matrix.
#'Each pixel of the grid have aproximately the same size (here 4.6 km x 4.6 km)
#'everywhere on the planet.
#'
#'@author Simon Bélanger
#'
get.Gindex.L4b.4km <- function(ixlat,ixlon){
  Re <- 6378.145
  Nlat <- 4320

  dr <- (pi*Re)/Nlat
  dPHI <- pi/Nlat

  PHI <- -(pi/2)+(1:Nlat)*dPHI-(dPHI/2)

  p <- 2*pi*Re*cos(PHI)
  Nlon <- round(p/dr, 0)


  FirstRowIndex=rep(NA,Nlat)
  FirstRowIndex[1]=1
  for (i in 2:Nlat) {
    FirstRowIndex[i]=FirstRowIndex[i-1]+Nlon[i-1]
  }

  First.ix.of.the.row = FirstRowIndex[ixlat]
  Gindex = First.ix.of.the.row + (ixlon-1)
  return(Gindex)
}
