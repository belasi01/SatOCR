#'
#' Create time series of each binned pixel found in a list of
#' L3b files
#'
#'@param latrange is the range of latitude to consider (Default is c(55,83))
#'@param fnames if a vector of L3b 4km file names in netcdf format coming from Globcolour data set
#'
#'@return Return a list containing the matrix (mat) of pixels time series (column = time),
#'        a vector of index of the Global ISIN grid (Gindex is a unique number starting at 1 in the south pole)
#'
#'
#'@author Simon Bélanger
#'

create.l3b.timeseries <- function(latrange=c(55,83), fnames) {

  # Need a vector of global index for the range of latitudes specified by the user
  Re <- 6378.145
  Nlat <- 4320

  dr <- (pi*Re)/Nlat
  dPHI <- pi/Nlat

  PHI <- -(pi/2)+(1:Nlat)*dPHI-(dPHI/2)

  p <- 2*pi*Re*cos(PHI)
  Nlon <- round(p/dr, 0)
  lat <- PHI*180/pi

  ix.row.min = which.min(abs(lat-latrange[1]))
  ix.row.max = which.min(abs(lat-latrange[2]))

  Gindex.m = get.Gindex.L4b.4km(ix.row.min, 1):get.Gindex.L4b.4km(ix.row.max, Nlon[ix.row.max])
  NpixMax = length(Gindex.m)

  # create a matrix of Npix and Nfile
  Nfiles = length(fnames)
  L3b.m = matrix(NA,nrow=NpixMax, ncol=Nfiles)

  for (i in 1: Nfiles) {
    tmp = read.GlobColour.l3b.4km(fnames[i], "CHL1_mean")

    # Keep pixels within the latitudes boundary
    ix = which(tmp$Gindex > Gindex.m[1] & tmp$Gindex < Gindex.m[NpixMax])

    L3b.m[tmp$Gindex[ix] - (Gindex.m[1]+1),i] = tmp$val[ix]
  }

  # remove unused lines
  s = apply(L3b.m, 1, sum, na.rm=T)
  good.row = which(s>0)

  return(list(mat=L3b.m[good.row,],Gindex=Gindex.m[good.row]))

}
