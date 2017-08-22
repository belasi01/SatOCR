#'
#' Read L3b data from GlobColour project (4.6 km resolution)
#'
#'@param filename is the file name string
#'@param prodname if the string of the product name you want to retreive (e.g. "CHL1_mean")
#'
#'@return Return a data frame with the global index and the values of the pixels available in the file
#'
#'@author Simon Bélanger

read.GlobColour.l3b.4km <- function(filename, prodname){
  # Open the NetCDF file
  nc <- nc_open(filename)
  idxrow <- ncvar_get(nc, "row")+1
  idxcol <- ncvar_get(nc, "col")+1
  val <- ncvar_get(nc, prodname)
  nc_close(nc)

  # retreive the Global index of each pixel
  Gindex = get.Gindex.L4b.4km(idxrow,idxcol)
  return(data.frame(Gindex,idxrow,idxcol,val))
}
