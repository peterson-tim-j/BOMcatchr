#' Get a summary of an existing netCDF data file.
#'
#' \code{summary} sumarises the netCDF variables, units and date ranges.
#'
#' This function opens an existing netCDF file built using the package and
#' returns a data.frame of variables, unit, stand and end dates of the data.
#'
#' @return
#' data.frame of variables, units, time step an URLs to data.
#'
#' @examples
#' vars = summary()
#'
#' @export
summary <- function(ncfile) {

  if (!is.character(ncdfFilename))
    stop('ncdfFilename is invalid. It must be a character string for the file name.')

  if (!file.exists(ncdfFilename))
    stop('ncdfFilename does not exists. It must first be built before a summary can be given.')

  # open netcdf file
  ncout <- ncdf4::nc_open(ncdfFilename, write=F)

  # Get the list of existing variables.
  vars = names(ncout$var)
  vars.trim = sub(".*?/", "", vars)
  nvars = length(vars.trim)

  # Initialise outputs
  summary.df = data.frame(from = rep(as.Date('0000-01-01', '%Y-%m-%d'), nvars),
                              to = rep(as.Date('9999-12-31', '%Y-%m-%d'), nvars),
                              units = rep('',nvars)
                              )
  row.names(summary.df) = vars

  # Loop through each variable and get start and end dates, geometry
  for (ivar in vars) {

    summary.df[ivar,]$from = as.Date( ncdf4::ncatt_get(ncout, varid = ivar,
                             attname = 'Start_date')$value,
                            '%Y-%m-%d')
    summary.df[ivar,]$to = as.Date( ncdf4::ncatt_get(ncout, varid = ivar,
                             attname = 'End_date')$value,
                            '%Y-%m-%d')

    summary.df[ivar,]$units = ncdf4::ncatt_get(ncout, varid = ivar,
                             attname = 'units')$value
  }
  row.names(summary.df) = vars.trim

  # close netcdf file
  ncdf4::nc_close(ncout)

  return(summary.df)
}
