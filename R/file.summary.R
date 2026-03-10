#' Get a summary of an existing netCDF data file.
#'
#' \code{file.summary} sumarises the netCDF variables, units and date ranges.
#'
#' This function opens an existing netCDF file built using the package and
#' returns a data.frame of variables, unit, stand and end dates of the data.
#'
#' @return
#' data.frame of variables, netcdf group for the variable
#' (i.e. the grid geometry group) , time step, start and end date
#' for the variable data .
#'
#' @examples
#' vars = file.summary()
#'
#' @export
file.summary <- function(ncfile) {

  if (!is.character(ncfile))
    stop('ncdfFilename is invalid. It must be a character string for the file name.')

  if (!file.exists(ncfile))
    stop('ncdfFilename does not exists. It must first be built before a summary can be given.')

  # open netcdf file
  ncout <- RNetCDF::open.nc(ncfile, write=F)

  # Get list of netCDF groups (one for each grid resolution)
  grps = RNetCDF::grp.inq.nc(ncout)
  ngrps = length(grps$grps)

  # Initialise outputs
  summary.df = data.frame(group = character(),
                          var.string = character(),
                          from = as.Date(character()),
                          to = as.Date(character()),
                          time.step = character(),
                          time.datum = character(),
                          units = character(),
                          ellipsoid.crs = character()
                          )

  # Get number of variables in all groups
  for (igrp in grps$grps) {
    nvars.tmp = RNetCDF::file.inq.nc(igrp)$nvars

    # Get group time starting point
    time.datum = RNetCDF::att.get.nc(igrp,'Time',"units")

    # Loop through each var of each group and get variable names
    # ONLY of those with THREE dimensions. ie not those thatare just
    # variables defining a dimension
    for (i in 0:(nvars.tmp-1)) {
      var.tmp = RNetCDF::var.inq.nc(igrp,i)

      if (var.tmp$ndims == 3) {
        date.from =  RNetCDF::att.get.nc(igrp, i, 'Start_date')
        date.to =  RNetCDF::att.get.nc(igrp, i, 'End_date')
        time.step =  RNetCDF::att.get.nc(igrp, i, 'time.step')
        units =  RNetCDF::att.get.nc(igrp, i, 'units')

        date.from = as.Date(date.from, '%Y-%m-%d')
        date.to = as.Date(date.to, '%Y-%m-%d')
        grp.string = RNetCDF::grp.inq.nc(igrp)$name
        var.string = paste(grp.string, '/', var.tmp$name, sep='' )

        ellipsoid.crs = RNetCDF::att.get.nc(igrp, 'NC_GLOBAL', 'CRS')

        summary.df.new =  data.frame( group = grp.string,
                                      var.string = var.string,
                                      from = date.from,
                                      to = date.to,
                                      time.step = time.step,
                                      time.datum = time.datum,
                                      units = units,
                                      ellipsoid.crs = ellipsoid.crs)

        row.names(summary.df.new) = var.tmp$name

        summary.df = rbind.data.frame(summary.df,
                                      summary.df.new)
      }

    }
  }

  # close netcdf file
  RNetCDF::close.nc(ncout)

  return(summary.df)
}
