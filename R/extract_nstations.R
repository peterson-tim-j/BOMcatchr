#' Age of netCDF data and updates required.
#'
#' \code{extract_nstations} gets number of observation stations per variable per time step.
#'
#' This function gets the number of observation weather stations ed to produce each grid at each time step.
#' \code{\link{grid_sources}}),
#' @param ncfile file name of the netCDF data file built by the package function \code{\link{grid_build}}.
#' @param vars is a vector of variables names to extract.
#'
#' @return
#' data.frames, one for each variable in the netCDF file. Each data frame contains
#' the following columns:
#' \itemize{
#'  \item{\code{Date}: observation date,}
#'  \item{\code{n_stations}: number of stations to the grid for the timestep.}
#' }
#' @seealso \code{\link{grid_build}}
#' @export
extract_nstations <- function(ncfile,
                              vars = '') {

  if (!is.character(ncfile))
    .pretty_stop('ncdfFilename is invalid. It must be a character string for the file name.')

  if (!file.exists(ncfile))
    .pretty_stop('ncdfFilename does not exists. It must first be built before that number of stations can be extracted.')

  # Get summary of existing data
  data.existing = grid_summary(ncfile)
  if (vars=='') {
    vars = row.names(data.existing)
  } else {
    ind = !(vars %in% row.names(data.existing))
    if (any(ind))
      .pretty_stop(cat('The following vars are not in the build netCDF file:', vars[ind]))
  }

  # open netcdf file
  ncout <- RNetCDF::open.nc(ncfile, write=F)

  # Initialise output list
  summary.lst = list()

  for (ivar in vars) {

    # Build vector of data dates and get netCDF index to the dates
    data.timepoints = .get_ncdf_dates(data.existing[ivar,]$from,
                                     data.existing[ivar,]$to,
                                     data.existing[ivar,]$time.step)

    data.ind = .get_ncdf_date_index(data.existing[ivar,]$time.datum,
                                   data.timepoints,
                                   ncdf.start = data.existing[ivar,]$from,
                                   ncdf.end = data.existing[ivar,]$to)

    # Get date data was downloaded
    grp = RNetCDF::grp.inq.nc(ncout, data.existing[ivar,]$group)$self

    data.nstations = RNetCDF::var.get.nc(grp,
                                          paste0(ivar,'.numStations'),
                                          start=data.ind[1],
                                          count = length(data.ind))
    ind = data.nstations == -999
    data.nstations[ind] = NA

    # Build factor vector of source data dates
    summary.lst[[ivar]] = data.frame(Date = data.timepoints,
                                     n_stations = data.nstations)
  }

  # close netcdf file
  RNetCDF::close.nc(ncout)

  return(summary.lst)
}
