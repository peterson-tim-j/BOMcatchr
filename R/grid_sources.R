#' Source data URLs and attributes.
#'
#' \code{grid_sources} get available variables, units and URLs to BoM gridded data.
#'
#' This function returns a list of available variables, unit, time step and URLs used to download the meteorological data.
#'
#' @return
#' data.frame of the source data location and properties required by the package:
#' \itemize{
#'  \item{\code{label}: string description of the variable.}
#'  \item{\code{units}: string for units of the variable.}
#'  \item{\code{time.step} : string for the time step of the data. \code{days} or \code{months} are accepted.}
#'  \item{\code{date.start} : string for the first date of the source data.}
#'  \item{\code{data.URL} : string of URL to the source gridded data.}
#'  \item{\code{data.file.extension} : string for the file extension of the downloaded compressed source data.}
#'  \item{\code{data.file.format} : string for file extension to the file required within the downloaded file.}
#'  \item{\code{ncdf.name} : string for the name of the variable once input to the package netCDF file.}
#'  \item{\code{ellipsoid.crs} : string for Coordinate Reference System (CRS) for the gridded data ellipsoid.}
#'  \item{\code{update.days} : string for the update schedule of the source data from \url{https://www.bom.gov.au/climate/austmaps/update-schedule.shtml}.
#'  Each listed item in the string is the number of days after which the BoM update a data grid. That is, when the data for a given date is, say,
#'  7 days old then the BoM may update the data for that day. This field is used to identify netCDF time points requiring update to the latest data}.
#' }
#'
#' @examples
#' vars = grid_sources()
#'
#' @export
grid_sources <- function() {

  vars = c('tmax',
           'tmin',
           'precip',
           'precip.RMSE',
           'precip.monthly',
           'vprp_9am',
           'vprp_3pm',
           'solarrad')

  nvars = length(vars)
  var.data = data.frame(label = rep('', nvars),
                        units  = rep('', nvars),
                        time.step = rep('', nvars),
                        date.start = rep('', nvars),
                        data.URL = rep('', nvars),
                        data.file.extension = rep('', nvars),
                        data.file.format = rep('', nvars),
                        ncdf.name = rep('', nvars),
                        ellipsoid.crs = rep('', nvars),
                        update.days = rep('', nvars),
                        row.names = vars)

  var.data['tmax',] <- c(  'Max daily temperature',
                           'deg_C',
                           'days',
                           '1910-01-01',
                           'https://www.bom.gov.au/web03/ncc/www/awap/temperature/maxave/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'tmax',
                           'EPSG:4283',
                           '3, 7, 10, 90')

  var.data['tmin',] <- c(  'Min daily temperature',
                           'deg_C',
                           'days',
                           '1910-01-01',
                           'https://www.bom.gov.au/web03/ncc/www/awap/temperature/minave/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'tmin',
                           'EPSG:4283',
                           '3, 7, 10, 90')

  var.data['precip',] <- c('Total daily precipitation',
                           'mm/day',
                           'days',
                           '1900-01-01',
                           'https://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'precip',
                           'EPSG:4283',
                           '7, 10, 20, 30, 40, 180')

  var.data['precip.RMSE',] <- c('Root mean square error of daily precipitation estimate',
                           'mm/day',
                           'days',
                           '1900-01-01',
                           'https://www.bom.gov.au/web03/ncc/www/awap/rainfall/rmse/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'precip.rmse',
                           'EPSG:4283',
                           '7, 10, 20, 30, 40, 180')

  var.data['precip.monthly',] <- c('Total monthly precipitation',
                           'mm/month',
                           'months',
                           '1900-01-01',
                           'https://www.bom.gov.au/web03/ncc/www/agcd/rainfall/totals/month/grid/0.05/history/nat/',
                           'grid.zip',
                           'txt',
                           'precip.monthly',
                           'EPSG:4283',
                           '')

  var.data['vprp_9am',] <- c(  '9am daily vapour pressure',
                           'hpa',
                           'days',
                           '1971-01-01',
                           'https://www.bom.gov.au/web03/ncc/www/awap/vprp/vprph09/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'vprp_9am',
                           'EPSG:4283',
                           '90')

  var.data['vprp_3pm',] <- c(  '3pm daily vapour pressure',
                           'hpa',
                           'days',
                           '1971-01-01',
                           'https://www.bom.gov.au/web03/ncc/www/awap/vprp/vprph15/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'vprp_3pm',
                           'EPSG:4283',
                           '90')

  var.data['solarrad',] <- c(  'Total daily solar radiation',
                           'MJ/m^2',
                           'days',
                           '1990-01-01',
                           'https://www.bom.gov.au/web03/ncc/www/awap/solar/solarave/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'solarrad',
                           'EPSG:4283',
                           '')

  return(var.data)

}
