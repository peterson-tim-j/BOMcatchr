#' Source data URLs and attributes.
#'
#' \code{get.variableSource} get available variables, units and URLs to BoM gridded data.
#'
#' This function returns a list of available variables, unit, time step and URLs used to download the meteorological data.
#'
#' @return
#' data.frame of the source data location and properties required by the package:
#' \itemize{
#'  \item{\code{label}: string description of the variable,}
#'  \item{\code{units}: string for units of the variable.}
#'  \item{\code{time.step} : string for the time step of the data. \code{days} or \code{months} are accepted.}
#'  \item{\code{data.URL} : string of URL to the source gridded data.}
#'  \item{\code{data.file.extension} : string for the file extension of the downloaded compressed source data.}
#'  \item{\code{data.file.format} : string for file extension to the file required within the downloaded file.}
#'  \item{\code{ncdf.name} : string for the name of the variable once input to the package netCDF file.}
#'  \item{\code{ellipsoid.crs} : string for Coordinate Reference System (CRS) for the gridded data ellipsoid.}
#' }
#'
#' @examples
#' vars = get.variableSource()
#'
#' @export
get.variableSource <- function() {

  vars = c('tmax', 'tmin', 'precip', 'precip.RMSE', 'precip.monthly', 'vprp', 'solarrad')
  nvars = length(vars)
  var.data = data.frame(label = rep('', nvars),
                        units  = rep('', nvars),
                        time.step = rep('', nvars),
                        data.URL = rep('', nvars),
                        data.file.extension = rep('', nvars),
                        data.file.format = rep('', nvars),
                        ncdf.name = rep('', nvars),
                        ellipsoid.crs = rep('', nvars),
                        row.names = vars)

  var.data['tmax',] <- c(  'Max daily temperature',
                           'deg_C',
                           'days',
                           'https://www.bom.gov.au/web03/ncc/www/awap/temperature/maxave/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'tmax',
                           '+proj=longlat +ellps=GRS80')

  var.data['tmin',] <- c(  'Min daily temperature',
                           'deg_C',
                           'days',
                           'https://www.bom.gov.au/web03/ncc/www/awap/temperature/minave/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'tmin',
                           '+proj=longlat +ellps=GRS80')

  var.data['precip',] <- c('Total daily precipitation',
                           'mm/day',
                           'days',
                           'https://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'precip',
                           '+proj=longlat +ellps=GRS80')

  var.data['precip.RMSE',] <- c('Root mean square error of daily precipitation estimate',
                           'mm/day',
                           'days',
                           'https://www.bom.gov.au/web03/ncc/www/awap/rainfall/rmse/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'precip.rmse',
                           '+proj=longlat +ellps=GRS80')

  var.data['precip.monthly',] <- c('Total monthly precipitation',
                           'mm/month',
                           'months',
                           'https://www.bom.gov.au/web03/ncc/www/agcd/rainfall/totals/month/grid/0.05/history/nat/',
                           'grid.zip',
                           'txt',
                           'precip.monthly',
                           '+proj=longlat +ellps=GRS80')

  var.data['vprp',] <- c(  '3pm daily vapour pressure',
                           'hpa',
                           'days',
                           'https://www.bom.gov.au/web03/ncc/www/awap/vprp/vprph15/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'vprp',
                           '+proj=longlat +ellps=GRS80')

  var.data['solarrad',] <- c(  'Total daily solar radiation',
                           'MJ/m^2',
                           'days',
                           'https://www.bom.gov.au/web03/ncc/www/awap/solar/solarave/daily/grid/0.05/history/nat/',
                           'grid.Z',
                           'grid',
                           'solarrad',
                           '+proj=longlat +ellps=GRS80')

  return(var.data)

}

