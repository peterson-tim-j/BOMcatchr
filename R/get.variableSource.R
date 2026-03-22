#' Get default variable data and source URLs.
#'
#' \code{get.variableSource} get available variables, units and URLs to BoM gridded data.
#'
#' This function returns a list of available variables, unit, time step and URLs used to download the meteorological data.
#'
#' @return
#' data.frame of variables, units, time step an URLs to data.
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

