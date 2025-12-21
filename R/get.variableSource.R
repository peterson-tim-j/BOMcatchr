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

  vars = c('precip', 'tmin', 'tmax', 'vprp', 'solarrad')
  nvars = length(vars)
  var.data = data.frame(label = rep('', nvars),
                        units  = rep('', nvars),
                        timestep = rep('', nvars),
                        data.URL = rep('', nvars),
                        ncdf.name = rep('', nvars),
                        infillGaps = rep(F, nvars),
                        row.names = vars)
  var.data['precip',] <- c('Total daily precipitation',
                           'mm/day',
                           'daily',
                           'https://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/',
                           'precip',
                           F)

  var.data['tmin',] <- c(  'Min daily temperature',
                           'deg_C',
                           'daily',
                           'https://www.bom.gov.au/web03/ncc/www/awap/temperature/minave/daily/grid/0.05/history/nat/',
                           'tmin',
                           F)

  var.data['tmax',] <- c(  'Max daily temperature',
                           'deg_C',
                           'daily',
                           'https://www.bom.gov.au/web03/ncc/www/awap/temperature/maxave/daily/grid/0.05/history/nat/',
                           'tmax',
                           F)

  var.data['vprp',] <- c(  '3pm daily vapour pressure',
                           'hpa',
                           'daily',
                           'https://www.bom.gov.au/web03/ncc/www/awap/vprp/vprph15/daily/grid/0.05/history/nat/',
                           'vprp',
                           F)

  var.data['solarrad',] <- c(  'Total daily solar radiation',
                           'MJ/m^2',
                           'daily',
                           'https://www.bom.gov.au/web03/ncc/www/awap/solar/solarave/daily/grid/0.05/history/nat/',
                           'solarrad',
                           T)

  return(var.data)

}

