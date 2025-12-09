#' Get default URLs for loading data.
#'
#' \code{getURLs} get URLS to AWAP and Australian 9s DEM.
#'
#' This function returns a list of default URLs used to download the meteorological data.
#'
#' @return
#' A list variable of URLs as characters.
#'
#' @examples
#'
#' URLs = getURLs()
#'
#' @export
getURLs <- function() {

  URLs = list(precip = 'https://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/',
              Tmin  =  'https://www.bom.gov.au/web03/ncc/www/awap/temperature/minave/daily/grid/0.05/history/nat/',
              Tmax =   'https://www.bom.gov.au/web03/ncc/www/awap/temperature/maxave/daily/grid/0.05/history/nat/',
              vprp =   'https://www.bom.gov.au/web03/ncc/www/awap/vprp/vprph15/daily/grid/0.05/history/nat/',
              solarrad  = 'https://www.bom.gov.au/web03/ncc/www/awap/solar/solarave/daily/grid/0.05/history/nat/')

  return(URLs)

}

