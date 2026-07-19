#' @title Example weather gauge observed data.
#' @docType data
#' @description Daily rain and maximum temperature observations (1/1/2010 - 31/12/2010) from six weather gauge within Australia.
#' The data is used by a vignette to evaluate the extracted point data. All six gauges have been selected
#' to be approximately at the centre of a grid cell to minimise error introduced by interpolation of the
#' extracted grid data.
#' @format data.frame of 4 columns and 365*6 rows.
#' @source Bureau of Meteorology, Australia. See \url{https://www.bom.gov.au/climate/data/index.shtml}
#' @usage data("weather_gauge_obs")
"weather_gauge_obs"
