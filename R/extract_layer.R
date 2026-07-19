#'
#' Extract netCDF layer of one variable at one date.
#'
#' @description
#' extract_layer extracts the AWAP climate data for one date and one variable. This is
#' low level function is unlikely to be of use to a user.
#'
#' @param ncdfFilename is a full file name (as string) to the netCDF file.
#' @param ncdf.cond connection to the netcdf file. Default is \code{NA}. If not provided the connection is established.
#' @param extract.date is a date string specifying the date for data extraction.
#' @param var is a character string one one variable to extract. The options are
#' \code{c('tmax', 'tmin', 'precip', 'precip.monthly', 'vprp', 'solarrad', 'et')}.
#' @param vars.summary netCDF summary data.frame returned by \code{grid_summary(ncdfFilename)}.
#' Default is \code{NA}, which results in \code{grid_summary(ncdfFilename)} being called internally.
#'
#' @return
#' \code{terra::vect} object.
#'
#' @seealso
#' \code{\link{grid_build}} for building the NetCDF files of daily climate data.
#'
#' @export
extract_layer <- function(
    ncdfFilename = NA,
    ncdf.cond = NA,
    extract.date = NA,
    var = NA,
    vars.summary = NA) {

  # Check date.
  if (!methods::is(extract.date,'Date')) {
    if (is.character(extract.date)) {
      dateExtract = as.Date(extract.date,"%Y-%m-%d")
    } else
      .pretty_stop('Input date must a character string or a Date class object.')
  }

  if (!is.data.frame(vars.summary)) {
    if (file.exists(ncdfFilename))
      vars.summary <- grid_summary(ncdfFilename)
    else
      .pretty_stop('When vars.summary is not input, then ncdfFilename must be input.')
  }

  if (!is.character(var))
    .pretty_stop('Input var must a character string.')

  if (length(var)>1)
    .pretty_stop('Input var must a single character string.')

  if (!(var %in% rownames(vars.summary)))
    .pretty_stop('Input var must a variable name already in the provided netCDf file.')

  # Get index to the required date.
  ind = .get_ncdf_date_index(vars.summary[var,]$time.datum, extract.date)

  # Get netCDF path to the variable.
  var.grid <- vars.summary[var,]$group

  # Open connection to netcdf file - if not provided.
  do.ncclose = F
  if (is.na(ncdf.cond) || !isa(ncdf.cond,'NetCDF')) {
    if (file.exists(ncdfFilename)) {
      ncdf.cond <- RNetCDF::open.nc(ncdfFilename)
      do.ncclose = T
    } else
      .pretty_stop('The input file string is not to an existing netCDF file.')
  }

  # Get connection to the required group.
  grp = RNetCDF::grp.inq.nc(ncdf.cond, grpname = var.grid)$self

  # Get CRS for map variable.
  grp.crs = RNetCDF::att.get.nc(grp,
                                variable = 'NC_GLOBAL',
                                attribute = "CRS")

  # Get dimension data for map
  x = RNetCDF::var.get.nc(grp, 'Long')
  y = RNetCDF::var.get.nc(grp, 'Lat')

  # Read in one netcDF layer.
  r = RNetCDF::var.get.nc(grp,
                          var,
                          start=c(1, 1, ind),
                          count = c(length(x), length(y), 1),
                          na.mode=1)

  # Get map extent and build terra obj.
  grp.ext = RNetCDF::att.get.nc(grp,
                                variable = 'NC_GLOBAL',
                                attribute = 'ext')
  grp.ext = terra::ext(grp.ext)

  # Close connetion
  if (do.ncclose)
    RNetCDF::close.nc(ncdf.cond)

  # Convert matric to rast and return.
  return( terra::rast(t(r), crs = grp.crs, ext = grp.ext))
}
