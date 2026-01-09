#'
#' \code{extractCatchmentData} extracts catchment average climate data from netCDF files containing Australian climate data.
#'
#' @description
#' extractCatchmentData extracts the AWAP climate data for each point or polygon. For the latter, either the daily spatial mean and variance (or user defined function) of
#' each climate metric is calculated or the spatial data is returned.
#'
#' @details
#' Daily data is extracted and can be aggregated to a weekly, monthly, quarterly, annual or a user-defined timestep using a user-defined function
#' (e.g. sum, mean, min, max as defined by \code{temporal.function.name}). The temporally aggregated data at each grid cell is then used to derive the spatial
#' mean or the spatial variance (or any other function as defined by \code{spatial.function.name}).
#'
#' The calculation of the spatial mean uses the fraction of each AWAP grid cell within the catchment polygon.
#' The variance calculation (or user defined function) does not use the fraction of the grid cell and returns NA if there are <2 grid cells in the catchment boundary.
#' Prior to the spatial aggregation, evapotranspiration (ET) can also calculated; after which, say, the mean and
#' variance PET can be calculated.
#'
#' The data extraction will by default be undertaken from 1/1/1900 to yesterday, even if the netCDF grids were only
#' built for a subset of this time period. If the latter situation applies, it is recommended that the extraction start
#' and end dates are input by the user.
#'
#' The ET can be calculated using one of eight methods at a user defined calculation time-step; that is the \code{ET.timestep} defines the
#' time step at which the estimates are derived and differs from the output timestep as defined by \code{temporal.function.name}). When \code{ET.timestep} is monthly or annual then
#' the ET estimate is linearly interpolated to a daily time step (using zoo:na.spline()) and then constrained to >=0. In calculating ET, the input data
#' is pre-processed using Evapotranspiration::ReadInputs() such that missing days, missing entries and abnormal values are interpolated
#' (by default) with the former two interpolated using the "DoY average", i.e. replacement with same day-of-the-year average. Additionally, when AWAP solar
#' radiation is required for the ET function, data is only available from 1/1/1990. To derive ET values <1990, the average solar radiation for each day of the year from
#' 1/1/990 to "extractTo" is derived (i.e. 365 values) and then applied to each day prior to 1990. Importantly, in this situation the estimates of ET <1990
#' are dependent upon the end date extracted. Re-running the estimation of ET with a later extractTo data will change the estimates of ET
#' prior to 1990.
#'
#' Some measures of ET require land surface elevation. Here, elevation at the centre of each 0.05 degree grid cell is obtained using the \code{elevatr} package, which here uses data from the
#' Amazon Web Service AWS Open Data Terrain Tiles. The data sources change with the user set \code{ET.DEM.res} zoom. The options are
#' 1 to 15. The default of 10 is reasonably computationally efficient and has a resolution of about 108 m, with is acceptable
#' given the 0.05 degree resolution of the BOM source data grids equates to about 5 km x 5 km.
#' For details see \url{https://github.com/tilezen/joerd/blob/master/docs/data-sources.md}
#'
#' Also, when "locations" is points (not polygons), then the netCDF grids are interpolate using bilinear interpolation of
#' the closest 4 grid cells.
#'
#' Lastly, data is extracted for all time points and no temporal infilling is undertaken if the grid cells are blank.
#'
#' @param ncdfFilename is a full file name (as string) to the netCDF file.
#' @param extractFrom is a date string specifying the start date for data extraction. The default is \code{"1900-1-1"}.
#' @param extractTo is a date string specifying the end date for the data extraction. The default is today's date as YYYY-MM-DD.
#' @param vars is a vector of variables names to extract. The available variables are: daily precipitation,
#' daily minimum temperature, daily maximum temperature, daily 3pm vapour pressure grids and daily solar radiation and evapotranspiration.
#' The input vector for these options are \code{c('precip', 'tmin', 'tmax', 'vprp', 'solarrad', 'et')}. Importantly, the input \code{et} is
#' calculated from the available gridded data (see \code{ET.} inputs below). To calculate the ET, all of the required inputs for the calculation
#' ET must also be extracted (i.e. the input for such would generally be \code{c('precip', 'tmin', 'tmax', 'vprp', 'solarrad', 'et')}.
#' Any or all of the defaults are available. The default \code{''} and this will result in all of the variables in the netCDF file and
#' provided by \code{rownames(AWAPer::file.summary(ncdfFilename))}.
#' @param locations is either the full file name to an ESRI shape file of points or polygons (latter assumed to be catchment boundaries) or a shape file
#' already imported using readShapeSpatial(). Either way the shape file must be in long/lat (i.e. not projected), use the ellipsoid GRS 80, and the first column must be a unique ID.
#' @param temporal.timestep character string for the time step of the output data. The options are \code{daily}, \code{weekly}, \code{monthly}, \code{quarterly},
#' \code{annual}  or a user-defined index for, say, water-years (see \code{xts::period.apply}). The default is \code{daily}.
#' @param temporal.function.name character string for the function name applied to aggregate the daily data to \code{temporal.timestep}.
#' Note, NA values are not removed from the aggregation calculation. If this is required then consider writing your own function. The default is \code{mean}.
#' @param spatial.function.name character string for the function name applied to estimate the daily spatial spread in each variable. If \code{NA} or \code{""} and \code{locations} is a polygon, then
#' the spatial data is returned. The default is \code{var}.
#' @param interpMethod character string defining the method for interpolating the gridded data (see \code{raster::extract}). The options are: \code{'simple'}, \code{'bilinear'} and \code{''}. The default
#' is \code{''}. This will set the interpolation to \code{'simple'} when \code{locations} is a polygon(s) and to \code{'bilinear'} when \code{locations} are points.
#' @param ET.function character string for the evapotranspiration function to be used. The methods that can be derived from the AWAP data are are \code{\link[Evapotranspiration]{ET.Abtew}},
#' \code{\link[Evapotranspiration]{ET.HargreavesSamani}}, \code{\link[Evapotranspiration]{ET.JensenHaise}}, \code{\link[Evapotranspiration]{ET.Makkink}}, \code{\link[Evapotranspiration]{ET.McGuinnessBordne}}, \code{\link[Evapotranspiration]{ET.MortonCRAE}} ,
#' \code{\link[Evapotranspiration]{ET.MortonCRWE}}, \code{\link[Evapotranspiration]{ET.Turc}}. Default is \code{\link[Evapotranspiration]{ET.MortonCRAE}} i.e. the  complementary relationship for areal evapotranspiration .
#' @param ET.DEM.res is the zoom resolution for the land surface elevation and is required to calculate the ET. \code{elevatr} package is used to extract elevation (metres) from
#' AWS Open Data Terrain Tiles. This input controls the zoom resolution. Higher values increase accuracy, but are significantly slower. See details. Default is 10.
#' @param ET.Mortons.est character string for the type of Morton's ET estimate. For \code{ET.MortonCRAE}, the options are \code{potential ET},\code{wet areal ET} or \code{actual areal ET}.
#' For \code{ET.MortonCRWE}, the options are \code{potential ET} or \code{shallow lake ET}. The default is \code{wet areal ET}, which when \code{ET.function = 'ET.MortonCRAE'} it provides
#' an estimate of the wet areal potential evapotranspiration.
#' @param ET.Turc.humid logical variable for the Turc function using the humid adjustment.See \code{\link[Evapotranspiration]{ET.Turc}}. For now this is fixed at \code{F}.
#' @param ET.timestep character string for the evapotranpiration time step. Options are \code{daily},  \code{monthly}, \code{annual} but the options are dependent upon the chosen \code{ET.function}. The default is \code{'monthly'}.
#' @param ET.missing_method character string for interpolation method for missing variables required for ET calculation. The options are \code{'monthly average'}, \code{'seasonal average'}, \code{'DoY average'} and \code{'neighbouring average'}. Default is \code{'DoY average'} but when the extraction duration is less than two years, the default is \code{'neighbouring average'}. See \code{\link[Evapotranspiration]{ReadInputs}}
#' @param ET.abnormal_method character string for interpolation method for abnormal variables required for ET calculation (e.g. Tmin > Tmax). Options and defaults are as for \code{ET.missing_method}. See \code{\link[Evapotranspiration]{ReadInputs}}
#' @param ET.constants list of constants from Evapotranspiration package required for ET calculations. To get the data use the command \code{data(constants)}. Default is \code{list()}.
#'
#' @return
#' When \code{locations} are polygons and \code{spatial.function.name} is not \code{NA} or \code{""}, then the returned variable is a list variable containing two data.frames. The first is the areal aggregated climate
#' metrics named \code{catchmentTemporal.} with a suffix as defined by \code{temporal.function.name}). The second is the measure of spatial variability
#' named \code{catchmentSpatial.} with a suffix as defined by \code{spatial.function.name}).
#'
#' When \code{locations} are polygons and \code{spatial.function.name} does equal \code{NA} or \code{""}, then the returned variable is a \code{sp::SpatialPixelsDataFrame} where the first column is the location/catchment IDs
#' and the latter columns are the results for each variable at each time point as defined by \code{temporal.timestep}.
#'
#' When \code{locations} are points, the returned variable is a data.frame containing daily climate data at each point.
#'
#' @seealso
#' \code{\link{makeNetCDF_file}} for building the NetCDF files of daily climate data.
#'
#' @examples
#' # The example shows how to extract and save data.
#' # For an additional example see \url{https://github.com/peterson-tim-j/AWAPer/blob/master/README.md}
#' #---------------------------------------
#' library(sp)
#'
#' # Set dates for building netCDFs and extracting data.
#' # Note, to reduce runtime this is done only a fortnight (14 days).
#' startDate = as.Date("2000-01-01","%Y-%m-%d")
#' endDate = as.Date("2000-01-14","%Y-%m-%d")
#'
#' # Set names for netCDF file.
#' ncdfFilename = tempfile(fileext='.nc')
#'
#' # Build netCDF grids and over a defined time period.
#' # Only precip data is to be added to the netCDF files.
#' # This is because the URLs for the other variables are set to zero.
#' \donttest{
#' file.name = makeNetCDF_file(ncdfFilename=ncdfFilename,
#'              updateFrom=startDate, updateTo=endDate,
#'              urlTmin=NA, urlTmax=NA, urlVprp=NA, urlSolarrad=NA)
#'
#' # Load example catchment boundaries and remove all but the first.
#' # Note, this is done only to speed up the example runtime.
#' data("catchments")
#' catchments = catchments[1,]
#'
#' # Extract daily precip. data (not Tmin, Tmax, VPD, ET).
#' # Note, the input "locations" can also be a file to a ESRI shape file.
#' climateData = extractCatchmentData(ncdfFilename=file.name,
#'               extractFrom=startDate, extractTo=endDate,
#'               vars = c('precip'),
#'               locations=catchments,
#'               temporal.timestep = 'daily')
#'
#' # Extract the daily catchment average data.
#' climateDataAvg = climateData$catchmentTemporal.mean
#'
#' # Extract the daily catchment variance data.
#' climateDataVar = climateData$catchmentSpatial.var
#'
#' # Remove temp. files
#' unlink(ncdfFilename)
#' unlink(ncdfSolarFilename)
#' }
#' @export
extractCatchmentData <- function(
    ncdfFilename=file.path(getwd(),'AWAP.nc'),
    extractFrom = as.Date("1900-01-01","%Y-%m-%d"),
    extractTo  = as.Date(Sys.Date(),"%Y-%m-%d"),
    vars = '',
    locations=NULL,
    temporal.timestep = 'daily',
    temporal.function.name = 'mean',
    spatial.function.name = 'var',
    interpMethod = '',
    ET.function = 'ET.MortonCRAE',
    ET.DEM.res = 10,
    ET.Mortons.est = 'wet areal ET',
    ET.Turc.humid=F,
    ET.timestep = 'monthly',
    ET.missing_method="DoY average",
    ET.abnormal_method='DoY average',
    ET.constants=list())  {

  # Get system time to estimate run time at the end.
  sys.start.time = Sys.time()

  # Check ncdfFilename file exist
  if (!file.exists(ncdfFilename))
    stop(paste("The following ncdfFilename input data file could not be found:",ncdfFilename))

  if (!is.character(vars))
    stop('vars must be a character vector of variables names.')

  # If vars is empty, then extract all variables in the file.
  if (length(vars)==1 && vars=='') {
    vars = rownames(AWAPer::file.summary(ncdfFilename))
  }
  # Check if vars include ET. If so remove and set getET=T
  getET = F
  if (any(vars %in% 'et')) {
    ind = which(vars %in% 'et')
    vars = vars[-ind]
    getET = T
  }

  # Check if the required variable is within the netcdf file.
  vars.prior.summary <- AWAPer::file.summary(ncdfFilename)
  vars.prior = row.names(vars.prior.summary)
  if (any(!(vars %in% vars.prior))) {
    stop('The netCDF file does not contain data for the requested variables. Update the netCDF grid and try again.')
  }
  nvars = length(vars)

  # Filter netCDF df summary to the variables to be extracted.
  filt = vars.prior %in% vars
  vars.extract.summary = vars.prior.summary[filt,]
  grids.extract = unique(vars.extract.summary$group)


  # netCDF.variables = names(ncout$var)
  # if (getTmin & !any(netCDF.variables=='nonsolar/tmin'))
  #   stop('getTmin is true but the netCDF file was not built with tmin data. Rebuild the netCDF file.')
  # if (getTmax & !any(netCDF.variables=='nonsolar/tmax'))
  #   stop('getTmax is true but the netCDF file was not built with tmax data. Rebuild the netCDF file.')
  # if (getVprp & !any(netCDF.variables=='nonsolar/vprp'))
  #   stop('getVprp is true but the netCDF file was not built with vprp data. Rebuild the netCDF file.')
  # if (getPrecip & !any(netCDF.variables=='nonsolar/precip'))
  #   stop('getPrecip is true but the netCDF file was not built with precip data. Rebuild the netCDF file.')

  # Build time points to update
  if (is.character(extractFrom))
    extractFrom = as.Date(extractFrom,'%Y-%m-%d');
  if (is.character(extractTo))
    extractTo = as.Date(extractTo,'%Y-%m-%d');
  if (extractFrom >= extractTo)
    stop('The extract dates are invalid. extractFrom must be prior to extractTo.')
  if (extractTo > as.Date(Sys.Date()-1,"%Y-%m-%d"))
    stop('The extractTo date must be prior to today.')
  timepoints2Extract = seq( as.Date(extractFrom,'%Y-%m-%d'), by="day", to=as.Date(extractTo,'%Y-%m-%d'))
  if (length(timepoints2Extract)==0)
    stop('The dates to extract produce a zero vector of dates of zero length. Check the inputs dates are as YYYY-MM-DD.')

  # Check time step
  temporal.timestep.options = c('daily','weekly','monthly','quarterly','annual', 'period')
  temporal.timestep.index = numeric()
  if (!is.character(temporal.timestep)) {
    if (!is.integer(temporal.timestep))
      stop('temporal.timestep must be a character string or an integer vector.')

    if (any(temporal.timestep<0 || temporal.timestep > length(timepoints2Extract)))
      stop(paste('temporal.timestep must be a character string or an integer vector with values >0 and <= the maximum days of data extracted (ie',
                 length(timepoints2Extract),')'))

    temporal.timestep.index = temporal.timestep
    temporal.timestep = 'period'

  } else if (!any(which(temporal.timestep.options == temporal.timestep))) {
    stop('temporal.timestep must be daily, weekly, monthly, quarterly or annual or an integer vector.')
  }

  # Check temporal analysis funcion is valid.
  data.junk = t(as.matrix(stats::runif(100, 0.0, 1.0)*100))
  result = tryCatch({
      apply(data.junk, 1,FUN=temporal.function.name)
    }, warning = function(w) {
      message(paste("Warning temporal.function.name produced the following",w))
    }, error = function(e) {
      stop(paste('temporal.function.name produced an error when applied using test data:',e))
    }, finally = {
      rm(data.junk)
    }
  )

  # Check ET inputs
  if (getET) {
    if (!exists('ET.constants'))
      stop('ET.constants must be input when getET = TRUE. Use the command: data(constants)')

    if (!is.list(ET.constants))
      stop('ET.constants must be a list variable when getET = TRUE. Use the command: data(constants)')

    if (length(ET.constants)==0)
      stop('ET.constants must be a list variable when getET = TRUE. Using the command: data(constants)')

    # Check the ET function is one of the acceptable forms
    ET.function.all =c('ET.Abtew', 'ET.HargreavesSamani', 'ET.JensenHaise','ET.Makkink', 'ET.McGuinnessBordne','ET.MortonCRAE' , 'ET.MortonCRWE','ET.Turc')
    if (!any(ET.function == ET.function.all)) {
      stop(paste('The ET.function must be one of the following:',ET.function.all))
    }

    # Check the ET function is one of the acceptable forms
    ET.timestep.all =c('daily', 'monthly', 'annual')
    if (!any(ET.timestep == ET.timestep.all)) {
      stop(paste('The ET.timestep must be one of the following:',ET.timestep.all))
    }

    # Check that extractFrom and extractTo span more than one ET timestep
    if (ET.timestep == 'monthly') {
      if (zoo::as.yearmon(timepoints2Extract[1]) == zoo::as.yearmon(timepoints2Extract[length(timepoints2Extract)]))
        stop('When the ET.timestep is monthly, the extraction dates must span more than one month.')
    }
    if (ET.timestep == 'annual') {
      if (format(timepoints2Extract[1],'%Y') == format(timepoints2Extract[length(timepoints2Extract)],'%Y'))
        stop('When the ET.timestep is annual, the extraction dates must span more than one year.')
    }

    # Check the appropriate time step is used.
    if ( (ET.function == 'ET.MortonCRAE' || ET.function == 'ET.MortonCRWE') && ET.timestep=='daily' ) {
      stop('The ET.timstep must be monthly or annual when using ET.MortonCRAE or ET.MortonCRWE')
    }

    # Build a data from of the inputs required for each ET function.
    ET.inputdata.req = data.frame(ET.function=ET.function.all, Tmin=rep(F,length(ET.function.all)), Tmax=rep(F,length(ET.function.all)), va=rep(F,length(ET.function.all)), Rs=rep(F,length(ET.function.all)), Precip=rep(F,length(ET.function.all)) )
    ET.inputdata.req[1,2:6] =  c(T,T,F,T,F)       #ET.Abtew
    ET.inputdata.req[2,2:6] =  c(T,T,F,F,F)       #ET.HargreavesSamani
    ET.inputdata.req[3,2:6] =  c(T,T,F,T,F)       #ET.JensenHaise
    ET.inputdata.req[4,2:6] =  c(T,T,F,T,F)       #ET.Makkink
    ET.inputdata.req[5,2:6] =  c(T,T,F,F,F)       #ET.McGuinnessBordne
    ET.inputdata.req[6,2:6] =  c(T,T,T,T,T)       #ET.MortonCRAE
    ET.inputdata.req[7,2:6] =  c(T,T,T,T,T)       #ET.MortonCRWE
    ET.inputdata.req[8,2:6] =  c(T,T,F,T,F)       #ET.Turc
    ind = which(ET.function == ET.function.all)
    ET.inputdata.filt = ET.inputdata.req[ind,]

    # Get list of required ET variable names
    ET.var.names = colnames(ET.inputdata.filt)[2:6]

    # If all required inputs are to be extracted for Mortions PET
    if (ET.inputdata.filt$Tmin[1] && !('tmin' %in% vars))
      stop('Calculation of ET for the given function requires extractions of tmin (i.e. set getTmin=T)')
    if (ET.inputdata.filt$Tmax[1] && !('tmax' %in% vars))
      stop('Calculation of ET for the given function requires extractions of tmax (i.e. set getTmax=T)')
    if (ET.inputdata.filt$va[1] && !('vprp' %in% vars))
      stop('Calculation of ET for the given function requires extractions of tmax (i.e. set getVprp=T)')
    if (ET.inputdata.filt$Precip[1] && !('precip' %in% vars))
      stop('Calculation of ET for the given function requires extractions of precip (i.e. set getPrecip=T)')
    if (ET.inputdata.filt$Rs[1] && !('solarrad' %in% vars))
        stop('Calculation of ET for the given function requires extractions of solar radiation (i.e. set getSolarrad=T)')
    if (!is.numeric(ET.DEM.res))
      stop('ET.DEM.res must be a numeric value between 1 anf 15.')

    if (ET.DEM.res < 1 || ET.DEM.res>15)
      stop('ET.DEM.res must be a numeric value between 1 anf 15.')
  }

  # Open file with polygons
  if (is.character(locations)) {
    if (!file.exists(locations))
      stop(paste('The following input file for the locations could not be found:',locations))

    # Read in polygons
    locations <- sf::st_read(locations)

    # Drop z vector if included
    locations = sf::st_zm(locations, drop=T)

    # Convert to sp spatial object
    locations = methods::as(locations,'Spatial')

  } else if (!methods::is(locations,"SpatialPolygonsDataFrame") && !methods::is(locations,"SpatialPointsDataFrame")) {
    stop('The input for "locations" must be a file name to a shape file or a SpatialPolygonsDataFrame or a SpatialPointsDataFrame object.')
  }

  # Check projection of locations
  if (is.na(sp::proj4string(locations))) {
    message('WARNING: The projection string of the locations is NA. Setting to +proj=longlat +ellps=GRS80.')
    sp::proj4string(locations) = '+proj=longlat +ellps=GRS80'
  }
  if (!grepl('+proj=longlat', sp::proj4string(locations)) || !grepl('+ellps=GRS80', sp::proj4string(locations))) {
    message('WARNING: The projection string of the locations does not appear to be +proj=longlat +ellps=GRS80. Attempting to transform coordinates...')
    locations = sp::spTransform(locations,sp::CRS('+proj=longlat +ellps=GRS80'))
  }

  # Check each catchment or point has a unique (non-NA) ID. Note.
  if (any(is.na(locations[[1]])))
    stop('The list of locations IDs (first column) contains NAs. Please remove these or rename')
  if ( length(unique(locations[[1]])) != length(locations[[1]]) )
    stop('The list of locations IDs (first column) is not unique. Please remove the duplicates')

  # Check if locations are points or a polygon.
  islocationsPolygon=TRUE;
  if (methods::is(locations,"SpatialPointsDataFrame")) {
    islocationsPolygon=FALSE;
  }

  # Check the interpolation method.
  if (interpMethod!='' && interpMethod!='simple' && interpMethod!='bilinear') {
    stop('The input for "interpMethod" must either "simple" or "bilinear".')
  }
  if (interpMethod=='') {
    if (islocationsPolygon) {
      interpMethod='simple';
    } else {
      interpMethod='bilinear';
    }
  }

  # Check if the spatial data should be returned or analysed.
  do.spatial.analysis=F
  if (islocationsPolygon) {
    do.spatial.analysis=T
    if (is.na(spatial.function.name) || (is.character(spatial.function.name) && spatial.function.name==''))
        do.spatial.analysis = F
  }

  if (do.spatial.analysis) {
    # Check spatial analysis funcion is valid.
    data.junk = t(as.matrix(stats::runif(100, 0.0, 1.0)*100))
    result = tryCatch({
      apply(data.junk, 1,FUN=spatial.function.name)
    }, warning = function(w) {
      message(paste("Warning spatial.function.name produced the following",w))
    }, error = function(e) {
      stop(paste('spatial.function.name produced an error when applied using test data:',e))
    }, finally = {
      rm(data.junk)
    }
    )
  }

  # Get netCDF geometry
  ncdf.dataFrom = max(vars.extract.summary$from)
  ncdf.dataTo = min(vars.extract.summary$to)
  timePoints = seq.Date(ncdf.dataFrom, ncdf.dataTo, by='day')

  # Build and provide summary of extraction dates and data extent.
  message('Extraction data summary:')
  # Write summary of Net CDF data.
  message(paste('    NetCDF climate data exists from', ncdf.dataFrom,'to', ncdf.dataTo));

  if (extractFrom < ncdf.dataFrom) {
    message('    WARNING: The extraction start date is prior to the existing data start date.');
    message('             Dates are adjusted accordingly.');
    message('             Consider extending the existing date range using makeNetCDF_file()');
  }
  if (extractTo > ncdf.dataTo) {
    message('    WARNING: The extraction end date is after to the existing data end date.');
    message('             Dates are adjusted accordingly.');
    message('             Consider extending the existing date range using makeNetCDF_file()');
  }

  # Limit the extraction time points to the data range
  extractFrom = max(c(extractFrom, ncdf.dataFrom))
  extractTo = min(c(extractTo, ncdf.dataTo))

  if (extractTo < extractFrom) {
    stop('extractTo date is less than the extractFrom date.')
  }

  message(paste('    Data will be extracted from ',format.Date(extractFrom,'%Y-%m-%d'),' to ', format.Date(extractTo,'%Y-%m-%d'),' at ',length(locations),' locations '));

  # Check ET interpolation methods are appropriate if duration is <2 years
  if (getET) {
    if ( (extractTo - extractFrom) < 2*365){
      if (ET.missing_method!='neighbouring average' || ET.abnormal_method!='neighbouring average' ) {
        message('    WARNING: The extraction duration is < 2 years and getET = TRUE.');
        message('             Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".');

        ET.missing_method = "neighbouring average"
        ET.abnormal_method = "neighbouring average"
      }
    }
  }

  message('Starting data extraction:')

  # Recalculate the time points to extract.
  timepoints2Extract = seq( as.Date(extractFrom,'%Y-%m-%d'), by="day", to=as.Date(extractTo,'%Y-%m-%d'))

  # Build a matrix of catchment weights, lat longs, and a look up table for each catchment.
  message('... Building catchment weights for each grid.')

  ngrids = length(grids.extract)
  w.grid = vector('list',ngrids)
  longLat.grid = vector('list',ngrids)
  location.lookup.grid = vector('list',ngrids)
  crs.grid = vector('list',ngrids)

  names(w.grid) = grids.extract
  names(longLat.grid) = grids.extract
  names(location.lookup.grid) = grids.extract

  # Set crs
  for (igrid in grids.extract) {
    ind = vars.extract.summary$group == igrid
    crs.grid[[igrid]] = vars.extract.summary[ind,]$ellipsoid.crs[1]
  }

  # Handle polygon to extract, then points.
  if (islocationsPolygon) {
    time.end = as.integer(difftime(max(timepoints2Extract), as.Date("1900-1-1",'%Y-%m-%d'),units = "days" ))
    for (igrid in grids.extract) {
      # initialise for grid geometry i
      longLat.grid[[igrid]] = matrix(NA,0,2)
      location.lookup.grid[[igrid]] = matrix(NA,length(locations),2)

      # Get one netCDF layer for current grid geometry.
      ind = vars.extract.summary$group == igrid
      var.group.string = vars.extract.summary[ind,]$var.string[1]
      grid.tmp = raster::raster(ncdfFilename,
                                band=time.end,
                                varname=var.group.string,
                                lvar=3,
                                crs=crs.grid[[igrid]])

      for (i in 1:length(locations)) {
          if (i%%10 ==0 ) {
            message(paste('   ... Building weights for catchment ', i,' of ',length(locations)));
            raster::removeTmpFiles(h=0)
          }

          # Extract the weights for grid cells within the locations polygon.
          # Note, the AWAP raster grid is cropped to the extent of the catchment polygon.
          # This was undertaken to improve the run time performance but more importantly to overcome an error
          # thrown by raster::rasterize when the raster is large (see https://github.com/rspatial/raster/issues/192).
          # This solution should work when the locations polygon is not very large (e.g. not a reasonable fraction of the
          # Australian land mass)
          w = raster::rasterize(x=locations[i,],
                                y=raster::crop(grid.tmp, locations[i,], snap='out'),
                                fun='last',
                                getCover=T)

          # Extract the mask values (i.e. fraction of each grid cell within the polygon.
          w2 = raster::getValues(w);
          filt = w2>0
          wLongLat = sp::coordinates(w)[filt,]
          w=w[filt]

          # Normalise the weights
          w = w/sum(w);

          # Add to data set of all locations
          if (length(w.grid)==0) {
            location.lookup.grid[i,] = c(1,length(w));
            w.grid[[igrid]] = w;
            longLat.grid[[igrid]] = wLongLat;
          } else {
            location.lookup.grid[[igrid]][i,] = c(length(w.grid[[igrid]]) + 1,
                                           length(w.grid[[igrid]]) + length(w));
            w.grid[[igrid]] = c(w.grid[[igrid]], w)
            longLat.grid[[igrid]] = rbind(longLat.grid[[igrid]], wLongLat);
          }
      }
    }
  } else {
    # Set points to extract to each grid geometry
    for (igrid in grids.extract) {
      # For point data, set weights to 1 and coordinates from point locations
      w.grid[[igrid]] = rep(1,length(locations))
      longLat.grid[[igrid]] = cbind(as.numeric(sp::coordinates(locations)[,1]),
                                 as.numeric(sp::coordinates(locations)[,2]))
      location.lookup.grid[[igrid]] = cbind(seq(1,length(locations),by=1),
                                     seq(1,length(locations),by=1))
    }
  }
  raster::removeTmpFiles(h=0)

  # Get string of group and variable names to extract
  var.group.string = vars.extract.summary$var.string
  var.string = row.names(vars.extract.summary)

  # Expand weightrs and grids for each coordinate to each variable to be extracted.
  w.vars = vector('list',nvars)
  longLat.vars = vector('list',nvars)
  location.lookup.vars = vector('list',nvars)
  crs.vars = vector('list',nvars)
  names(w.vars) = var.group.string
  names(longLat.vars) = var.group.string
  names(location.lookup.vars) = var.group.string
  names(crs.vars) = var.group.string

  for (ivar in vars) {
    igrid = vars.extract.summary[ivar, ]$group
    igrid.var = vars.extract.summary[ivar, ]$var.string
    w.vars[[igrid.var]] = w.grid[[igrid]]
    longLat.vars[[igrid.var]] = longLat.grid[[igrid]]
    location.lookup.vars[[igrid.var]] = location.lookup.grid[[igrid]]
    crs.vars[[igrid.var]] = crs.grid[[igrid]]
  }

  if (getET) {
    message('... Extracted DEM elevations from AWS (using tmax coordinate and a GRS80 ellipsoid).')
    crsAUS = sp::CRS("+proj=longlat +ellps=GRS80")
    ind = which(sub(".*/", "", var.group.string) == 'tmax')
    DEMpoints = elevatr::get_elev_point(locations=data.frame(x=longLat.vars[[ind]][,1],
                                                             y=longLat.vars[[ind]][,2]),
                                        prj = crsAUS,
                                        src='aws',ncpu=8, z=ET.DEM.res)
    DEMpoints = DEMpoints$elevation
    if (any(is.na(DEMpoints))) {
      warning('NA DEM values were derived. Trying increasing the resolution zoom.')
    }
  }

  # Define function to extract netCDF data.
  get.nc.data = function( varname, longLat, crs.string, fname, band, interpMethod) {
    return(raster::extract(
      raster::raster(fname, band=band, varname=varname, lvar=3, crs=crs.string),
      longLat, method=interpMethod))
  }

  # Set extraction date terms
  extractYear = as.numeric(format(timepoints2Extract,"%Y"))
  extractMonth = as.numeric(format(timepoints2Extract,"%m"))
  extractDay = as.numeric(format(timepoints2Extract,"%d"))

  message('... Starting to extract data across all locations:')

  # Setup progress bar
  ntimepoints2Extract = length(timepoints2Extract)
  pbar <- progress::progress_bar$new(
    format = "    :current of :total  [:bar] :percent in :elapsed",
    total = ntimepoints2Extract, clear = FALSE, width= 80)

  # Initialise output list variable of extracted data
  data.brick = vector('list', nvars)
  names(data.brick) = vars

  # Set netCDF time indexes for required time points to extract.
  ind = as.integer(difftime(timepoints2Extract, as.Date("1900-1-1",'%Y-%m-%d'),units = "days" ))

  # Loop through each time point and get data
  for (j in ind){
    # Update progress bar
    pbar$tick()

    # Extract data for each variable
    #data.brick.tmp = lapply(var.group.string, get.nc.data, band=j, longLat = longLat.vars)
    data.brick.tmp = mapply(get.nc.data,
                            as.list(var.group.string),
                            longLat.vars,
                            crs.vars,
                            MoreArgs=list(fname=ncdfFilename, band=j, interpMethod=interpMethod),
                            SIMPLIFY=F)
    names(data.brick.tmp) = var.string

    # Append new extracted data
    data.brick =  mapply(rbind, data.brick, data.brick.tmp, SIMPLIFY = F)
  }

  # Handle spatial and temporal (<1990) gaps in solar radiation data
  if (any(var.string %in% 'solarrad')) {
    # Get variable names
    ind = var.string %in% 'solarrad'
    var.string.solar = var.string[ind]
    var.grid.string.solar = vars.extract.summary[var.string.solar,]$var.string

    # Set non-sensible values to NA
    data.brick[[var.string.solar]][data.brick[[var.string.solar]] <0] = NA;
    solarrad_interp = data.brick[[var.string.solar]];

    # Get numerb of grid cells to est
    ngrid.solar = length(w.vars[[var.grid.string.solar]])

    # Calculate the average daily solar radiation for each day of the year.
    message('... Calculating mean daily solar radiation <1990-1-1')
    monthdayUnique = sort(unique(extractMonth*100+extractDay));
    day = as.integer(format(timepoints2Extract, "%d"));
    month = as.integer(format(timepoints2Extract, "%m"));
    monthdayAll = month*100+day;
    solarrad_avg = matrix(NA, length(monthdayUnique), ngrid.solar);
    for (j in 1:length(monthdayUnique)) {
      ind = monthdayAll==monthdayUnique[j];
      if (sum(ind)==1) {
        solarrad_avg[j,] = data.brick[[var.string.solar]][ind,];
      } else {
        if (ncol(solarrad)==1) {
          solarrad_avg[j,] = mean(data.brick[[var.string.solar]][ind,],na.rm=T)
        } else {
          solarrad_avg[j,] = apply(stats::na.omit(data.brick[[var.string.solar]][ind,]),2,mean)
        }
      }
    }

    # Assign the daily average solar radiation to each day prior to 1 Jan 1990
    for (j in 1:length(timepoints2Extract)) {
      if (timepoints2Extract[j]<as.Date('1990-1-1','%Y-%m-%d')) {
        ind = which(monthdayUnique==monthdayAll[j])
        if (length(ind)==1)
          solarrad_interp[j,] = solarrad_avg[ind,]
      }
    }

    # Linearly interpolate time points without a solar radiation value.
    message('... Linearly interpolating gaps in daily solar.')
    for (j in 1:ngrid.solar) {
      filt = is.na(solarrad_interp[,j])
      x = 1:length(timepoints2Extract);
      xpred = x[filt];

      # Interpolate if any NAs
      if (length(xpred)>0) {
        x = x[!filt]
        y = solarrad_interp[!filt,j]

        # Interpolate if at least 2 non-NA obs.
        if (length(y)>1) {
          ypred=stats::approx(x,y,xpred,method='linear', rule=2)
          solarrad_interp[filt,j] = ypred$y
        }
      }
    }

    data.brick[[var.string.solar]] = solarrad_interp
  }

  # Calculate ET at each grid cell and time point.
  # NOTE, va is divided by 10 to go from hPa to Kpa
  nlocations = length(locations)
  if (getET) {

    # Get strings to each netcdf group and variable
    precip.str = vars.extract.summary['precip',]$var.string
    tmax.str = vars.extract.summary['tmax',]$var.string

    # Setup progress bar
    message('... Calculate daily ET at each grid cell.')
    for (i in 1:nlocations) {

        # Get indexes to grid cells of current location, i
        ind = location.lookup.vars[[tmax.str]][i,1]:location.lookup.vars[[tmax.str]][i,2]

        # Initialise outputa
        ET.est = matrix(NA,length(timepoints2Extract),length(ind))

        pbar <- progress::progress_bar$new(
          format = paste("    Location",i,": :current grid cells of :total  [:bar] :percent in :elapsed"),
          total = length(ind), clear = FALSE, width= 80)

        # Loop through each grid cell
        k=0;
        for (j in ind) {

          # Update progress bar
          pbar$tick()

          # Update grid cell counter
          k=k+1

          # Check lat, Elev and precip are finite scalers.
          if (any(!is.finite(data.brick[[precip.str]][,j])) || !is.finite(DEMpoints[j])) {
            message(paste('WARNING: Non-finite input values detected for catchment',i,' at grid cell',j))
            # message(paste('   Elevation value:' ,DEMpoints[j]))
            # message(paste('   Latitude value:' ,longLat.all[j,2]))
            # ind.nonfinite = which(!is.finite(data.brick$`nonsolar/precip`[,j]))
            # if (length(ind.nonfinite)>0)
            #   message(paste('   Precipiation nonfinite value (first):' ,precip[ind.nonfinite[1],j]))
            ET.est[,k] = NA;
            next
          }

          # Build data from of daily climate data
          dataRAW = data.frame(Year =  as.integer(format.Date(timepoints2Extract,"%Y")),
                               Month= as.integer(format.Date(timepoints2Extract,"%m")),
                               Day= as.integer(format.Date(timepoints2Extract,"%d")),
                               Tmin = data.brick[['tmin']][,j],
                               Tmax = data.brick[['tmax']][,j],
                               Rs = data.brick[['solarrad']][,j],
                               va = data.brick[['vprp']][,j]/10.0,
                               Precip = data.brick[['precip']][,j])

          # Convert to required format for ET package.
          # Note, missing_method changed from NULL to "DoY average" because individual grid cells can be NA. eg
          dataPP=Evapotranspiration::ReadInputs(ET.var.names ,dataRAW,constants=NA,stopmissing = c(99,99,99),
                                                interp_missing_days=T, interp_missing_entries=T, interp_abnormal=T,
                                                missing_method=ET.missing_method, abnormal_method=ET.abnormal_method, message = "no")

          # Update constants for the site
          ET.constants$Elev = DEMpoints[j]
          ET.constants$lat = longLat.vars[[tmax.str]][j,2]
          ET.constants$lat_rad = ET.constants$lat / 180.0*pi

          # Call  ET package
          if (ET.function=='ET.Abtew') {
            results <- Evapotranspiration::ET.Abtew(dataPP, ET.constants, ts=ET.timestep,solar="data",AdditionalStats='no', message='no');
          } else if(ET.function=='ET.HargreavesSamani') {
            results <- Evapotranspiration::ET.HargreavesSamani(dataPP, ET.constants, ts=ET.timestep,AdditionalStats='no', message='no');
          } else if(ET.function=='ET.JensenHaise') {
            results <- Evapotranspiration::ET.JensenHaise(dataPP, ET.constants, ts=ET.timestep,solar="data",AdditionalStats='no', message='no');
          } else if(ET.function=='ET.Makkink') {
            results <- Evapotranspiration::ET.Makkink(dataPP, ET.constants, ts=ET.timestep,solar="data",AdditionalStats='no', message='no');
          } else if(ET.function=='ET.McGuinnessBordne') {
            results <- Evapotranspiration::ET.McGuinnessBordne(dataPP, ET.constants, ts=ET.timestep,AdditionalStats='no', message='no');
          } else if(ET.function=='ET.MortonCRAE') {
            results <- Evapotranspiration::ET.MortonCRAE(dataPP, ET.constants,est=ET.Mortons.est, ts=ET.timestep,solar="data",Tdew=FALSE, AdditionalStats='no', message='no');
          } else if(ET.function=='ET.MortonCRWE') {
            results <- Evapotranspiration::ET.MortonCRWE(dataPP, ET.constants,est=ET.Mortons.est, ts=ET.timestep,solar="data",Tdew=FALSE, AdditionalStats='no', message='no');
          } else if(ET.function=='ET.Turc') {
            results <- Evapotranspiration::ET.Turc(dataPP, ET.constants, ts=ET.timestep,solar="data",humid=F, AdditionalStats='no', message='no');
          } #else if (ET.function=='ET.PenmanMonteith') {
          #  results <- Evapotranspiration::ET.PenmanMonteith(dataPP, ET.constants, ts=ET.timestep, solar="data", wind="no", message="no", AdditionalStats="no")
          #}


          # Interpolate monthly or annual data
          if (ET.timestep=='monthly' || ET.timestep=='annual') {
            # Get the last day of each month
            last.day.month = zoo::as.Date(zoo::as.yearmon(stats::time(results$ET.Monthly)), frac = 1)

            # Get days per month
            days.per.month = as.integer(format.Date(zoo::as.Date(zoo::as.yearmon(stats::time(results$ET.Monthly)), frac = 1),'%d'))

            # Set the first month to the start date for extraction.
            start.day.month = as.numeric(format(timepoints2Extract,"%d"))[1]
            days.per.month[1] = days.per.month[1] - start.day.month + 1

            # Set the last month to the end date for extraction.
            end.day.month = as.numeric(format(timepoints2Extract,"%d"))[length(timepoints2Extract)]
            days.per.month[length(days.per.month)] = end.day.month

            # Calculate average ET per day of each month
            monthly.ET.as.daily = zoo::zoo( as.numeric(results$ET.Monthly/days.per.month), last.day.month)

            # Spline interpolate Monthly average ET
            timepoints2Extract.as.zoo = zoo::zoo(NA,timepoints2Extract);
            ET.est.tmp = zoo::na.approx(merge(monthly.ET.as.daily, dates=timepoints2Extract.as.zoo)[, 1], rule=2)
            filt = stats::time(ET.est.tmp)>=stats::start(timepoints2Extract.as.zoo) & stats::time(ET.est.tmp)<=stats::end(timepoints2Extract.as.zoo)
            ET.est.tmp = pmax(0.0, as.numeric( ET.est.tmp));
            ET.est.tmp = ET.est.tmp[filt]
            ET.est[,k] = ET.est.tmp;
          } else {
            ET.est[,k] = results$ET.Daily
          }
        }
        if (i==1) {
          data.brick[['et']] = ET.est
        } else {
          data.brick[['et']] = cbind(data.brick[['et']], ET.est)
        }

    }
  }

  # Define functions for spatial and temporal aggregation
  #--------------------
  # Define time aggregation function.
  do.TemporalAggregation = function( data,
                                     location.lookup,
                                     dates,
                                     location.ID,
                                     timestep.options,
                                     timestep,
                                     fn,
                                     ind) {
    cell.index = location.lookup[location.ID,1]:location.lookup[location.ID,2]
    data.xts = xts::as.xts(data[, cell.index], order.by=dates)
    data.xts <-
      switch(
        which(timestep.options == timestep),
        data.xts, # daily timestep - do nothing
        xts::apply.weekly(data.xts, apply, 2, fn),  # weekly timestep
        xts::apply.monthly(data.xts, apply, 2, fn), # monthly timestep
        xts::apply.quarterly(data.xts, apply, 2, fn), # quarterly timestep
        xts::apply.yearly(data.xts, apply, 2, fn), # annual timestep
        xts::period.apply(data.xts, INDEX=ind, apply, 2, fn), # user defined period
      )
    return(data.xts)
  }

  # Define spatial averaging function
  do.SpatialAggregation = function(data,
                                   w,
                                   location.lookup,
                                   location.ID) {

    cell.index = location.lookup[location.ID,1]:location.lookup[location.ID,2]
    w = w[cell.index]
    return(apply(t(t(data) * w),1,sum,na.rm=TRUE) )
  }

  # Define spatial averaging function
  do.SpatialStatistic = function( data, fn) {
      return( apply(data, 1, fn, na.rm=TRUE) )
  }
  #--------------------

  # Get unique location IDs
  locations.ID = unique(locations@data[,1])

  # Create ISO dates of daily time step extraction dates
  extractDate = ISOdate(year=extractYear,month=extractMonth,day=extractDay)

  # Get the number of days in each time step. This is reported to allow the user to
  # easily see when a timestep is, say, shorter than other time steps (eg ends before the week).
  nDayPerTimestep = mapply(do.TemporalAggregation,
                           list(matrix(rep(1, ntimepoints2Extract ), ncol=1)),
                           list(matrix(c(1,1), ncol=2)),
                           MoreArgs = list(
                             dates = extractDate,
                             location.ID = 1,
                             timestep.options = temporal.timestep.options,
                             timestep = temporal.timestep,
                             fn = 'sum',
                             ind = temporal.timestep.index),
                           SIMPLIFY = F)

  nDayPerTimestep = nDayPerTimestep[[1]]

  # Get the number of time steps (for creation of the outputs)
  timesteps = zoo::index(zoo::as.zoo(nDayPerTimestep))
  nTimeSteps = length(timesteps)

  # Do the aggregation by looping though each catchment and
  # calculate the catchment average and variance.
  #--------------------
  message('... Calculating area weighted results at required time-step.')
  for (i in 1:nlocations) {

    # Do the temporal aggregation
    data.brick.timaAgg = vector('list', nvars)
    names(data.brick.timaAgg) = vars

    data.brick.timaAgg = mapply(do.TemporalAggregation,
                                data.brick,
                                location.lookup.vars,
                                MoreArgs = list(
                                  dates = extractDate,
                                  location.ID = i,
                                  timestep.options = temporal.timestep.options,
                                  timestep = temporal.timestep,
                                  fn = temporal.function.name,
                                  ind = temporal.timestep.index),
                                SIMPLIFY = F)

    names(data.brick.timaAgg) = vars

    # Do spatial aggregation if a spatial function is provided. If spatial aggregation
    # is not required, then build up a data.frame (each time step and data type as one column)
    # for latter conversion to a spatial object.
    #--------------------
    if (do.spatial.analysis) {

      # Create data frame for final results
      catchmentAvgTmp = data.frame(Location.ID=locations.ID[i],
                                   year=as.integer(format(timesteps,"%Y")),
                                   month=as.integer(format(timesteps,"%m")),
                                   day=as.integer(format(timesteps,"%d")),
                                   days.per.timestep = nDayPerTimestep,
                                   row.names = NULL);
      catchmentVarTmp = catchmentAvgTmp

      # Call the spatial averaging function
      catchmentAvgTmp = cbind(catchmentAvgTmp,
                              mapply(do.SpatialAggregation,
                                     data.brick.timaAgg,
                                     w.vars,
                                     location.lookup.vars,
                                     MoreArgs = list(location.ID = i),
                                     SIMPLIFY = T)
                                     )

      # Call spatial variability function (e.g. spatial variance)
      catchmentVarTmp = cbind(catchmentVarTmp,
                                sapply(data.brick.timaAgg,
                                       do.SpatialStatistic,
                                       fn = spatial.function.name)
                                )
    } else if (islocationsPolygon) {
      # Do not undertake spatial aggregation.
      # Collate results for each grid cell.
      # Below, catchmentAvg is converted to a spatial object.
      nGridCells = 1
      if (islocationsPolygon)
        nGridCells = location.lookup[i,2] - location.lookup[i,1] +1

      catchmentAvgTmp = data.frame(Location.ID=rep(locations.ID[i],nGridCells))
      catchmentVar = NA
      for (k in 1:nvars) {
        newDataCol = as.data.frame(t(data.brick.timaAgg[[ vars[k] ]]))
        colnames(newDataCol) = paste(vars[k],format.Date(colnames(newDataCol),'%Y_%m_%d'),sep='_')
        catchmentAvgTmp = cbind.data.frame(catchmentAvgTmp, newDataCol)
      }
    } else {
      # Locations are point values. Reformat into a data.frame.

      catchmentAvgTmp = data.frame(Location.ID=locations.ID[i],
                                   year=as.integer(format(timesteps,"%Y")) ,
                                   month=as.integer(format(timesteps,"%m")),
                                   day=as.integer(format(timesteps,"%d")),
                                   days.per.timestep = nDayPerTimestep,
                                   row.names = NULL);
      catchmentVarTmp = NA
      for (k in 1:nvars) {
        newDataCol = as.data.frame(data.brick.timaAgg[[ vars[k] ]],
                                   row.names = NULL)
        colnames(newDataCol) = vars[k]
        catchmentAvgTmp = cbind.data.frame(catchmentAvgTmp, newDataCol)
      }
    }

    if (i==1) {
      catchmentAvg = catchmentAvgTmp;
      if (do.spatial.analysis) {
        catchmentVar = catchmentVarTmp;
      }
    } else {
      catchmentAvg = rbind(catchmentAvg,catchmentAvgTmp)
      if (do.spatial.analysis) {
        catchmentVar = rbind(catchmentVar,catchmentVarTmp)
      }
    }
  }
  # end for-loop
  #--------------------

  # Remove row names
  rownames(catchmentAvg) = NULL
  if (do.spatial.analysis)
    rownames(catchmentVar) = NULL

  # Handle spatially averaged vs gridded values within catchment polygon.
  if (islocationsPolygon) {
    if (do.spatial.analysis) {
      catchmentAvg = list(catchmentAvg, catchmentVar)
      names(catchmentAvg) = c(paste('catchmentTemporal.',temporal.function.name,sep=''), paste('catchmentSpatial.',spatial.function.name,sep=''))
    } else {
      # Convert data to  a spatial grid (SpatialPixelsDataFrame)
      gridCoords = data.frame(Long=longLat.all[,1], Lat=longLat.all[,2])
      catchmentAvg = cbind.data.frame(gridCoords, catchmentAvg)
      sp::coordinates(catchmentAvg) <- ~Long+Lat
      sp::gridded(catchmentAvg) <- T
    }
  }

  message('Data extraction FINISHED.')
  duration <- difftime(Sys.time(), sys.start.time, units="secs")
  x <- abs(as.numeric(duration))
  message(sprintf("Total run time (DD:HH:MM:SS): %02d:%02d:%02d:%02d",
                  x %/% 86400,  x %% 86400 %/%
                    3600, x %% 3600 %/% 60,  x %% 60 %/% 1))

  return(catchmentAvg)
}
