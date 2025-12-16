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
#' Amazon Web Service AWS Open Data Terrain Tiles. The data sources change with the user set \code{DEM.res} zoom. The options are
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
#' @param getPrecip logical variable for extracting precipitation. Default is \code{TRUE}.
#' @param getTmin logical variable for extracting Tmin. Default is \code{TRUE}.
#' @param getTmax logical variable for extracting Tmax. Default is \code{TRUE}.
#' @param getVprp logical variable for extracting vapour pressure. Default is \code{TRUE}.
#' @param getSolarrad logical variable for extracting solar radiation. Default is \code{TRUE}.
#' @param getET logical variable for calculating Morton's potential ET. Note, to calculate set \code{getTmin=T}, \code{getTmax=T},
#' \code{getVprp=T} and \code{getSolarrad=T}. Default is \code{TRUE}.
#' @param DEM.res is the zoom resolution for the land surface elevation. \code{elevatr} package is used to extract elevation (metres) from AWS Open Data Terrain Tiles. This input controls the zoom resolution. Higher values increase accuracy, but are significantly slower. See details. Default is 10.
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
#' \code{\link[Evapotranspiration]{ET.MortonCRWE}}, \code{\link[Evapotranspiration]{ET.Turc}}. Default is \code{\link[Evapotranspiration]{ET.MortonCRAE}}.
#' @param ET.Mortons.est character string for the type of Morton's ET estimate. For \code{ET.MortonCRAE}, the options are \code{potential ET},\code{wet areal ET} or \code{actual areal ET}.
#'  For \code{ET.MortonCRWE}, the options are \code{potential ET} or \code{shallow lake ET}. The default is \code{potential ET}.
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
#'               getTmin = FALSE, getTmax = FALSE, getVprp = FALSE,
#'               getSolarrad = FALSE, getET = FALSE,
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
    getPrecip = TRUE,
    getTmin = TRUE,
    getTmax = TRUE,
    getVprp = TRUE,
    getSolarrad = TRUE,
    getET = TRUE,
    DEM.res=10,
    locations=NULL,
    temporal.timestep = 'daily',
    temporal.function.name = 'mean',
    spatial.function.name = 'var',
    interpMethod = '',
    ET.function = 'ET.MortonCRAE',
    ET.Mortons.est = 'potential ET',
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

  # Open NetCDF grids
  ncout <- ncdf4::nc_open(ncdfFilename)

  # Check if the required variable is within the netcdf file.
  netCDF.variables = names(ncout$var)
  if (getTmin & !any(netCDF.variables=='nonsolar/tmin'))
    stop('getTmin is true but the netCDF file was not built with tmin data. Rebuild the netCDF file.')
  if (getTmax & !any(netCDF.variables=='nonsolar/tmax'))
    stop('getTmax is true but the netCDF file was not built with tmax data. Rebuild the netCDF file.')
  if (getVprp & !any(netCDF.variables=='nonsolar/vprp'))
    stop('getVprp is true but the netCDF file was not built with vprp data. Rebuild the netCDF file.')
  if (getPrecip & !any(netCDF.variables=='nonsolar/precip'))
    stop('getPrecip is true but the netCDF file was not built with precip data. Rebuild the netCDF file.')

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
    if (ET.inputdata.filt$Tmin[1] && !getTmin)
      stop('Calculation of ET for the given function requires extractions of tmin (i.e. set getTmin=T)')
    if (ET.inputdata.filt$Tmax[1] && !getTmax)
      stop('Calculation of ET for the given function requires extractions of tmax (i.e. set getTmax=T)')
    if (ET.inputdata.filt$va[1] && !getVprp)
      stop('Calculation of ET for the given function requires extractions of tmax (i.e. set getVprp=T)')
    if (ET.inputdata.filt$Precip[1] && !getPrecip)
      stop('Calculation of ET for the given function requires extractions of precip (i.e. set getPrecip=T)')
    if (ET.inputdata.filt$Rs[1] && !getSolarrad)
      stop('Calculation of ET for the given function requires extractions of solar radiation (i.e. set getSolarrad=T)')

    if (!is.numeric(DEM.res))
      stop('DEM.res must be a numeric value between 1 anf 15.')

    if (DEM.res < 1 || DEM.res>15)
      stop('DEM.res must be a numeric value between 1 anf 15.')
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
  do.spatial.analysis=T
  if (islocationsPolygon && is.na(spatial.function.name) || (is.character(spatial.function.name) && spatial.function.name=='')) {
    do.spatial.analysis=F
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

  # # Open solar rad netCDF file
  # if (getSolarrad)
  #   ncout_solar <- ncdf4::nc_open(ncdfSolarFilename)

  # Get netCDF geometry
  timePoints <- ncdf4::ncvar_get(ncout, "nonsolar/time")
  tunits <- ncdf4::ncatt_get(ncout, "nonsolar/time", "units")
  nTimePoints = length(timePoints);
  Long <- ncdf4::ncvar_get(ncout, "nonsolar/Long")
  Lat <- ncdf4::ncvar_get(ncout, "nonsolar/Lat")
  if (getSolarrad) {
    timePoints_solar <- ncdf4::ncvar_get(ncout, "solar/time")
    tunits <- ncdf4::ncatt_get(ncout, "solar/time", "units")
    nTimePoints_solar = length(timePoints_solar);
    Long_solar <- ncdf4::ncvar_get(ncout, "solar/Long")
    Lat_solar <- ncdf4::ncvar_get(ncout, "solar/Lat")
  }

  # Convert the ncdf time to R time.
  # This is achieved by spliting the time units string into fields.
  # Adapted form http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm.
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth = as.integer(unlist(tdstr)[2])
  tday = as.integer(unlist(tdstr)[3])
  tyear = as.integer(unlist(tdstr)[1])
  timePoints_R = as.Date(chron::chron(timePoints, origin = c(month=tmonth, day=tday, year=tyear),format=c(dates = "y-m-d")));

  # Get start and end date of netCDF data
  doDataExtendCheck = F
  if (length(tustr[[1]])==12) {
    ncdf.dataFrom = as.Date(tustr[[1]][8], '%Y-%m-%d')
    ncdf.dataTo = as.Date(tustr[[1]][12], '%Y-%m-%d')
    doDataExtendCheck = T
  }

  if (getSolarrad) {
    tustr <- strsplit(tunits$value, " ")
    tdstr <- strsplit(unlist(tustr)[3], "-")
    tmonth = as.integer(unlist(tdstr)[2])
    tday = as.integer(unlist(tdstr)[3])
    tyear = as.integer(unlist(tdstr)[1])
    timePoints_solar_R = as.Date(chron::chron(timePoints_solar, origin = c(tmonth, tday, tyear),format=c(dates = "y-m-d")));

    # Get start and end datea of netCDF data
    if (length(tustr[[1]]==12)) {
      ncdf_solar.dataFrom = as.Date(tustr[[1]][8], '%Y-%m-%d')
      ncdf_solar.dataTo = as.Date(tustr[[1]][12], '%Y-%m-%d')
    }
  }

  # Build and provide summary of extraction dates and data extent.
  message('Extraction data summary:')
  if (doDataExtendCheck) {

    # Write summary of Net CDF data.
    message(paste('    NetCDF non-solar radiation climate data exists from', ncdf.dataFrom,'to', ncdf.dataTo));
    if (getSolarrad)
      message(paste('    NetCDF solar radiation data exists from', ncdf_solar.dataFrom,'to', ncdf_solar.dataTo));

    # Warn user is the requested extraction dates are outside of the data record length.
    if (getSolarrad) {
      if (extractFrom < max(ncdf.dataFrom,ncdf_solar.dataFrom)) {
        message('    WARNING: The extraction start date is prior to the existing data start date.');
        message('             Dates are adjusted accordingly.');
        message('             Consider extending the existing date range using makeNetCDF_file()');
      }
      if (extractTo > min(ncdf.dataTo, ncdf_solar.dataTo)) {
        message('    WARNING: The extraction end date is after to the existing data end date.');
        message('             Dates are adjusted accordingly.');
        message('             Consider extending the existing date range using makeNetCDF_file()');
      }
    } else {
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
    }

    # Limit the extraction time points to the data range
    extractFrom = max(c(extractFrom, ncdf.dataFrom))
    extractto = min(c(extractTo, ncdf.dataTo))
    if (getSolarrad) {
      extractFrom = min(c(extractFrom,ncdf_solar.dataFrom));
      extractTo = min(c(extractTo,ncdf_solar.dataTo));
    }
  }

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

  # Get one netCDF layer.
  precipGrd = raster::raster(ncdfFilename, band=nTimePoints, varname='nonsolar/precip',lvar=3)
  if (getSolarrad) {
    solarGrd = raster::raster(ncdfFilename, band=nTimePoints_solar, varname='solar/solarrad',lvar=3)
  }

  # Build a matrix of catchment weights, lat longs, and a lookup table for each catchment.
  message('... Building catchment weights');
  if (islocationsPolygon) {

    w.all = c();
    longLat.all = matrix(NA,0,2)
    location.lookup = matrix(NA,length(locations),2);

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
      w = raster::rasterize(x=locations[i,], y=raster::crop(precipGrd, locations[i,], snap='out'), fun='last',getCover=T)

      # Extract the mask values (i.e. fraction of each grid cell within the polygon.
      w2 = raster::getValues(w);
      filt = w2>0
      wLongLat = sp::coordinates(w)[filt,]
      w=w[filt]

      # Normalise the weights
      w = w/sum(w);

      # Add to data set of all locations
      if (length(w.all)==0) {
        location.lookup[i,] = c(1,length(w));
        w.all = w;
        longLat.all = wLongLat;
      } else {
        location.lookup[i,] = c(length(w.all)+1,length(w.all)+length(w));
        w.all = c(w.all, w)
        longLat.all = rbind(longLat.all, wLongLat);
      }
    }
  } else {

    # For point data, set weights to 1 and coordinates from point locations
    w.all = rep(1,length(locations))
    longLat.all = cbind(as.numeric(sp::coordinates(locations)[,1]),as.numeric(sp::coordinates(locations)[,2]))
    location.lookup = cbind(seq(1,length(locations),by=1),seq(1,length(locations),by=1));
  }

  raster::removeTmpFiles(h=0)

  # Close netCDF connection
  ncdf4::nc_close(ncout)

  if (getSolarrad && getET) {
    message('... Extracted DEM elevations from AWS.')
    longLat.all.df = data.frame(x=longLat.all[,1], y=longLat.all[,2])
    crsAUS = sp::CRS("+proj=longlat +ellps=GRS80")
    DEMpoints = elevatr::get_elev_point(locations=data.frame(x=longLat.all[,1], y=longLat.all[,2]),
                                        prj = crsAUS,
                                        src='aws',ncpu=8, z=DEM.res)
    DEMpoints = DEMpoints$elevation
    if (any(is.na(DEMpoints))) {
      warning('NA DEM values were derived. Trying increasing the resolution zoom.')
    }
  }

  # Create list of all variables to extract.
  varnames = c()
  nvars = 0
  if (getPrecip) {
    nvars = nvars + 1
    varnames[nvars] = 'nonsolar/precip'
  }
  if (getTmin) {
    nvars = nvars + 1
    varnames[nvars] = 'nonsolar/tmin'
  }
  if (getTmax) {
    nvars = nvars + 1
    varnames[nvars] = 'nonsolar/tmax'
  }
  if (getVprp) {
    nvars = nvars + 1
    varnames[nvars] = 'nonsolar/vprp'
  }
  if (getSolarrad) {
    nvars = nvars + 1
    varnames[nvars] = 'solar/solarrad'
  }

  # Define function to extract netCDF data.
  get.nc.data = function( varname, band) {
    return(raster::extract(raster::raster(ncdfFilename, band=band, varname=varname, lvar=3), longLat.all, method=interpMethod))
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
  names(data.brick) = varnames

  # Set netCDF time indexes for required tiem points to extract.
  ind = as.integer(difftime(timepoints2Extract, as.Date("1900-1-1",'%Y-%m-%d'),units = "days" ))+1

  # Loop through each time point and get data
  for (j in ind){
    # Update progress bar
    pbar$tick()

    # Extract data for each variable
    data.brick.tmp = lapply(varnames, get.nc.data, band=j)
    names(data.brick.tmp) = varnames

    # Append new extracted data
    data.brick =  mapply(rbind, data.brick, data.brick.tmp, SIMPLIFY = F)
  }

  # Handle spatial and temporal (<1990) gaps in solar radiation data
  if (getSolarrad) {
    # Set non-sensible values to NA
    data.brick$`solar/solarrad`[data.brick$`solar/solarrad` <0] = NA;
    solarrad_interp = data.brick$`solar/solarrad`;

    # Calculate the average dailysolar radiation for each day of the year.
    message('... Calculating mean daily solar radiation <1990-1-1')
    monthdayUnique = sort(unique(extractMonth*100+extractDay));
    day = as.integer(format(timepoints2Extract, "%d"));
    month = as.integer(format(timepoints2Extract, "%m"));
    monthdayAll = month*100+day;
    solarrad_avg = matrix(NA, length(monthdayUnique), length(w.all));
    for (j in 1:length(monthdayUnique)) {
      ind = monthdayAll==monthdayUnique[j];
      if (sum(ind)==1) {
        solarrad_avg[j,] = data.brick$`solar/solarrad`[ind,];
      } else {
        if (ncol(solarrad)==1) {
          solarrad_avg[j,] = mean(data.brick$`solar/solarrad`[ind,],na.rm=T)
        } else {
          solarrad_avg[j,] = apply(stats::na.omit(data.brick$`solar/solarrad`[ind,]),2,mean)
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
    for (j in 1:length(w.all)) {
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

    nvars = nvars + 1
    varnames[nvars] = 'solarrad.interp'
    data.brick$`solar/solarrad.interp` = solarrad_interp
  }

  # Define time aggregation function.
  do.TemporalAggregation = function( data, cell.index,
                                     temporal.timestep.options, temporal.timestep,
                                     temporal.function.name, temporal.timestep.index) {
    data.xts = xts::as.xts(data[,cell.index], order.by=extractDate)
    data.xts <-
      switch(
        which(temporal.timestep.options == temporal.timestep),
        data.xts, # daily timestep - do nothing
        xts::apply.weekly(data.xts, apply, 2, temporal.function.name),  # weekly timestep
        xts::apply.monthly(data.xts, apply, 2, temporal.function.name), # monthly timestep
        xts::apply.quarterly(data.xts, apply, 2, temporal.function.name), # quarterly timestep
        xts::apply.yearly(data.xts, apply, 2, temporal.function.name), # annual timestep
        xts::period.apply(data.xts, INDEX=temporal.timestep.index, apply, 2, temporal.function.name), # user defined period
      )
    return(data.xts)
  }

  # Define spatial averaging function
  do.SpatialAggregation = function( data, cell.weights) {
    return(apply(t(t(data) * cell.weights),1,sum,na.rm=TRUE))
  }

  # Define spatial averaging function
  do.SpatialStatistic = function( data, spatial.function.name) {
    catchmentVarTmp$precip_mm = apply(data,1,spatial.function.name,na.rm=TRUE);
  }

  # Calculate ET at each grid cell and time point.
  # NOTE, va is divided by 10 to go from hPa to Kpa
  nlocations = length(locations)
  if (getET) {

    # Add ET variable to data brick
    nvars = nvars + 1
    varnames[nvars] = 'evap'

    # Setup progress bar
    message('... Calculate daily ET at each grid cell.')
    pbar <- progress::progress_bar$new(
      format = "    :current grid cells of :total  [:bar] :percent in :elapsed",
      total = length(w.all), clear = FALSE, width= 80)

    for (i in 1:nlocations) {

        # Get indexes to grid cells of current location, i
        ind = location.lookup[i,1]:location.lookup[i,2]

        # Initialise outputa
        ET.est = matrix(NA,length(timepoints2Extract),length(ind))

        # Loop through each grid cell
        k=0;
        for (j in ind) {

          # Update progress bar
          pbar$tick()

          # Update grid cell counter
          k=k+1

          # Check lat, Elev and precip are finite scalers.
          if (any(!is.finite(data.brick$`nonsolar/precip`[,j])) || !is.finite(DEMpoints[j]) || !is.finite(longLat.all[j,2])) {
            message(paste('WARNING: Non-finite input values detected for catchment',i,' at grid cell',j))
            message(paste('   Elevation value:' ,DEMpoints[j]))
            message(paste('   Latitude value:' ,longLat.all[j,2]))
            ind.nonfinite = which(!is.finite(data.brick$`nonsolar/precip`[,j]))
            if (length(ind.nonfinite)>0)
              message(paste('   Precipiation nonfinite value (first):' ,precip[ind.nonfinite[1],j]))
            ET.est[,k] = NA;
            next
          }

          # Build data from of daily climate data
          dataRAW = data.frame(Year =  as.integer(format.Date(timepoints2Extract,"%Y")),
                               Month= as.integer(format.Date(timepoints2Extract,"%m")),
                               Day= as.integer(format.Date(timepoints2Extract,"%d")),
                               Tmin = data.brick$`nonsolar/tmin`[,j],
                               Tmax = data.brick$`nonsolar/tmax`[,j],
                               Rs = data.brick$`solar/solarrad.interp`[,j],
                               va = data.brick$`nonsolar/vprp`[,j]/10.0,
                               Precip = data.brick$`nonsolar/precip`[,j])

          # Convert to required format for ET package.
          # Note, missing_method changed from NULL to "DoY average" because individual grid cells can be NA. eg
          dataPP=Evapotranspiration::ReadInputs(ET.var.names ,dataRAW,constants=NA,stopmissing = c(99,99,99),
                                                interp_missing_days=T, interp_missing_entries=T, interp_abnormal=T,
                                                missing_method=ET.missing_method, abnormal_method=ET.abnormal_method, message = "no")

          # Update constants for the site
          ET.constants$Elev = DEMpoints[j]
          ET.constants$lat = longLat.all[j,2]
          ET.constants$lat_rad = longLat.all[j,2]/180.0*pi

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
          data.brick$evap = ET.est
        } else {
          data.brick$evap = cbind(data.brick$evap, ET.est)
        }

    }
  }

  # Loop though each catchment and calculate the catchment average and variance.
  message('... Calculating area weighted results at required time-step.')
  for (i in 1:nlocations) {

    # Get the weights for the catchment
    ind = location.lookup[i,1]:location.lookup[i,2]
    w = w.all[ind]

    extractDate = ISOdate(year=extractYear,month=extractMonth,day=extractDay)

    # Do the temporal aggregation
    #-----------------------------
    data.brick.timaAgg = vector('list', nvars)
    names(data.brick.timaAgg) = varnames

    data.brick.timaAgg = lapply(data.brick, do.TemporalAggregation,
                                cell.index = ind,
                                temporal.timestep.options, temporal.timestep,
                                temporal.function.name, temporal.timestep.index)

    names(data.brick.timaAgg) = varnames
    #-----------------------------

    # Determine the number of time steps (for creation of the output)
    timesteps=zoo::index(zoo::as.zoo(data.brick.timaAgg[[1]]))
    nTimeSteps = length(timesteps)

    # Do spatial aggregation if a spatial function is provided. If spatial aggregation
    # is not required, then build up a data.frame (each time step and data type as one column)
    # for latter conversion to a spatial object.
    #-----------------------------
    if (do.spatial.analysis) {
      # Create data frame for final results
      catchmentAvgTmp = data.frame(CatchmentID=locations[i,1],year=as.integer(format(timesteps,"%Y")) ,month=as.integer(format(timesteps,"%m")),day=as.integer(format(timesteps,"%d")),row.names = NULL);
      catchmentVarTmp = catchmentAvgTmp

      # Check if there are enough grid cells to calculate the variance.
      calcVariance =  F;
      if (length(ind)>1)
        calcVariance =  T;

      # Do spatial aggregation using grid cell weights
      #-----------------------------
      # Define spatial averaging function
      catchmentAvgTmp = cbind(catchmentAvgTmp, sapply(data.brick.timaAgg, do.SpatialAggregation, w))
      if (calcVariance) {
        catchmentVarTmp = cbind(catchmentVarTmp, sapply(data.brick.timaAgg, do.SpatialStatistic, spatial.function.name))
      }

    } else {
      # Do not undertake spatial aggregation. Instead collate
      # results for each grid cell.

      nGridCells = location.lookup[i,2] - location.lookup[i,1] +1
      catchmentAvgTmp = data.frame(CatchmentID=rep(locations@data[i,1],nGridCells))
      catchmentVar = NA
      for (k in 1:nvars) {
        newDataCol = as.data.frame(t(data.brick.timaAgg[[ varnames[k] ]]))
        colnames(newDataCol) = paste(varnames[k],format.Date(colnames(newDataCol),'%Y_%m_%d'),sep='_')
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
