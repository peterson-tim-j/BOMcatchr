#' Build a netCDF file of the Bureau of Meteorology (Australia) national gridded climate data.
#'
#' \code{makeNetCDF_file} builds one netCDF file containing Australian climate data.
#'
#' makeNetCDF_file creates one netCDF file of daily climate data.
#'
#' @details
#' One netCDF file is created than contains the contains precipitation, minimum
#' daily temperature, maximum daily temperature and vapour pressure and the solar radiation data. It should span from 1/1/1900 to today
#' and requires ~20GB of hard-drive space (using default compression). For the solar radiation, spatial gaps are infilled using a 3x3 moving average repeated 3 times. To minimise the runtime
#' in extracting data, the netCDF file should be stored locally and not on a network drive. Also, building the file requires installation of 7zip.
#'
#' The climate data is sourced from the  Bureau of Meteorology Australian Water Availability Project
#' (\url{http://www.bom.gov.au/jsp/awap/}.  For details see Jones et al. (2009).
#'
#' The output from this function is required for all data extraction functions within this package and must
#' be ran prior.
#'
#' The function can be used to a build netCDF file from scratch or to update an existing netCDF file previously
#' derived from this function. To not build or update a variable, set its respective URL to \code{NA}.
#'
#' @param ncdfFilename is a file path (as string) and name to the netCDF file. The default file name and path is \code{file.path(getwd(),'AWAP.nc')}.
#' @param updateFrom is a date string specifying the start date for the AWAP data. If
#' \code{ncdfFilename} and \code{ncdfSolarFilename} are specified and exist, then the netCDF grids will be
#'  updated with new data from \code{updateFrom}. To update the file from the end of the last day in the file
#'  set \code{updateFrom=NA}. The default is \code{"1900-1-1"}.
#' @param updateTo is a date string specifying the end date for the AWAP data. If
#'  \code{ncdfFilename} and \code{ncdfSolarFilename} are specified and exist, then the netCDF grids will be
#'  updated with new data to \code{updateFrom}. The default is two days ago as YYYY-MM-DD.
#' @param workingFolder is the file path (as string) in which to download the AWAP grid files. The default is \code{getwd()}.
#' @param keepFiles is a logical scalar to keep the downloaded AWAP grid files. The default is \code{FALSE}.
#' @param compressionLevel is the netCDF compression level between 1 (low) and 9 (high), and \code{NA} for no compression.
#' Note, data extraction runtime may slightly increase with the level of compression. The default is \code{5}.
#' @param urlPrecip URL to the folder containing the AWAP daily precipitation grids. The default is from \code{getURLs()$precip}.
#' @param urlTmin URL to the folder containing the AWAP daily minimum temperature grids. The default is from \code{getURLs()$Tmin}.
#' @param urlTmax URL to the folder containing the AWAP daily maximum temperature grids. The default is from \code{getURLs()$Tmax}.
#' @param urlVprp URL to the folder containing the AWAP daily vapour pressure grids. The default is from \code{getURLs()$vprp}.
#' @param urlSolarrad URL to the folder containing the AWAP daily solar radiation grids. The default is from \code{getURLs()$solarrad}.
#'
#' @return
#' A string containing the full file name to the netCDF file.
#'
#' @seealso \code{\link{extractCatchmentData}} for extracting catchment daily average and variance data.
#'
#' @references
#' David A. Jones, William Wang and Robert Fawcett, (2009), High-quality spatial climate data-sets for Australia,
#' Australian Meteorological and Oceanographic Journal, 58 , p233-248.
#'
#' @examples
#' # The example shows how to build the netCDF data cubes.
#' # For an additional example see \url{https://github.com/peterson-tim-j/AWAPer/blob/master/README.md}
#' #---------------------------------------
#'
#' # Set dates for building netCDFs and extracting data from 15 to 5 days ago.
#' startDate = as.Date(Sys.Date()-15,"%Y-%m-%d")
#' endDate = as.Date(Sys.Date()-5,"%Y-%m-%d")
#'
#' # Set names for the netCDF file (in the system temp. directory).
#' ncdfFilename = tempfile(fileext='.nc')
#'
#' \donttest{
#' # Build netCDF grids for all data but only over the defined time period.
#' file.names = makeNetCDF_file(ncdfFilename=ncdfFilename
#'              updateFrom=startDate, updateTo=endDate)
#'
#' # Now, to demonstrate updating the netCDF grids to one day ago, rerun with
#' # the same file names but \code{updateFrom=NA}.
#' file.names = makeNetCDF_file(ncdfFilename=ncdfFilename,
#'              ncdfSolarFilename=ncdfSolarFilename,
#'              updateFrom=NA)
#'
#'  # Remove temp. file
#'  unlink(ncdfFilename)
#' }
#' @export
makeNetCDF_file <- function(
  ncdfFilename=file.path(getwd(),'AWAP.nc'),
  ncdfSolarFilename=file.path(getwd(),'AWAP_solar.nc'),
  updateFrom = as.Date("1900-01-01","%Y-%m-%d"),
  updateTo  = as.Date(Sys.Date()-2,"%Y-%m-%d"),
  workingFolder=getwd(),
  keepFiles=FALSE,
  compressionLevel = 5,
  urlPrecip = getURLs()$precip,
  urlTmin = getURLs()$Tmin,
  urlTmax = getURLs()$Tmax,
  urlVprp = getURLs()$vprp,
  urlSolarrad = getURLs()$solarrad)  {

  # To Update HTML documentationm:
  # devtools::document()
  # NOTE, to build pdf manual. For Windows install: install.packages("tinytex"):
  # path <- find.package("AWAPer")
  # system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

  # Get system time to estimate run time at the end.
  sys.start.time = Sys.time()

  # Check that the ncdf files
  doUpdate = F;
  if (file.exists(ncdfFilename)) {
    doUpdate = TRUE
    message('Starting to update netCDF grids.')
  } else {
    message('Starting to build new netCDF grids.')
  }

  # Check the compresion level is NA or an integer b/w 1 and 9
  if (is.numeric(compressionLevel)) {
    if (compressionLevel<1 || compressionLevel>9) {
      stop('compressionLevel input must be NA or an integer between 1 and 9')
    }
  } else if (!is.na(compressionLevel))  {
    stop('compressionLevel input must be NA or an integer between 1 and 9')
  }


  # Increase maximum file download time from 60 sec to 300 sec.
  # This is following the requat from Em. Prof. Brian Ripley on 9/12/2020.
  options(timeout = max(300, getOption("timeout")))

  # Test the AWAP downloading
  filedate_str = '20000101'
  haveGridGeometry = FALSE;
  haveGridGeometry_solar = FALSE;
  if (!is.na(urlPrecip)) {
    message('... Testing downloading of AWAP precip. grid')
    destFile <- AWAPer::download.ASCII.file(urlPrecip, 'precip.', workingFolder, filedate_str)

    # Get the grid geometry of the non solar data
    headerData <- AWAPer::get.ASCII.file.header('precip.', workingFolder, filedate_str, remove.file=T)
    nCols  <- headerData$nCols
    nRows  <- headerData$nRows
    SWLong <- headerData$SWLong
    SWLat  <- headerData$SWLat
    DPixel <- headerData$DPixel
    nodata <- headerData$nodata
    haveGridGeometry = TRUE;
  }

  # Test the AWAP downloading
  if (!is.na(urlTmin)) {
    message('... Testing downloading of AWAP tmin grid')
    destFile <- AWAPer::download.ASCII.file(urlTmin, 'tmin.', workingFolder, filedate_str)

    # Get grid geometry if not available from precip
    if (!haveGridGeometry) {
      # Get the grid geometry of the non solar data
      headerData <- AWAPer::get.ASCII.file.header('tmin.', workingFolder, filedate_str, remove.file=F)
      nCols  <- headerData$nCols
      nRows  <- headerData$nRows
      SWLong <- headerData$SWLong
      SWLat  <- headerData$SWLat
      DPixel <- headerData$DPixel
      nodata <- headerData$nodata
      haveGridGeometry = TRUE;
    }
    file.remove(destFile$file.name)

  }

  # Test the AWAP downloading
  if (!is.na(urlTmax)) {
    message('... Testing downloading of AWAP tmax grid')
    destFile <- AWAPer::download.ASCII.file(urlTmax, 'tmax.', workingFolder, filedate_str)

    # Get grid geometry if not available from precip
    if (!haveGridGeometry) {
      # Get the grid geometry of the non solar data
      headerData <- AWAPer::get.ASCII.file.header('tmax', workingFolder, filedate_str, remove.file=F)
      nCols  <- headerData$nCols
      nRows  <- headerData$nRows
      SWLong <- headerData$SWLong
      SWLat  <- headerData$SWLat
      DPixel <- headerData$DPixel
      nodata <- headerData$nodata
      haveGridGeometry = TRUE;
    }

    file.remove(destFile$file.name)
  }

  # Test the AWAP downloading
  if (!is.na(urlVprp)) {
    message('... Testing downloading of AWAP vapour pressure grid')
    destFile <- AWAPer::download.ASCII.file(urlVprp, 'vprp.', workingFolder, filedate_str)

    # Get grid geometry if not available from precip
    if (!haveGridGeometry) {
      # Get the grid geometry of the non solar data
      headerData <- AWAPer::get.ASCII.file.header('vprp', workingFolder, filedate_str, remove.file=F)
      nCols  <- headerData$nCols
      nRows  <- headerData$nRows
      SWLong <- headerData$SWLong
      SWLat  <- headerData$SWLat
      DPixel <- headerData$DPixel
      nodata <- headerData$nodata
      haveGridGeometry = TRUE;
    }
    file.remove(destFile$file.name)
  }

  # Test the solar radiation downloading
  if (!is.na(urlSolarrad)) {
    message('... Testing downloading of AWAP solar grid')
    destFile <- AWAPer::download.ASCII.file(urlSolarrad, 'solarrad.', workingFolder, filedate_str)

    # Get the grid geometry of the non solar data
    headerData <- AWAPer::get.ASCII.file.header('solarrad.', workingFolder, filedate_str, remove.file=T)
    nCols_solar  <- headerData$nCols
    nRows_solar  <- headerData$nRows
    SWLong_solar <- headerData$SWLong
    SWLat_solar  <- headerData$SWLat
    DPixel_solar <- headerData$DPixel
    nodata_solar <- headerData$nodata
    haveGridGeometry_solar = TRUE;
  }

  # Create net CDF files
  if (!doUpdate) {

    message('... Building new netcdf data file.')

    # Convert and check input time dates
    if (is.na(updateFrom) || nchar(updateFrom)==0) {
      updateFrom = as.Date("1900-01-01","%Y-%m-%d")
    } else if (is.character(updateFrom))
      updateFrom = as.Date(updateFrom,'%Y-%m-%d');
    if (is.na(updateTo) || nchar(updateTo)==0) {
      updateTo = as.Date(Sys.Date()-1,"%Y-%m-%d")
    } else if (is.character(updateTo)) {
      updateTo = as.Date(updateTo,'%Y-%m-%d');
    } else if (methods::is(updateTo,"Date")) {
        updateTo = min(c(as.Date(Sys.Date()-1,"%Y-%m-%d"),updateTo));
    }
    if (updateFrom >= updateTo)
      stop('The update dates are invalid. updateFrom must be prior to updateTo')

    # Set data time points
    timepoints = seq( as.Date("1900-01-01","%Y-%m-%d"), by="day", to=updateTo)

    # Define variables non-solar variables and grid
    fillvalue <- NA
    nvar = 0
    var.list = list()
    if (haveGridGeometry) {
      # Get x and y vectors (dimensions)
      Longvector = seq(SWLong, by=DPixel,length.out = nCols)
      Latvector = seq(SWLat, by=DPixel,length.out = nRows)

      # define dimensions
      londim <- ncdf4::ncdim_def("nonsolar/Long","degrees",vals=Longvector)
      latdim <- ncdf4::ncdim_def("nonsolar/Lat","degrees",vals=Latvector)
      timedim <- ncdf4::ncdim_def("nonsolar/time",paste("days since 1900-01-01 00:00:00.0 -0:00. DATA FROM:",updateFrom,'. DATA TO:', updateTo),
                                  unlim=T, vals=0:(length(timepoints)-1), calendar='standard')

      nvar = nvar+1
      dlname <- "min daily temperature"
      var.list[[nvar]] <- ncdf4::ncvar_def("nonsolar/tmin","deg_C",list(londim,latdim,timedim),fillvalue,dlname,prec="single", compression=compressionLevel)

      nvar = nvar+1
      dlname <- "min daily temperature"
      var.list[[nvar]] <- ncdf4::ncvar_def("nonsolar/tmax","deg_C",list(londim,latdim,timedim),fillvalue,dlname,prec="single", compression=compressionLevel)

      nvar = nvar+1
      dlname <- "vapour pressure"
      var.list[[nvar]] <- ncdf4::ncvar_def("nonsolar/vprp","hPa",list(londim,latdim,timedim),fillvalue,dlname,prec="single", compression=compressionLevel)

      nvar = nvar+1
      dlname <- "precipitation"
      var.list[[nvar]] <- ncdf4::ncvar_def("nonsolar/precip","mm",list(londim,latdim,timedim),fillvalue,dlname,prec="single", compression=compressionLevel)
    }

    # Define variables solar variables and grid
    if (haveGridGeometry_solar) {
      londim <- ncdf4::ncdim_def("solar/Long","degrees",vals=Longvector)
      latdim <- ncdf4::ncdim_def("solar/Lat","degrees",vals=Latvector)
      timedim <- ncdf4::ncdim_def("solar/time",paste("days since 1900-01-01 00:00:00.0 -0:00. DATA FROM:",updateFrom,'. DATA TO:', updateTo),
                                  unlim=T, vals=0:(length(timepoints)-1), calendar='standard')

      nvar = nvar+1
      dlname <- "Solar radiation"
      var.list[[nvar]] <- ncdf4::ncvar_def("solar/solarrad","MJ/m^2",list(londim,latdim,timedim),fillvalue,dlname,prec="single", compression=compressionLevel)
    }

    # Create netCDF file of required variabless
    ncout <- ncdf4::nc_create(filename=ncdfFilename,vars=var.list, force_v4=T)

    # Put additional attributes into dimension and data variables
    if (haveGridGeometry) {
      ncdf4::ncatt_put(ncout,"nonsolar/Long","axis","X")
      ncdf4::ncatt_put(ncout,"nonsolar/Lat","axis","Y")
      ncdf4::ncatt_put(ncout,"nonsolar/time","axis","T")
      timePoints_netCDF <- ncdf4::ncvar_get(ncout, "nonsolar/time")
    }
    if (haveGridGeometry_solar) {
      ncdf4::ncatt_put(ncout,"solar/Long","axis","X")
      ncdf4::ncatt_put(ncout,"solar/Lat","axis","Y")
      ncdf4::ncatt_put(ncout,"solar/time","axis","T")
      timePoints_netCDF <- ncdf4::ncvar_get(ncout, "solar/time")
    }

    # add global attributes
    ncdf4::ncatt_put(ncout,0,"title","BoM daily climate data")
    ncdf4::ncatt_put(ncout,0,"institution","Data: BoM, R Code: Tim J. Peterson and Conrad Wasko")

    # Convert netCDF time points to date
    if (haveGridGeometry) {
      tunits <- ncdf4::ncatt_get(ncout, "nonsolar/time", "units")
    } else {
      tunits <- ncdf4::ncatt_get(ncout, "solar/time", "units")
    }
    tustr <- strsplit(tunits$value, " ")
    tdstr <- strsplit(unlist(tustr)[3], "-")
    tmonth = as.integer(unlist(tdstr)[2])
    tday = as.integer(unlist(tdstr)[3])
    tyear = as.integer(unlist(tdstr)[1])
    timePoints_R = chron::chron(timePoints_netCDF, origin = c(tmonth, tday, tyear));

  } else {
    # open netcdf file
    ncout <- ncdf4::nc_open(ncdfFilename, write=T)

    # Get the list of exising variables
    allVariable.names = names(ncout$var)

    # Check the netCDF file contains solar and nonsolar data.
    if (!any(grep('nonsolar/',allVariable.names)) && !any(grep('solar/',allVariable.names)))
      stop(paste('The netCDF file appears to have been created using an old version of the package.\n',
                 'Please re-build the netCDF file.',sep=""))

    # Check the variables to be updated are already in the netCDF file.
    if (!is.na(urlPrecip) && !('nonsolar/precip' %in% allVariable.names))
      stop(paste('The netCDF file to be updated with precip. data does not already contain precipitation.\n',
                 'Please re-build the netCDF file to contain all required variablea and then updated',sep=""))
    if (!is.na(urlTmin) && !('nonsolar/tmin' %in% allVariable.names))
      stop(paste('The netCDF file to be updated with Tmin data does not already contain Tmin.\n',
                 'Please re-build the netCDF file to contain all required variablea and then updated',sep=""))
    if (!is.na(urlTmax) && !('nonsolar/tmax' %in% allVariable.names))
      stop(paste('The netCDF file to be updated with Tmax data does not already contain Tmax.\n',
                 'Please re-build the netCDF file to contain all required variablea and then updated',sep=""))
    if (!is.na(urlVprp) && !('nonsolar/vprp' %in% allVariable.names))
      stop(paste('The netCDF file to be updated with vapour pressure data does not already contain vapour pressure.\n',
                 'Please re-build the netCDF file to contain all required variablea and then updated',sep=""))
    if (!is.na(urlSolarrad) && !('solar/solarrad' %in% allVariable.names))
      stop(paste('The netCDF file to be updated with solar radiation data does not already contain solar radiation\n',
                 'Please re-build the netCDF file to contain all required variablea and then updated',sep=""))

    # Get netcdf time points
    timePoints_netCDF <- ncdf4::ncvar_get(ncout, "nonsolar/time")

    # Convert netCDF time points to date
    tunits <- ncdf4::ncatt_get(ncout, "nonsolar/time", "units")
    tustr <- strsplit(tunits$value, " ")
    tdstr <- strsplit(unlist(tustr)[3], "-")
    tmonth = as.integer(unlist(tdstr)[2])
    tday = as.integer(unlist(tdstr)[3])
    tyear = as.integer(unlist(tdstr)[1])
    timePoints_R = chron::chron(timePoints_netCDF, origin = c(tmonth, tday, tyear));

    # Set updateFrom to the end of the netCDF file if updateFrom is NA or ''.
    if (is.na(updateFrom)) {
      updateFrom = max(as.Date(timePoints_R));
    } else if (is.character(updateFrom)) {
      if (nchar(updateFrom)>0) {
        updateFrom = min(c(max(as.Date(timePoints_R)),as.Date(updateFrom,'%Y-%m-%d')));
      } else {
        updateFrom = max(as.Date(timePoints_R));
      }
    } else {
      if (updateFrom == as.Date("1900-01-01","%Y-%m-%d")) {
        doQuit <- readline(prompt="Warning: netCDF grids exist and are to be updated from 1/1/1900. Do you want to continue (Y/N)? : ")
        if (doQuit=='N' | doQuit=='n') {
          message('Now quitting. Note, to update the data form the last day in the netCDF files, set updateFrom=NA')
          return()
        }
      }
    }

    # Set updateTo to the min of the input data and now.
    if (is.na(updateTo)) {
      updateTo = as.Date(Sys.Date()-1,"%Y-%m-%d")
    } else if (is.character(updateTo)) {
      if (nchar(updateTo)>0) {
        updateTo = min(c(as.Date(Sys.Date()-1,"%Y-%m-%d"),as.Date(updateTo,'%Y-%m-%d')));
      } else {
        updateTo = as.Date(Sys.Date()-1,"%Y-%m-%d")
      }
    } else if ( methods::is(updateTo,"Date") ) {
      updateTo = min(c(as.Date(Sys.Date()-1,"%Y-%m-%d"),updateTo));
    }
  }

  # Check input dates
  if (updateFrom >= updateTo)
    stop('The update dates are invalid. updateFrom must be prior to updateTo')

  timepoints2Update = seq( as.Date(updateFrom,'%Y-%m-%d'), by="day", to=as.Date(updateTo,'%Y-%m-%d'))

  if (length(timepoints2Update)==0)
      stop('The dates to update produce a zero vector of dates of zero length. Check the inputs dates are as YYYY-MM-DD')

  message(paste('    NetCDF data will be updated from ',format.Date(updateFrom,'%Y-%m-%d'),' to ', format.Date(updateTo,'%Y-%m-%d')));

  # Update the netCDF file time units to give the new dates for data.
  timepoints = seq( as.Date("1900-01-01","%Y-%m-%d"), by="day", to=updateTo)
  ncdf4::nc_redef(ncout)
  timedim <- ncdf4::ncdim_def("nonsolar/time",paste("days since 1900-01-01 00:00:00.0 -0:00. DATA FROM:",updateFrom,'. DATA TO:', updateTo),
                              unlim=T, vals=0:(length(timepoints)-1), calendar='standard')
  timedim.solar <- ncdf4::ncdim_def("solar/time",paste("days since 1900-01-01 00:00:00.0 -0:00. DATA FROM:",updateFrom,'. DATA TO:', updateTo),
                              unlim=T, vals=0:(length(timepoints)-1), calendar='standard')

  ntimepoints2Update = length(timepoints2Update)

  ncdf4::nc_enddef(ncout)

  # Setup progress bar
  pbar <- progress::progress_bar$new(
    format = "    :current of :total  [:bar] :percent in :elapsed",
    total = ntimepoints2Update, clear = FALSE, width= 80)

  # Start Filling the netCDF grid.
  message('    Downloading non-solar data and importing to netcdf file:')
  for (date in 1:ntimepoints2Update){

    # Update progress bar
    pbar$tick()

    # Get datestring for input filenames
    datestring<-format(timepoints2Update[date], "%Y%m%d")

    # Find index to the date to update within the net CDF grid
    ind = as.integer(difftime(timepoints2Update[date], as.Date("1900-1-1",'%Y-%m-%d'),units = "days" ))+1

    # Update timePoints_netCDF time vector
    timePoints_netCDF[ind] = ind-1;

    # Download data
    #----------------
    didFail_precip=1
    if (!is.na(urlPrecip)) {

      destFile <- AWAPer::download.ASCII.file(urlPrecip, 'precip.', workingFolder, datestring)
      destFile_precip <- destFile$file.name
      didFail_precip <- destFile$didFail
    }

    didFail_tmin=1
    if (!is.na(urlTmin)) {
      destFile <- AWAPer::download.ASCII.file(urlTmin, 'tmin.', workingFolder, datestring)
      destFile_tmin <- destFile$file.name
      didFail_tmin <- destFile$didFail
    }

    didFail_tmax=1
    if (!is.na(urlTmax)) {
      destFile <- AWAPer::download.ASCII.file(urlTmax, 'tmax.', workingFolder, datestring)
      destFile_tmax <- destFile$file.name
      didFail_tmax <- destFile$didFail
    }

    didFail_vprp=1
    if (!is.na(urlVprp)) {
      destFile <- AWAPer::download.ASCII.file(urlVprp, 'vprp.', workingFolder, datestring)
      destFile_vprp <- destFile$file.name
      didFail_vprp <- destFile$didFail
    }
    #----------------

    # Get precip grid and add to Net CDF grid
    if (!is.na(urlPrecip) && file.exists(destFile_precip) && didFail_precip==0) {
      # Re-extra header data in case the NODATA number changes with time (resolving https://github.com/peterson-tim-j/AWAPer/issues/19)
      headerData.tmp <- AWAPer::get.ASCII.file.header('precip.', workingFolder, datestring, remove.file=F)

      AWAPgrid <- AWAPer::readin.ASCII.file(destFile_precip, nRows, noData=headerData.tmp$nodata)
      ncdf4::ncvar_put( ncout, "nonsolar/precip", AWAPgrid, start=c(1, 1, ind), count=c(nCols, nRows, 1), verbose=F )
    }
    if (!is.na(urlPrecip) && file.exists(destFile_precip) && !keepFiles)
      file.remove(destFile_precip)

    # Get tmin grid and add to Net CDF grid
    if (!is.na(urlTmin) && file.exists(destFile_tmin) && didFail_tmin==0) {
      # Re-extra header data in case the NODATA number changes with time (resolving https://github.com/peterson-tim-j/AWAPer/issues/19)
      headerData.tmp <- AWAPer::get.ASCII.file.header('tmin.', workingFolder, datestring, remove.file=F)

      AWAPgrid <- AWAPer::readin.ASCII.file(destFile_tmin, nRows, noData=headerData.tmp$nodata)
      ncdf4::ncvar_put( ncout, "nonsolar/tmin", AWAPgrid, start=c(1, 1, ind), count=c(nCols, nRows, 1), verbose=F )
    }
    if (!is.na(urlTmin) && file.exists(destFile_tmin) && !keepFiles)
      file.remove(destFile_tmin)

    # Get tmax grid and add to Net CDF grid
    if (!is.na(urlTmax) && file.exists(destFile_tmax) && didFail_tmax==0) {
      # Re-extra header data in case the NODATA number changes with time (resolving https://github.com/peterson-tim-j/AWAPer/issues/19)
      headerData.tmp <- AWAPer::get.ASCII.file.header('tmax.', workingFolder, datestring, remove.file=F)

      AWAPgrid <- AWAPer::readin.ASCII.file(destFile_tmax, nRows, noData=headerData.tmp$nodata)
      ncdf4::ncvar_put( ncout, "nonsolar/tmax", AWAPgrid, start=c(1, 1, ind), count=c(nCols, nRows, 1), verbose=F )
    }
    if (!is.na(urlTmax) && file.exists(destFile_tmax) && !keepFiles)
      file.remove(destFile_tmax)

    # Get vapour pr grid and add to Net CDF grid
    if (!is.na(urlVprp) && file.exists(destFile_vprp) && didFail_vprp==0) {
      # Re-extra header data in case the NODATA number changes with time (resolving https://github.com/peterson-tim-j/AWAPer/issues/19)
      headerData.tmp <- AWAPer::get.ASCII.file.header('vprp.', workingFolder, datestring, remove.file=F)

      AWAPgrid <- AWAPer::readin.ASCII.file(destFile_vprp, nRows, noData=headerData.tmp$nodata)
      ncdf4::ncvar_put( ncout, "nonsolar/vprp", AWAPgrid, start=c(1, 1, ind), count=c(nCols, nRows, 1), verbose=F )
    }
    if (!is.na(urlVprp) && file.exists(destFile_vprp) && !keepFiles)
      file.remove(destFile_vprp)

    # Flush data to the netcdf file to avoid losses if code crashed.
    if (date %% 365 == 0) {
      message(paste('Syncing 365 days of data to netCDF file. The time point to be synched is:', format(timepoints2Update[date], "%Y-%m-%d")))
      ncdf4::nc_sync(ncout)
    }
  }

  # Updtate netCDF time variable
  ncdf4::ncvar_put(ncout, "nonsolar/time",timePoints_netCDF)

  # Flush data to the netcdf file to avoid losses if code crashed.
  ncdf4::nc_sync(ncout)

  # BUILD SOLAR DATA NETCDF
  #----------------------------------------------
  # Create net CDF files
  if (haveGridGeometry_solar) {
    message(paste('    NetCDF Solar data will be updated from ',format.Date(updateFrom,'%Y-%m-%d'),' to ', format.Date(updateTo,'%Y-%m-%d')));

    # Setup progress bar
    pbar <- progress::progress_bar$new(
      format = "    :current of :total  [:bar] :percent in :elapsed",
      total = ntimepoints2Update, clear = FALSE, width= 80)

    # Start Filling the netCDF grid.
    message('    Downloading solar data and importing to netcdf file:')

    # Start Filling the netCDF grid.
    for (date in 1:ntimepoints2Update){

      # Update progress bar
      pbar$tick()

      # Get datestring for input filenames
      datestring<-format(timepoints2Update[date], "%Y%m%d")

      # Find index to the date to update within the net CDF grid
      ind = as.integer(difftime(timepoints2Update[date], as.Date("1900-1-1",'%Y-%m-%d'),units = "days" ))+1

      # Update timePoints_netCDF time vector
      timePoints_netCDF[ind] = ind-1;

      # Download the file
      didFail=1
      if (!is.na(urlSolarrad)) {
        destFile <- AWAPer::download.ASCII.file(urlSolarrad, 'solarrad.', workingFolder, datestring)
        destFile_solarrad <- destFile$file.name
        didFail <- destFile$didFail
      }

      # Get vapour pr grid and add to Net CDF grid
      if (file.exists(destFile_solarrad) && didFail==0) {
        # Re-extra header data in case the NODATA number chnages with time (resolving https://github.com/peterson-tim-j/AWAPer/issues/19)
        headerData.tmp <- AWAPer::get.ASCII.file.header('solarrad.', workingFolder, datestring, remove.file=F)

        # Import file
        AWAPgrid <- AWAPer::readin.ASCII.file(destFile_solarrad, nRows_solar, noData=headerData.tmp$nodata)

        # Infill NA values of grid by taking the local average and convert back to matrix.
        AWAPgrid <- raster::raster(AWAPgrid)
        AWAPgrid <- raster::focal(AWAPgrid, w=matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)
        AWAPgrid <- raster::focal(AWAPgrid, w=matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)
        AWAPgrid <- raster::focal(AWAPgrid, w=matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)
        AWAPgrid = raster::as.matrix(AWAPgrid);

        # Add to ncdf
        ncdf4::ncvar_put( ncout, "solar/solarrad", AWAPgrid, start=c(1, 1, ind), count=c(nCols_solar, nRows_solar, 1), verbose=F )
      }

      if (file.exists(destFile_solarrad)  && !keepFiles)
        file.remove(destFile_solarrad)

      # Flush data to the netcdf file to avoid losses if code crashed.
      if (date %% 365 == 0) {
        message(paste('Syncing 365 days of data to netCDF file. The time point to be synched is:', format(timepoints2Update[date], "%Y-%m-%d")))
        ncdf4::nc_sync(ncout)
      }

    }

    # Updtate netCDF time variable
    ncdf4::ncvar_put(ncout, "solar/time",timePoints_netCDF)
  }

  # Close the file, writing data to disk
  ncdf4::nc_close(ncout)

  message('Data construction FINISHED.')
  duration <- difftime(Sys.time(), sys.start.time, units="secs")
  x <- abs(as.numeric(duration))
  message(sprintf("Total run time (DD:HH:MM:SS): %02d:%02d:%02d:%02d",
            x %/% 86400,  x %% 86400 %/%
            3600, x %% 3600 %/% 60,  x %% 60 %/% 1))

  return(ncdfFilename)

}
