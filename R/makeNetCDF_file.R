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
#' @param vars is a vector of variables names to build or update. The available variables are: daily precipitation,
#' daily minimum temperature, daily maximum temperature, daily 3pm vapour pressure grids and daily solar radiation.
#' Any or all of the defaults are available. The default is \code{c('precip', 'tmin', 'tmax', 'vprp', 'solarrad')} and
#' provided by \code{rownames(get.variableSource())}.
#' @param workingFolder is the file path (as string) in which to download the AWAP grid files. The default is \code{getwd()}.
#' @param keepFiles is a logical scalar to keep the downloaded AWAP grid files. The default is \code{FALSE}.
#' @param compressionLevel is the netCDF compression level between 1 (low) and 9 (high), and \code{NA} for no compression.
#' Note, data extraction runtime may slightly increase with the level of compression. The default is \code{5}.
#' @param vars.sourceData is a data.frame of variable unit, time step and source URLs. This input is provided in-case the default URLs need to be changed.
#' The default is \code{get.variableSource())}
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
  vars = rownames(get.variableSource()),
  workingFolder=getwd(),
  keepFiles=FALSE,
  compressionLevel = 5,
  vars.sourceData = get.variableSource() )  {

  # To Update HTML documentationm:
  # devtools::document()
  # NOTE, to build pdf manual. For Windows install: install.packages("tinytex"):
  # path <- find.package("AWAPer")
  # system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

  # Get system time to estimate run time at the end.
  sys.start.time = Sys.time()

  if (!is.character(ncdfFilename))
    stop('ncdfFilename is invalid. It must be a character string for the file name.')

  # Check the input variables
  vars.all = get.variableSource()
  vars.all.names = rownames(vars.all)
  if (length(vars)==0)
    stop('The input data variable names,  vars, must be input.')
  if ( !all(unique(vars) %in% vars.all.names))
    stop(paste('The input vars contain unhandled variable names. The available inputs vars are:',vars.all))

  # Set number of variables
  nvars = length(vars)

  # Check that the ncdf files
  ncdf.exists = F;
  vars.prior = c()
  vars.prior.wgrid = vars.prior
  if (file.exists(ncdfFilename)) {
    ncdf.exists = TRUE

    # open netcdf file
    ncout <- ncdf4::nc_open(ncdfFilename, write=F)

    # Get the list of existing variables.
    vars.prior = names(ncout$var)
    vars.prior.wgrid = vars.prior
    vars.prior = sub(".*?/", "", vars.prior)
    ncdf4::nc_close(ncout)

    # Get new variables to add to netCDF file
    ind.vars2add = !(vars %in% vars.prior)
    vars.2add = vars[ind.vars2add]

    # Set variables to update. Note this is all of
    # the existing variables in the netCDF file,
    # *(excludes those variable to add).
    # This is done because, if the updateFrom
    # or updateTo for vars extends beyond the
    # current data range, then all variables need
    # to be updated. Later, vars.2update is filtered
    # based on updateFrom and updateTo period.
    ind.vars2update = vars.prior
    vars.2update = vars.prior
  } else {
    vars.2update = c()
    vars.2add = vars
  }
  vars.2modify = c(vars.2add, vars.2update)
  nvars.2modify = length(vars.2modify)
  nvars.2update = length(vars.2update)
  nvars.2add = length(vars.2add)

  # Check the compression level is NA or an integer b/w 1 and 9
  if (is.numeric(compressionLevel)) {
    if (compressionLevel<1 || compressionLevel>9) {
      stop('compressionLevel input must be NA or an integer between 1 and 9')
    }
  } else if (!is.na(compressionLevel))  {
    stop('compressionLevel input must be NA or an integer between 1 and 9')
  }

  # Check input dates and convert and check input time dates
  if (is.na(updateFrom) || nchar(updateFrom)==0) {
    updateFrom = NA
  } else if (is.character(updateFrom))
    updateFrom = as.Date(updateFrom,'%Y-%m-%d');
  if (is.na(updateTo) || nchar(updateTo)==0) {
    updateTo = as.Date(Sys.Date()-2,"%Y-%m-%d")
  } else if (is.character(updateTo)) {
    updateTo = as.Date(updateTo,'%Y-%m-%d');
  } else if (methods::is(updateTo,"Date")) {
    updateTo = min(c(as.Date(Sys.Date()-1,"%Y-%m-%d"),updateTo));
  }
  if (!is.na(updateFrom) && updateFrom >= updateTo)
    stop('The update dates are invalid. updateFrom must be prior to updateTo')


  # Increase maximum file download time from 60 sec to 300 sec.
  # This is following the request from Em. Prof. Brian Ripley on 9/12/2020.
  options(timeout = max(300, getOption("timeout")))

  # Test downloading of required data and get grid geometry.
  filedate_str = '20000101'
  gridgeo = data.frame(nCols  = rep(NA,nvars), nRows = rep(NA,nvars),
                       SWLong = rep(NA,nvars), SWLat = rep(NA,nvars),
                       DPixel = rep(NA,nvars), nodata= rep(NA,nvars),
                       has.geom = rep(F,nvars),
                       row.names = vars)
  message('... Testing downloading of each variable.')
  for (ivar in vars.2modify) {
    message(paste('    Testing',ivar,'grid data.'))
    destFile <- AWAPer::download.ASCII.file(vars.all[ivar,]$data.URL, ivar, workingFolder, filedate_str)

    # Get the grid geometry of the non solar data
    headerData <- AWAPer::get.ASCII.file.header(ivar, workingFolder, filedate_str, remove.file=T)
    gridgeo[ivar,]$nCols  <- headerData$nCols
    gridgeo[ivar,]$nRows  <- headerData$nRows
    gridgeo[ivar,]$SWLong <- headerData$SWLong
    gridgeo[ivar,]$SWLat  <- headerData$SWLat
    gridgeo[ivar,]$DPixel <- headerData$DPixel
    gridgeo[ivar,]$nodata <- headerData$nodata
    gridgeo[ivar,]$has.geom = TRUE;
  }

  # Identify the unique grid dimensions and assign grid
  # to each variable name
  #-------------
  gridgeo.unique = unique.data.frame(gridgeo)
  ngrids = nrow(gridgeo.unique)
  vars.all = cbind(vars.all,ncdf.grid.name=NA)
  gridgeo.unique.newnames = rep('',ngrids)
  grid.max = 0
  # First get the existing grids
  for (ivar in vars.2update) {
    for (i in 1:ngrids) {
      if (all(gridgeo[ivar,] == gridgeo.unique[i,])
          && any(vars.prior %in% ivar)) {

          grd.tmp = vars.prior.wgrid[vars.prior %in% ivar]
          grd.tmp = sub("/.*", "", grd.tmp)
          vars.all[ivar,]$ncdf.grid.name = grd.tmp

          gridgeo.unique.newnames[i] = grd.tmp
          grid.max = max(grid.max,
                         as.numeric(sub(".*d", "", grd.tmp)))
      }
    }
  }
  # Add new grids, but ensure that and new grid geometry has a
  # different grid number and above that of the existing grids.
  for (ivar in vars.2add) {
    for (i in 1:ngrids) {
      if (all(gridgeo[ivar,] == gridgeo.unique[i,])
          && !any(vars.prior %in% ivar)) {

          # If there is NOT an existing name for this grid geometry, then
          # create a new name. Otherwise, add the existing name.
          if (gridgeo.unique.newnames[i] == '') {
            vars.all[ivar,]$ncdf.grid.name = paste('grid',i+grid.max,sep='')
            gridgeo.unique.newnames[i] = paste('grid',i+grid.max,sep='')
          } else {
            vars.all[ivar,]$ncdf.grid.name = gridgeo.unique.newnames[i]
          }
      }
    }
  }
  row.names(gridgeo.unique) = gridgeo.unique.newnames
  #-------------

  # Define each dimension for each unique grid
  timepoints = seq( as.Date("1900-01-01","%Y-%m-%d"), by="day", to=updateTo)
  grid.dims =vector('list',ngrids)
  names(grid.dims) = row.names(gridgeo.unique)
  for (i in 1:ngrids) {
    Longvector = seq(gridgeo.unique[i,]$SWLong,
                     by = gridgeo.unique[i,]$DPixel,
                     length.out = gridgeo.unique[i,]$nCols)
    Latvector = seq(gridgeo.unique[i,]$SWLat,
                    by = gridgeo.unique[i,]$DPixel,
                    length.out = gridgeo.unique[i,]$nRows)

    # define dimensions
    ncdf.grid.name = names(grid.dims)[i]
    londim <- ncdf4::ncdim_def(paste(ncdf.grid.name, "/Long",sep=''),"degrees",vals=Longvector)
    latdim <- ncdf4::ncdim_def(paste(ncdf.grid.name, "/Lat",sep='') ,"degrees",vals=Latvector)
    timedim <- ncdf4::ncdim_def(paste(ncdf.grid.name,"/time",sep=''),
                                paste("days since 1900-01-01 00:00:00.0 -0:00"),
                                unlim=T, vals=0:(length(timepoints)-1), calendar='standard')
    grid.dims[[i]] = list(londim, latdim, timedim)
  }


  # Add new variables to netCDF
  if (nvars.2add>0) {
    # Create netCDF variable definitions for each data type
    fillvalue <- NA
    vardef.list = list()
    for (ivar in vars.2add) {

      ncdf.grid.name = vars.all[ivar,]$ncdf.grid.name
      ncdf.name = paste(ncdf.grid.name,'/',vars.all[ivar,]$ncdf.name, sep='')
      vardef.list[[ivar]] <- ncdf4::ncvar_def(name = ncdf.name,
                                              units = vars.all[ivar,]$units,
                                              dim = grid.dims[[ vars.all[ivar,]$ncdf.grid.name ]],
                                              missval = fillvalue,
                                              longname = vars.all[ivar,]$label,
                                              prec = "single",
                                              compression = compressionLevel)
    }

    # open or create netCDF file
    if (nvars.2update==0) {
      # Create of no updates, just new variables.
      ncout <- ncdf4::nc_create(filename=ncdfFilename,vars=vardef.list, force_v4=T)

      # Add global attributes
      ncdf4::ncatt_put(ncout,0,"title","BoM climate data")
      ncdf4::ncatt_put(ncout,0,"institution","Data: BoM, R Code: Tim J. Peterson and Conrad Wasko")

      # Add attributes for the start and end dates of each variable
      for (ivar in vars.2add) {
        ncdf.grid.name = vars.all[ivar,]$ncdf.grid.name
        ncdf.name = paste(ncdf.grid.name,'/',vars.all[ivar,]$ncdf.name, sep='')

        ncdf4::ncatt_put(ncout,
                         varid = ncdf.name,
                         attname = 'Start_date',
                         attval = "0000-1-1")

        ncdf4::ncatt_put(ncout,
                         varid = ncdf.name,
                         attname = 'End_date',
                         attval = "9999-12-31")

      }

    } else {
      # Open existing netCDF file.
      ncout <- ncdf4::nc_open(ncdfFilename, write=T)

      # Add each new variable and daa start and end dates.
      for (ivar in vars.2add) {
        ncout <- ncdf4::ncvar_add(ncout, vardef.list[[ivar]])

        ncdf.grid.name = vars.all[ivar,]$ncdf.grid.name
        ncdf.name = paste(ncdf.grid.name,'/',vars.all[ivar,]$ncdf.name, sep='')

        ncdf4::ncatt_put(ncout,
                         varid = ncdf.name,
                         attname = 'Start_date',
                         attval = "0000-1-1")

        ncdf4::ncatt_put(ncout,
                         varid = ncdf.name,
                         attname = 'End_date',
                         attval = "9999-12-31")
      }

      # Write to netCDF file and close
      ncdf4::nc_sync(ncout)
    }

    # Set dimension axis
    for (ivar in vars.2add) {
      ivar.dim.name = vars.all[ivar,]$ncdf.grid.name
      ncdf4::ncatt_put(ncout, paste(ivar.dim.name,"/Long",sep=''),"axis","X")
      ncdf4::ncatt_put(ncout,paste(ivar.dim.name ,"/Lat",sep='') ,"axis","Y")
      ncdf4::ncatt_put(ncout,paste(ivar.dim.name ,"/time",sep=''),"axis","T")
    }
    ncdf4::nc_close(ncout)
  }

  # Get the start and end dates for each variable to be updated
  existing.dates <- AWAPer::summary(ncdfFilename)

  # Set update from to the end of the current data
  if (is.na(updateFrom))
    updateFrom = min(existing.dates$to)

  # If the earliest and latest dates from existing data differ, then
  # change updateFrom and updateTo
  if (diff(range(existing.dates$from))>0 &&
      min(existing.dates$from) < updateFrom &&
      min(existing.dates$from) > as.Date('0000-01-01', '%Y-%m-%d')) {

      updateFrom = min(existing.dates$from)
      message('... updateFrom reduced to ensure all variables have the same start date.')
  }
  if (diff(range(existing.dates$to))>0 &&
      max(existing.dates$to) > updateTo &&
      max(existing.dates$to) < as.Date('9999-12-31', '%Y-%m-%d')) {

      updateTo = max(existing.dates$to)
      message('... updateTo increased to ensure all variables have the same end date.')
  }

  # Filter vars.2update. If the input vars exists in the netCDF and the data range
  # is wholly within the existing data range, then only update this variable.
  # However, if the
  ind = rep(TRUE, length(vars.2update))
  names(ind) = vars.2update
  for (ivar in vars.2update) {
    if (existing.dates[ivar,]$from >= updateFrom &&
        existing.dates[ivar,]$to <= updateTo)
      ind[ivar] = F
  }
  vars.2update = vars.2update[ind]
  nvars.2update = length(vars.2update)

  # Update list of variables to modify in any way
  vars.2modify = c(vars.2add, vars.2update)
  nvars.2modify = length(vars.2modify)

  # Set time points to update
  timepoints2Update = seq( as.Date(updateFrom,'%Y-%m-%d'), by="day", to=as.Date(updateTo,'%Y-%m-%d'))
  ntimepoints2Update = length(timepoints2Update)
  timepoints = seq( as.Date("1900-01-01","%Y-%m-%d"), by="day", to=updateTo)

  if (length(timepoints2Update)==0)
      stop('The dates to update produce a zero vector of dates of zero length. Check the inputs dates are as YYYY-MM-DD')

  # Give summary of data changes
  message('... NetCDF file will be updated as follows:')
  if (nvars.2add==0) {
    message('       - New variables to add: (none)')
  } else {
    message(paste(c('       - New variables to add: ',paste(vars.2add, ' '))))
  }
  if (nvars.2update==0) {
    message('       - Existing variables to modify: (none)')
  } else {
    message(paste(c('       - Existing variables to modify: ',paste(vars.2update, ' '))))
  }
  message(paste('       - Data will be updated from ',
                format.Date(updateFrom,'%Y-%m-%d'),' to ',
                format.Date(updateTo,'%Y-%m-%d')));

  message('... Downloading data for each variable and importing to netcdf file:')
  ncout <- ncdf4::nc_open(ncdfFilename, write=T)
  for (ivar in vars.2modify) {

    ivar.tmp = paste(ivar,".",sep="")
    ivar.grid.name = vars.all[ivar,]$ncdf.grid.name
    ivar.ncdf.name =  paste(ivar.grid.name,'/',vars.all[ivar,]$ncdf.name, sep='')
    ivar.url = vars.all[ivar,]$data.URL
    ivar.doInfill = vars.all[ivar,]$infillGaps

    ncdf.name = paste(ncdf.grid.name,'/',vars.all[ivar,]$ncdf.name, sep='')

    # Setup progress bar
    pbar <- progress::progress_bar$new(
      format = paste("    ",ivar,": :current of :total  [:bar] :percent in :elapsed",sep=''),
      total = ntimepoints2Update, clear = FALSE, width= 80)

    for (date in 1:ntimepoints2Update){

      # Update progress bar
      pbar$tick()

      # Get date string for input filenames
      datestring<-format(timepoints2Update[date], "%Y%m%d")

      # Download data
      var.failed = 1
      destFile <- AWAPer::download.ASCII.file(ivar.url,
                                              ivar.tmp,
                                              workingFolder,
                                              datestring)

      # Read in grid and add to netCDF file
      if (destFile$didFail == 0 && file.exists(destFile$file.name)) {
        headerData.tmp <- AWAPer::get.ASCII.file.header(ivar.tmp,
                                                        workingFolder,
                                                        datestring,
                                                        remove.file = F)

        grid.tmp <- AWAPer::readin.ASCII.file(destFile$file.name,
                                              gridgeo[ivar,]$nRows,
                                              noData=gridgeo[ivar,]$nodata)

        # Do infilling of NAs. Generally only included for gaos in solar radiation.
        if (ivar.doInfill) {
          # Infill NA values of grid by taking the local average and convert back to matrix.
          grid.tmp <- raster::raster(grid.tmp)
          grid.tmp <- raster::focal(grid.tmp, w=matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)
          grid.tmp <- raster::focal(grid.tmp, w=matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)
          grid.tmp <- raster::focal(grid.tmp, w=matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)
          grid.tmp = raster::as.matrix(grid.tmp);
        }

        # Find index to the date to update within the net CDF grid
        ind = as.integer(difftime(timepoints2Update[date], as.Date("1900-1-1",'%Y-%m-%d'),units = "days" ))

        # Put new grid in netCDF
        ncdf4::ncvar_put( ncout, ivar.ncdf.name,
                          grid.tmp, start=c(1, 1, ind),
                          count = c(gridgeo[ivar,]$nCols, gridgeo[ivar,]$nRows, 1),
                          verbose=F )
      }

      # Delete downloaded file.
      if (file.exists(destFile$file.name) && !keepFiles)
        file.remove(destFile$file.name)
    }

    # Syncing variable data to netCDF file
    ncdf4::nc_sync(ncout)

    # Update start and end dates of the available data
    ncdf4::ncatt_put(ncout,
                     varid = ivar.ncdf.name,
                     attname = 'Start_date',
                     attval = format.Date(updateFrom,'%Y-%m-%d'))

    ncdf4::ncatt_put(ncout,
                     varid = ivar.ncdf.name,
                     attname = 'End_date',
                     attval = format.Date(updateTo,'%Y-%m-%d'))
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
