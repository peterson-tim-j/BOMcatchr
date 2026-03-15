#' Build a netCDF file of the Bureau of Meteorology (Australia) national gridded climate data.
#'
#' \code{makeNetCDF_file} builds one netCDF file containing Australian climate data.
#'
#' makeNetCDF_file creates one netCDF file of daily climate data.
#'
#' @details
#' One netCDF file is created than contains precipitation, minimum
#' daily temperature, maximum daily temperature and vapour pressure and the solar radiation data. It should span from 1/1/1900 to yesterday
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
#' @param ncdfFilename is a file path (as string) and name to the netCDF file.
#' If only a file name is given, then the file is assumed to exist / be created in \code{getwd()}. The default file name and path is \code{file.path(getwd(),'AWAP.nc')}.
#' @param updateFrom is a date string specifying the start date for the AWAP data. If
#' \code{ncdfFilename} is specified and exist, then the netCDF grids will be
#'  updated with new data from \code{updateFrom}. To update the file from the end of the last day in the file
#'  set \code{updateFrom=NA}. The default is \code{"1900-1-1"}.
#' @param updateTo is a date string specifying the end date for the AWAP data. If
#'  \code{ncdfFilename} is specified and exist, then the netCDF grids will be
#'  updated with new data to \code{updateFrom}. The default is two days ago as YYYY-MM-DD.
#' @param vars is a vector of variables names to build or update. The available variables are: daily precipitation,
#' monthly precipitation, daily minimum temperature, daily maximum temperature, daily 3pm vapour pressure grids and daily solar radiation.
#' Any or all of the defaults are available. If \code{vars=''} and the netCDF does not exist, then the default is
#' \code{c('precip', 'precip.monthly','tmin', 'tmax', 'vprp', 'solarrad')} and provided by \code{rownames(get.variableSource())}.
#' However, if \code{vars=''} and the netCDF file does exist, then default is to use the variable names in the file.
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
#' file.names = makeNetCDF_file(ncdfFilename=ncdfFilename,
#'              updateFrom=startDate,
#'              updateTo=endDate)
#'
#' # Now, to demonstrate updating the netCDF grids to one day ago, rerun with
#' # the same file names but \code{updateFrom=NA}.
#' file.names = makeNetCDF_file(ncdfFilename=ncdfFilename,
#'              updateFrom=NA)
#'
#'  # Remove temp. file
#'  unlink(ncdfFilename)
#' }
#' @export
makeNetCDF_file <- function(
  ncdfFilename=file.path(getwd(),'AWAP.nc'),
  updateFrom = as.Date("1900-01-01","%Y-%m-%d"),
  updateTo  = as.Date(Sys.Date()-2,"%Y-%m-%d"),
  vars = '',
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

  # Check file name is a string
  if (!is.character(ncdfFilename))
    stop('ncdfFilename is invalid. It must be a character string for the file name.')

  # Get workingFolder
  workingFolder = dirname(ncdfFilename)
  if (workingFolder == '.')
    workingFolder = getwd()

  # Check file and folder names and that they're writable
  if (file.exists(ncdfFilename)) {
    # check write access to existing netcdf file
    if (file.access(ncdfFilename, mode = 2) != 0)
      stop('ncdfFilename cannot be written to. Check folder.')
  } else {
    # Get path for new netcdF file
    if (dirname(ncdfFilename) == '.') {
      if (file.access(workingFolder, mode=2) != 0)
        stop('ncdfFilename cannot be written to the working directory. Check the working directory.')
    } else {
      if (file.access(dirname(ncdfFilename), mode=2) != 0)
        stop('ncdfFilename cannot be written to the file directory. Check the fle name and path.')
    }
  }

  # Check the input vars list
  #----------------
  if (!is.character(vars))
    stop('vars must be a character vector of variables names.')

  # If vars is empty, then set the defaults.
  if (length(vars)==1 && vars=='') {
    if (file.exists(ncdfFilename))
      vars = rownames(AWAPer::file.summary(ncdfFilename))
    else
      vars = rownames(get.variableSource())
  }

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
  vars.prior = c()
  if (file.exists(ncdfFilename)) {
    # Get the list of existing variables.
    vars.prior.summary <- AWAPer::file.summary(ncdfFilename)
    grid.prior = vars.prior.summary$group
    vars.prior = row.names(vars.prior.summary)

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

  # Test internet connection
  if (!curl::has_internet())
    stop('No internet connection appears available. Check connection.')

  # Test downloading of required data and get grid geometry.
  filedate_str = '20000101'
  gridgeo = data.frame(nCols  = rep(NA,nvars), nRows = rep(NA,nvars),
                       SWLong = rep(NA,nvars), SWLat = rep(NA,nvars),
                       DPixel = rep(NA,nvars), nodata= rep(NA,nvars),
                       has.geom = rep(F,nvars),
                       ellipsoid.crs = rep(NA,nvars),
                       time.step = rep(NA,nvars),
                       time.datum = rep(NA,nvars),
                       row.names = vars)
  message('... Testing downloading of each variable.')
  for (ivar in vars.2modify) {
    message(paste('    Testing',ivar,'grid data.'))
    destFile <- AWAPer::download.ASCII.file(vars.all[ivar,]$data.URL,
                                            vars.all[ivar,]$data.file.extension,
                                            vars.all[ivar,]$data.file.format,
                                            vars.all[ivar,]$time.step,
                                            ivar,
                                            workingFolder,
                                            filedate_str)

    # Get the grid geometry of the non solar data
    headerData <- AWAPer::get.ASCII.file.header(destFile$file.name,
                                                workingFolder,
                                                remove.file=T)
    gridgeo[ivar,]$nCols  <- headerData$nCols
    gridgeo[ivar,]$nRows  <- headerData$nRows
    gridgeo[ivar,]$SWLong <- headerData$SWLong
    gridgeo[ivar,]$SWLat  <- headerData$SWLat
    gridgeo[ivar,]$DPixel <- headerData$DPixel
    gridgeo[ivar,]$nodata <- headerData$nodata
    gridgeo[ivar,]$has.geom = TRUE;

    # Add ellipsoid CRS from get.variableSource()
    gridgeo[ivar,]$ellipsoid.crs = vars.all[ivar,]$ellipsoid.crs

    # Add time step for variable
    gridgeo[ivar,]$time.step = vars.all[ivar,]$time.step

    # Add string for time origin
    gridgeo[ivar,]$time.datum = paste( gridgeo[ivar,]$time.step, "since 1900-01-01 00:00:00.0 -0:00")
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

          grd.tmp = vars.prior.summary[ivar,]$group
          #grd.tmp = sub("/.*", "", grd.tmp)
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
    longVec = seq(gridgeo.unique[i,]$SWLong,
                     by = gridgeo.unique[i,]$DPixel,
                     length.out = gridgeo.unique[i,]$nCols)
    latVec = seq(gridgeo.unique[i,]$SWLat,
                    by = gridgeo.unique[i,]$DPixel,
                    length.out = gridgeo.unique[i,]$nRows)

    grid.dims[[i]] = list(long = longVec,
                          lat = latVec,
                          time.datum = paste(gridgeo.unique$time.step[i], "since 1900-01-01 00:00:00.0 -0:00"),
                          crs = gridgeo.unique[i,]$ellipsoid.crs)
  }


  # Add new variables to netCDF
  if (nvars.2add>0) {

    # open or create netCDF file
    if (nvars.2update==0) {

      # Create of no updates, just new variables.
      ncout <- RNetCDF::create.nc(filename=ncdfFilename,
                                  format = 'netcdf4')

      # Add global attributes
      RNetCDF::att.put.nc(ncout,'NC_GLOBAL',
                          "title",
                          "NC_CHAR",
                          "BoM climate data")
      RNetCDF::att.put.nc(ncout,'NC_GLOBAL',
                          "institution",
                          "NC_CHAR",
                          "Data: BoM, R Code: Tim J. Peterson and Conrad Wasko")
      RNetCDF::att.put.nc(ncout,'NC_GLOBAL',
                          "history",
                          "NC_CHAR", paste("Created on", date()))

      # Get existing grid groups
      grid.prior = character()

    } else {
      ncout <- RNetCDF::open.nc(ncdfFilename, write=T)
    }

    # Add groups and dimensions for each grid geometry
    ind = which(!(names(grid.dims) %in% grid.prior))
    for (i in ind) {
      # Add new group
      grp = RNetCDF::grp.def.nc(ncout, names(grid.dims)[i])

      # Add ellipoid CRS to the grid
      RNetCDF::att.put.nc(grp,
                          variable = 'NC_GLOBAL',
                          name = "CRS",
                          type = "NC_CHAR",
                          value = grid.dims[[i]]$crs)

      # Add time dimension to group. Variable for dim also added
      # to allow attributes.
      RNetCDF::dim.def.nc(grp,
                          'Time',
                          unlim=TRUE)
      RNetCDF::var.def.nc(grp,
                          varname = 'Time',
                          vartype = 'NC_FLOAT',
                          dimensions = 'Time',
                          deflate = compressionLevel)
      RNetCDF::att.put.nc(grp,'Time',
                          "units",
                          "NC_CHAR",
                          grid.dims[[i]]$time.datum )

      # Define spatial dimensions
      vals= grid.dims[[i]]$long
      ndimvals = length(vals)
      RNetCDF::dim.def.nc(grp, 'Long',
                          dimlength = ndimvals,
                          unlim=FALSE)
      RNetCDF::var.def.nc(grp,
                          varname = 'Long',
                          vartype = 'NC_DOUBLE',
                          dimensions = 'Long',
                          deflate = compressionLevel)
      RNetCDF::att.put.nc(grp,'Long',
                          "units",
                          "NC_CHAR",
                          "degrees")
      RNetCDF::var.put.nc(grp,
                          'Long',
                          vals)

      vals= grid.dims[[i]]$lat
      ndimvals = length(vals)
      RNetCDF::dim.def.nc(grp, 'Lat',
                          dimlength = ndimvals,
                          unlim=FALSE)
      RNetCDF::var.def.nc(grp,
                          varname = 'Lat',
                          vartype = 'NC_DOUBLE',
                          dimensions = 'Lat',
                          deflate = compressionLevel)
      RNetCDF::att.put.nc(grp,'Lat',
                          "units",
                          "NC_CHAR",
                          "degrees")
      RNetCDF::var.put.nc(grp,
                          'Lat',
                          vals)

      # Add attributes axis labels (for raster extraction)
      RNetCDF::att.put.nc(grp,
                          'Long',
                          "axis",
                          "NC_CHAR",
                          "X")
      RNetCDF::att.put.nc(grp,
                          'Lat',
                          "axis",
                          "NC_CHAR",
                          "Y")
      RNetCDF::att.put.nc(grp,
                          'Time',
                          "axis",
                          "NC_CHAR",
                          "T")
    }
    RNetCDF::close.nc(ncout)

    # Create netCDF variable definitions for each data type
    ncout <- RNetCDF::open.nc(ncdfFilename, write=T)
    fillvalue <- NA
    vardef.list = list()
    for (ivar in vars.2add) {

      # Get existing group ID
      ncdf.grid.name = vars.all[ivar,]$ncdf.grid.name
      grp = RNetCDF::grp.inq.nc(ncout, ncdf.grid.name)$self

      # Define variable in group
      RNetCDF::var.def.nc(grp,
                          ivar,
                          'NC_FLOAT',
                          c('Long', 'Lat', 'Time'),
                          deflate = compressionLevel)

      # Add variable attributes
      RNetCDF::att.put.nc(grp,
                          ivar,
                          'Start_date',
                          "NC_CHAR",
                          "0000-1-1")
      RNetCDF::att.put.nc(grp,
                          ivar,
                          'End_date',
                          "NC_CHAR",
                          "9999-12-31")
      RNetCDF::att.put.nc(grp,
                          ivar,
                          'time.step',
                          "NC_CHAR",
                          vars.all[ivar,]$time.step)
      RNetCDF::att.put.nc(grp,
                          ivar,
                          "units",
                          "NC_CHAR",
                          vars.all[ivar,]$units)
      RNetCDF::att.put.nc(grp,
                          ivar,
                          "longname",
                          "NC_CHAR",
                          vars.all[ivar,]$label)

      # Add attribute for missing value
      RNetCDF::att.put.nc(grp,
                          ivar,
                          "_FillValue",
                          "NC_FLOAT",
                          gridgeo[ivar,]$nodata)

    }
    RNetCDF::close.nc(ncout)
  }

  # Get the start and end dates for each variable to be updated
  vars.summary <- AWAPer::file.summary(ncdfFilename)

  # Set update from to the end of the current data
  if (is.na(updateFrom))
    updateFrom = min(vars.summary$to)

  # Limit update dates to plausible range
  updateFrom = max(as.Date('1900-01-01','%Y-%m-%d'), updateFrom)
  updateTo = min(as.Date(Sys.Date(),'%Y-%m-%d'), updateTo)

  # If the earliest and latest dates from existing data differ, then
  # change updateFrom and updateTo
  if (any(updateFrom > vars.summary[vars.2update,]$from)) {
    updateFrom = min(vars.summary[vars.2update,]$from)
    message('... updateFrom reduced to ensure all variables have the same start date.')
  }
  if (any(updateTo < vars.summary[vars.2update,]$to)) {
    updateTo = max(vars.summary[vars.2update,]$to)
    message('... updateTo increased to ensure all variables have the same end date.')
  }

  # Filter vars.2update. If the input vars exists in the netCDF and the data range
  # is wholly within the existing data range, then only update this variable.
  # However, if the
  ind = rep(TRUE, length(vars.2update))
  names(ind) = vars.2update
  for (ivar in vars.2update) {
    if (updateFrom >= vars.summary[ivar,]$from  &&
        updateTo <= vars.summary[ivar,]$to &&
        !(any(ivar %in% vars)))
      ind[ivar] = F
  }
  vars.2update = vars.2update[ind]
  nvars.2update = length(vars.2update)

  # Update list of variables to modify in any way
  vars.2modify = c(vars.2add, vars.2update)
  nvars.2modify = length(vars.2modify)

  if (difftime(updateTo, updateFrom, units="days") <1)
      stop('The update dates are less than 1 day. Check the inputs dates are as YYYY-MM-DD')

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
  ncout <- RNetCDF::open.nc(ncdfFilename, write=T)

  buildSummary.df = data.frame(Imported=rep(0, length(vars.2modify)),
                           Errors=rep(0, length(vars.2modify)),
                           row.names = vars.2modify)

  for (ivar in vars.2modify) {

    # Get group for current var
    ivar.grid = vars.summary[ivar,]$group
    igrp = RNetCDF::grp.inq.nc(ncout, grpname = ivar.grid)$self
    ivar.url = vars.all[ivar,]$data.URL
    ivar.url.ext = vars.all[ivar,]$data.file.extension
    ivar.file.ext = vars.all[ivar,]$data.file.format
    ivar.timetep = vars.all[ivar,]$time.step

    # Set time points to update for the time step of this variable
    timepoints2Update = switch(gridgeo[ivar,]$time.step,
                        days = seq( from=updateFrom, by="days", to=updateTo),
                        weeks =  seq( from=updateFrom, by="weeks", to=updateTo),
                        months = seq( from=as.Date(updateFrom,'%Y%m01'), by="months", to=as.Date(updateTo,'%Y%m01')),
                        years =  seq( from=as.Date(updateFrom,'%Y0101'), by="years", to=as.Date(updateTo,'%Y0101')))
    ntimepoints2Update = length(timepoints2Update)

    # Setup progress bar
    pbar <- progress::progress_bar$new(
      format = paste("    ",ivar,": :current of :total  [:bar] :percent in :elapsed",sep=''),
      total = ntimepoints2Update, clear = FALSE, width= 80)

    for (i in 1:ntimepoints2Update){

      # Update progress bar
      pbar$tick()

      # Get date string for input filenames
      datestring<-format(timepoints2Update[i], "%Y%m%d")

      # Download data
      var.failed = 1
      destFile <- AWAPer::download.ASCII.file(ivar.url,
                                              ivar.url.ext,
                                              ivar.file.ext,
                                              ivar.timetep,
                                              ivar,
                                              workingFolder,
                                              datestring)

      # Read in grid and add to netCDF file
      if (destFile$didFail == 0 && file.exists(destFile$file.name)) {

        grid.tmp <- AWAPer::readin.ASCII.file(destFile$file.name,
                                              gridgeo[ivar,]$nRows,
                                              noData=gridgeo[ivar,]$nodata)

        # Find index to the date to update within the net CDF grid
        ind = ceiling(RNetCDF::utinvcal.nc(gridgeo[ivar,]$time.datum,
                                           format(timepoints2Update[i], '%Y-%m-%d 01:59:59')))
        #ind = as.integer(difftime(timepoints2Update[date], as.Date("1900-1-1",'%Y-%m-%d'),units = "days" ))

        # Put new grid in netCDF
        RNetCDF::var.put.nc(igrp,
                            ivar,
                            grid.tmp,
                            start=c(1, 1, ind),
                            count = c(gridgeo[ivar,]$nCols, gridgeo[ivar,]$nRows, 1),
                            na.mode=1)
        buildSummary.df[ivar,]$Imported = buildSummary.df[ivar,]$Imported + 1
      } else {
        buildSummary.df[ivar,]$Errors = buildSummary.df[ivar,]$Errors + 1
      }

      # Delete downloaded file.
      if (file.exists(destFile$file.name) && !keepFiles)
        file.remove(destFile$file.name)
    }

    # Syncing variable data to netCDF file
    RNetCDF::sync.nc(ncout)

    # Update start and end dates of the available data
    RNetCDF::att.put.nc(igrp,
                        ivar,
                        'Start_date',
                        "NC_CHAR",
                        format.Date(updateFrom,'%Y-%m-%d'))
    RNetCDF::att.put.nc(igrp,
                        ivar,
                        'End_date',
                        "NC_CHAR",
                        format.Date(updateTo,'%Y-%m-%d'))
  }

  # Close the file, writing data to disk
  RNetCDF::close.nc(ncout)

  message('Data construction FINISHED.')

  message('Summary of time points successfully imported (and errors).')
  print(buildSummary.df)

  duration <- difftime(Sys.time(), sys.start.time, units="secs")
  x <- abs(as.numeric(duration))
  message(sprintf("Total run time (DD:HH:MM:SS): %02d:%02d:%02d:%02d",
            x %/% 86400,  x %% 86400 %/%
            3600, x %% 3600 %/% 60,  x %% 60 %/% 1))

  return(ncdfFilename)

}
