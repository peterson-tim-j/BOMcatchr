#' \code{grid.download} downloads grid data.
#'
#' This is an internal function. It downloads an ARCMAP ASCII grid file from a URL.
#'
#' @return
#' A list variable of the file name and succes/failure flag.
#'
#' @keywords internal
#' @noRd
grid.download <- function (url.string, ivar.url.ext, ivar.file.ext, ivar.timestep, data.type.label,  workingFolder, datestring) {

  if (!is.character(url.string))
    pretty_stop(paste('The input URL for',data.type.label,'must be a URL string.'))

  if (!startsWith(url.string,'https://'))
    pretty_stop(paste('The input URL string for',data.type.label,'must start "with https://" '))

  if (ivar.timestep == 'days') {
    sdate = datestring
    edate = datestring
  } else if (ivar.timestep == 'months') {
    sdate = format( as.Date(datestring,'%Y%m%d'),"%Y%m01")
    edate = format(as.Date(format(as.Date(sdate,'%Y%m%d') + 31,"%Y%m01"),'%Y%m%d')-1,'%Y%m%d')
  } else if (ivar.timestep == 'years') {
    sdate = format( as.Date(datestring,'%Y%m%d'),"%Y0101")
    edate = format(as.Date(datestring,'%Y%m%d') ,"%Y1231")
  } else {
    pretty_stop(paste('Unknown source data time step:',ivar.timestep))
  }

  # Build URL
  url = paste0(url.string,
               sdate,
               edate ,
               '.',
               ivar.url.ext)

  # Build source file destination name
  des.file.name = file.path(workingFolder, paste(data.type.label, datestring,'.', ivar.url.ext, sep=''))

  # Download the zip file
  didFail = 1
  didFail = tryCatch({
    bin.data = RCurl::getBinaryURL(url)
    fid <- file(des.file.name, "wb")
    writeBin(bin.data, fid)
    close(fid)
    didFail = 0
  },error = function(cond) {return(1)})

  return(list(file.name=des.file.name, didFail=didFail))
}

#' \code{grid.read} reads in header or grid data.
#'
#' This is an internal function. It reads the downloaded boM compressed file and gets the
#' header info or the grid data (assumes an ARCMAP ASCII grid format).
#'
#' @return
#' List of header information or a matrix of the grid data.
#'
#' @keywords internal
#' @noRd
grid.read <- function(file.name, ivar.file.ext, only.header, nRows, nCols, noData) {

  # Get file connection to .Z and .zip compressed files
  f.extn = tools::file_ext(file.name)
  if (f.extn == 'Z') {
    con <- archive::file_read(file.name)
  } else if (f.extn == 'zip') {

    zip.fname = unzip(file.name, list=T)$Name
    ind = which(tools::file_ext(zip.fname) == ivar.file.ext)
    zip.fname = zip.fname[ind]

    con <-unz(file.name, zip.fname)

  } else {
    pretty_stop(paste0('The following source data file format cannot be handled:',f.extn,'\n',
                       'Check input source data format within grid_sources().'))
  }

  # Only get file header data
  if (only.header) {
    headerData = readLines(con,n=6)
    #close(con)

    nCols =as.integer(sub('ncols', '', headerData[1]))
    nRows = as.integer(sub('nrows', '', headerData[2]));
    SWLong = as.numeric(sub('xllcenter', '', headerData[3]));
    SWLat = as.numeric(sub('yllcenter', '', headerData[4]));
    DPixel = as.numeric(sub('cellsize', '', headerData[5]));
    nodata = as.numeric(sub('nodata_value', '', headerData[6]));

    header.data =  list(nCols=nCols,nRows=nRows,SWLong=SWLong,SWLat=SWLat,DPixel=DPixel,nodata=nodata)

    return(header.data)
  }else {
    # Read grid data
    grd = matrix(
      scan(
        text = readLines(con, n = (6+nRows))[7:(nRows+6)],
        what = numeric(),
        quiet = TRUE,
        na.strings=noData),
      ncol = nCols,
      byrow = T)

    #close(con)

    return(grd)
  }
}


# Helper function to import ncdf4. Required by raster() package
ignore_unused_imports <- function(fname) {
  ncdf4::nc_version()
}
