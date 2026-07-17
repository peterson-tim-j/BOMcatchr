#' \code{.grid_download} downloads grid data.
#'
#' This is an internal function. It downloads an ARCMAP ASCII grid file from a URL.
#'
#' @return
#' A list variable of the file name and succes/failure flag.
#'
#' @keywords internal
#' @noRd
.grid_download <- function (url.string, ivar.url.ext, ivar.file.ext, ivar.timestep, data.type.label,  workingFolder, datestring) {

  if (!is.character(url.string))
    .pretty_stop(paste('The input URL for',data.type.label,'must be a URL string.'))

  if (!startsWith(url.string,'https://'))
    .pretty_stop(paste('The input URL string for',data.type.label,'must start "with https://" '))

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
    .pretty_stop(paste('Unknown source data time step:',ivar.timestep))
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

#' \code{.grid_read} reads in header or grid data.
#'
#' This is an internal function. It reads the downloaded boM compressed file and gets the
#' header info or the grid data (assumes an ARCMAP ASCII grid format).
#'
#' @return
#' List of header information or a matrix of the grid data.
#'
#' @keywords internal
#' @noRd
.grid_read <- function(file.name, ivar.file.ext, only.header, nRows, nCols, noData) {

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
    .pretty_stop(paste0('The following source data file format cannot be handled:',f.extn,'\n',
                       'Check input source data format within grid_sources().'))
  }

  # Only get file header data
  if (only.header) {
    nheaders = 6
    headerData = readLines(con,n = nheaders)

    headerData_df = data.frame(lbl = character(), val = numeric())
    for (i in 1:nheaders) {
      tmp = strsplit(headerData[i], ' ')[[1]]
      headerData_df[i, 'lbl'] = tolower(tmp[1])
      headerData_df[i, 'val'] = as.numeric(tmp[2])
    }

    # Displace datum long lat IF coordinates are from the
    # lower left of a cell and not the centre.
    if (any(grepl("llcorner", headerData_df[ , 'lbl'], ignore.case = TRUE))) {
      ind = headerData_df[i, 'lbl'] == 'cellsize'

      if (length(ind)==0)
        .pretty_stop('Unexpected error. ASCII raster grid does not contain "cellsize" field.')

      dxdy = headerData_df[ind, 'val'] / 2

      ind = headerData_df[i, 'lbl'] == 'xllcorner'
      headerData_df[ind, 'val'] = headerData_df[ind, 'val'] + dxdy
      headerData_df[ind, 'lbl'] = 'xllcenter'

      ind = headerData_df[i, 'lbl'] == 'yllcorner'
      headerData_df[ind, 'val'] = headerData_df[ind, 'val'] + dxdy
      headerData_df[ind, 'lbl'] = 'yllcenter'
    }

    # Check required columns exist
    cols_req = c('ncols', 'nrows', 'xllcenter', 'yllcenter', 'cellsize', 'nodata_value')
    if (!(all(cols_req %in%  headerData_df[ , 'lbl'])))
      .pretty_stop(cat('Unexpected error. ASCII raster grid doesnt not contain the following required fields:',cols_req))

    headerData = headerData_df[, 'val']
    names(headerData) = headerData_df[, 'lbl']
    header.data =  list(nCols = headerData['ncols'],
                        nRows = headerData['nrows'],
                        SWLong = headerData['xllcenter'],
                        SWLat = headerData['yllcenter'],
                        DPixel = headerData['cellsize'],
                        nodata = headerData['nodata_value'])

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
.ignore_unused_imports <- function(fname) {
  ncdf4::nc_version()
}

# Extract point data from ras grid
.extract_point_data = function(r, interp.method, coords, do.infill, ext, interpMax) {

  # Do infilling of NAs. Generally only included for gaos in solar radiation.
  if (do.infill) {
    # crop to extent
    r <- terra::crop(r, ext, snap = "out")

    # Infill NA values of grid by taking the local average. Only do so
    # if there are some finite values. The maximum area of NAs that is
    # infilled is defined by interpMax. That is a value of 3 infills a
    # 3x3 cell area.
    if (terra::global(r, fun = "anynotNA")[,1]) {
      i = 0
      while (terra::global(r, fun = "anyNA")[,1] && i<interpMax) {
        r <- terra::focal(r, w=matrix(1,3,3), fun=mean, na.rm=TRUE, na.policy='only')
        i = i +1
      }
    }
  }

  return(terra::extract(r, coords, method=interp.method)[[1]])
}

.pretty_stop <- function(str) {
  stop(str, call. = FALSE)
}
