#' \code{grid.read} reads in header or grid data.
#'
#' This is an internal function. It reads the downloaded boM compressed file and gets the
#' header info or the grid data (assumes an ARCMAP ASCII grid format).
#'
#' @return
#' List of header information or a matrix of the grid data.
#'
#' @keywords internal
#'
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
    pretty.stop(paste0('The following source data file format cannot be handled:',f.extn,'\n',
                'Check input source data format within grid.sources().'))
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
