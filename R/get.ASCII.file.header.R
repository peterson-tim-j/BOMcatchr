#' \code{get.ASCII.file.header} get header information.
#'
#' This is an internal function. It returns the header information within a ARCMAP ASCII grid file.
#'
#' @return
#' A list variable of header information.
#'
#' @keywords internal
get.ASCII.file.header <- function (des.file.name, ivar.file.ext, workingFolder, remove.file=T) {

  # Get data geometry for eachfile type
  OS <- Sys.info()
  OS <- OS[1]
  if (OS=='Windows') {
    raw<-textConnection(readLines(a<-file(des.file.name)))
  } else {
    if (ivar.file.ext=='grid')
      raw<-textConnection(readLines(a<-gzfile(des.file.name)))
    else
      raw<-textConnection(readLines(a<-file(des.file.name)))
  }

  # Get file header data
  headerData = readLines(raw,n=6)
  nCols =as.integer(sub('ncols', '', headerData[1]))
  nRows = as.integer(sub('nrows', '', headerData[2]));
  SWLong = as.numeric(sub('xllcenter', '', headerData[3]));
  SWLat = as.numeric(sub('yllcenter', '', headerData[4]));
  DPixel = as.numeric(sub('cellsize', '', headerData[5]));
  nodata = as.numeric(sub('nodata_value', '', headerData[6]));


  close(a)
  close(raw)
  if (remove.file) {
    didRemoveFile = tryCatch({file.remove(des.file.name)},finally=TRUE)
  }

  header.data =  list(nCols=nCols,nRows=nRows,SWLong=SWLong,SWLat=SWLat,DPixel=DPixel,nodata=nodata)
  return(header.data)
}
