#' \code{grid.download} downloads grid data.
#'
#' This is an internal function. It downloads an ARCMAP ASCII grid file from a URL.
#'
#' @return
#' A list variable of the file name and succes/failure flag.
#'
#' @keywords internal
grid.download <- function (url.string, ivar.url.ext, ivar.file.ext, ivar.timestep, data.type.label,  workingFolder, datestring) {

  if (!is.character(url.string))
    pretty.stop(paste('The input URL for',data.type.label,'must be a URL string.'))

  if (!startsWith(url.string,'https://'))
    pretty.stop(paste('The input URL string for',data.type.label,'must start "with https://" '))

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
    pretty.stop(paste('Unknown source data time step:',ivar.timestep))
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
