#' \code{download.ASCII.file} downloads grid data.
#'
#' This is an internal function. It downloads an ARCMAP ASCII grid file from a URL.
#'
#' @return
#' A list variable of the file name and succes/failure flag.
#'
#' @keywords internal
#'
#' @export
download.ASCII.file <- function (url.string, ivar.url.ext, ivar.file.ext, ivar.timestep, data.type.label,  workingFolder, datestring) {

  if (!is.character(url.string))
    stop(paste('The input URL for',data.type.label,'must be a URL string.'))

  if (!startsWith(url.string,'https://'))
    stop(paste('The input URL string for',data.type.label,'must start "with https://" '))

  if (ivar.timestep == 'days') {
    sdate = datestring
    edate = datestring
  } else if (ivar.timestep == 'months') {
    sdate = format( as.Date(datestring,'%Y%m%d'),"%Y%m01")
    edate = format(as.Date(format(as.Date(datestring,'%Y%m%d') + 33,"%Y%m01"),'%Y%m%d')-1,'%Y%m%d')
  } else if (ivar.timestep == 'years') {
    sdate = format( as.Date(datestring,'%Y%m%d'),"%Y0101")
    edate = format(as.Date(datestring,'%Y%m%d') ,"%Y1231")
  } else {
    stop(paste('Unknown source data time step:',ivar.timestep))
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
  },error = function(cond) {return(TRUE)})

  # Unzip file
  OS <- Sys.info()
  OS <- OS[1]
  if (OS=='Windows') {
      if (file.exists(des.file.name) && didFail==0) {

        # Get list of files in the downloaded zip file
        # From: https://stackoverflow.com/questions/55355466/7z-list-only-filenames
        hasError = F
        tryCatch( exp = {
            zip.fnames = system(paste0('7z l -ba "',des.file.name),intern = T)
            zip.fnames = grep("D....", zip.fnames, invert = TRUE, fixed = TRUE, value = TRUE)
            zip.fnames = sub("^.{53}", "", zip.fnames)
          },
          error = function(e) {
            hasError = T
            }
        )

        # Unzip downloaded file
        tryCatch( exp = {
          exitMessage = system(paste0('7z e -aoa -bso0 "',des.file.name, '"', ' -o', workingFolder),
                               intern = T)
        },
        error = function(e) {
          hasError = T
          }
        )

      if (hasError) {
        message('------------------------------------------------------------------------------------')
        message('The program "7z" is either not installed or cannot be found. If not installed then')
        message('install it from https://www.7-zip.org/download.html .')
        message('Once installed, do the following step:')
        message('  1. Click "Search Windows", search "Edit environmental variables for your account" and click on it.')
        message('  2. In the "User variables" window, select the "Path", and click "Edit..."')
        message('  3. In the "Edit environmental variable" window, click "New".')
        message('  4. Paste the path to the 7zip application folder, and click OK.')
        message('  5. Restart Windows.')
        message('  6. Open the "Command Prompt" and enter the command "7z".')
        message('     If setup correctly, this should output details such as the version, descriptions of commands, etc.')
        message('------------------------------------------------------------------------------------')
        stop()
      }
    }
  } else {
    if (file.exists(des.file.name) && didFail==0) {
      # Get list of files in the downloaded zip file
      zip.fnames = system(paste('znew -v ',des.file.name));

      # Unzip downloaded file
      system(paste('znew -f ',des.file.name));
    }
  }

  # Remove zip file
  file.remove(des.file.name)

  # Find new file of the required format and delete the other file
  ind = grepl(paste0(ivar.file.ext,'$'), zip.fnames)
  if (!any(ind))
    stop(paste('The following file format was not found within the zip file:',ivar.url.ext))
  des.file.name = zip.fnames[ind]
  des.file.name = file.path(workingFolder, des.file.name)

  # Delete the file not set as required
  file.remove( file.path(workingFolder, zip.fnames[!ind]) )

  return(list(file.name=des.file.name, didFail=didFail))
}
