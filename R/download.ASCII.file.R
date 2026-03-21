#' \code{download.ASCII.file} downloads grid data.
#'
#' This is an internal function. It downloads an ARCMAP ASCII grid file from a URL.
#'
#' @return
#' A list variable of the file name and succes/failure flag.
#'
#' @keywords internal
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
    didFail = 0
  },error = function(cond) {return(1)})

  # Unzip file
  if (didFail == 0) {
    OS <- Sys.info()
    OS <- OS[1]
    if (OS=='Windows') {
      if (file.exists(des.file.name) && didFail==0) {

        # Get list of files in the downloaded zip file
        # From: https://stackoverflow.com/questions/55355466/7z-list-only-filenames
        didFail = 1
        didFail = tryCatch({
          zip.fnames = system(paste0('7z l -ba "',des.file.name),intern = T)
          zip.fnames = grep("D....", zip.fnames, invert = TRUE, fixed = TRUE, value = TRUE)
          zip.fnames = sub("^.{53}", "", zip.fnames)
          didFail = 0
        },
        error = function(e) {
          return(1)
        }
        )

        # Unzip downloaded file
        if (didFail==0) {
          hasError = tryCatch({
            exitMessage = system(paste0('7z e -aoa -bso0 "',des.file.name, '"', ' -o', workingFolder),
                                 intern = T)
          },
          error = function(e) {
            return(1)
          }
          )
        }

        if (didFail==1) {
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
        if (ivar.url.ext == 'grid.Z') {
          system(paste('uncompress -f ',des.file.name))
          zip.fnames = gsub('.Z', '', des.file.name)

          # Remove path
          zip.fnames = basename(zip.fnames)

        } else if (ivar.url.ext == 'grid.zip') {
          # Get list of files in zip
          zip.fnames = system(paste0('unzip -l ',des.file.name), intern = T)
          zip.fnames = zip.fnames[-(1:3)]
          zip.fnames = zip.fnames[-length(zip.fnames)]
          zip.fnames = zip.fnames[-length(zip.fnames)]
          zip.fnames = unlist(strsplit(zip.fnames,'   '))
          zip.fnames = zip.fnames[ seq(2, length(zip.fnames), by=2)]

          # uncompress
          system(paste0('unzip -q ',des.file.name, ' -d ', workingFolder))

        } else {
          stop(paste('Unrecognised downloaded file compression extension:',ivar.url.ext))
        }
      }
    }
  }

  if (didFail==1)
    return(list(file.name='', didFail=1))

  # Remove zip file
  if (file.exists(des.file.name))
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
