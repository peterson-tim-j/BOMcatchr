#' Age of netCDF data and updates required.
#'
#' \code{grid.ages} age of the netCDF date and which time points likely have BoM updates.
#'
#' This function compares the download date of data within an existing netCDF file
#' and compares them against the BoM schedule of data updates (provided (see
#' \code{\link{grid.sources}}),
#' @param ncfile file name of the netCDF data file built by the package function \code{\link{grid.build}}.
#' @param data.src is a data.frame of the source grid information. Default is from \code{\link{grid.sources}}.
#' @param silent is a logical for silencing console outputs. Default is \code{F}.
#' @param plot.sourcedate is a logical for plotting the date that the data for each time point was downloaded. Default is \code{F}.
#' @param today is today's date. Default is \code{Sys.Date()}. This input is only provided
#' to demonstrate the function within the vignettes.
#' @return
#' list of data.frames, one for each variable in the netCDF file. Each data frame contains
#' the following columns:
#' \itemize{
#'  \item{\code{Date}: observation date,}
#'  \item{\code{Source.date}: date observation was downloaded from the BoM.}
#'  \item{\code{Source.age} : age of observation in days.}
#'  \item{\code{1 < age <= 7 } : logical if \code{Source.age} is greater than the left threshold and less than or equal to right duration threshold.}
#'  \item{\code{age <= 7 } : logical if \code{Source.age} is less than or equal to the maximum duration threshold.}
#' }
#' @seealso \code{\link{grid.sources}} for update threshold data.
#' @export
grid.ages <- function(ncfile,
                      data.src = grid.sources(),
                      silent = F,
                      plot.sourcedate = F,
                      today = Sys.Date()) {

  # Get summary of existing data
  data.existing = grid.summary(ncfile)

  # open netcdf file
  ncout <- RNetCDF::open.nc(ncfile, write=F)

  # Initialise output list
  summary.lst = list()

  for (ivar in rownames(data.existing)) {

    # Build vector of data dates and get netCDF index to the dates
    data.timepoints = get.ncdf.dates(data.existing[ivar,]$from,
                                     data.existing[ivar,]$to,
                                     data.existing[ivar,]$time.step)

    data.ind = get.ncdf.date.index(data.existing[ivar,]$time.datum,
                                   data.timepoints,
                                   ncdf.start = data.existing[ivar,]$from,
                                   ncdf.end = data.existing[ivar,]$to)

    # Get date data was downloaded
    grp = RNetCDF::grp.inq.nc(ncout, data.existing[ivar,]$group)$self

    data.soureDates = RNetCDF::var.get.nc(grp,
                                          paste0(ivar,'.sourceDate'),
                                          start=data.ind[1],
                                          count = length(data.ind))

    # Convert netCDF source dates to a date class
    data.soureDates = as.Date(as.character(data.soureDates),
                              format = "%Y%m%d")

    # Calc the age of each data ppint. Below, the result is compared against the threshold
    # ages when the source data is updated
    tot.age = ceiling(difftime(today, data.soureDates, units = 'days'))

    # Get threshold ages when BoM update source data grids.
    has.thresholds = F
    if (nchar(data.src[ivar,]$update.days)>0) {
      has.thresholds = T

      age.thresh.str = strsplit(data.src[ivar,]$update.days,',')[[1]]
      age.thresh = t(as.matrix(as.numeric(age.thresh.str)))

      nthres = length(age.thresh)
      if (nthres>1) {
        age.thresh.lower = cbind( matrix(1), age.thresh[1, 1:(nthres-1), drop=F])
      } else {
        age.thresh.lower = matrix(1)
      }
      age.thresh.upper = age.thresh


      # Calc if age of data exceeds BoM update threshold for the variable
      data.age.2update = outer(tot.age, age.thresh.lower[1,], ">") &
        outer(tot.age, age.thresh.upper[1,], "<=")
      data.age.2update = cbind(data.age.2update, rowSums(data.age.2update)>0)

      colnames(data.age.2update) = c( paste(age.thresh.lower, '< age <=',age.thresh.upper),
                                      paste('age <= ', max(age.thresh)))
    } else {
      data.age.2update = NA
    }

    # Convert source dates to a factor - since there will be only a few dates
    # data was downloaded.
    data.soureDates = as.factor(data.soureDates)

    # Build factor vector of source data dates
    summary.lst[[ivar]] = data.frame(Date = data.timepoints,
                                             Source.date = data.soureDates,
                                             Source.age = tot.age)

    # Add if data past age thresholds
    if (has.thresholds)
      summary.lst[[ivar]] = cbind(summary.lst[[ivar]], data.age.2update)

  }

  # close netcdf file
  RNetCDF::close.nc(ncout)

  # Report earliest update dates to user
  if (!silent) {
    message('Summary of source grid update dates for each variable:')
    for (ivar in rownames(data.existing)) {
        tmp.data = summary.lst[[ivar]]
        update.from = '(none)'
        if (ncol(tmp.data)>3) {
          ind = which(tmp.data[, ncol(tmp.data)], arr.ind = T)
          if (length(ind)>0)
            update.from = tmp.data[ min(ind), 1]
        }
        message( paste('   ',ivar,':',update.from))
    }
  }

  if (plot.sourcedate) {
    vars = names(summary.lst)

    # Y positions
    ypos <- 1:length(vars)
    names(ypos) <- vars

    # Get obs date range
    f.min = function(x){return(min(x$Date))}
    f.max = function(x){return(max(x$Date))}
    start.date = lapply(summary.lst, f.min)
    end.date = lapply(summary.lst, f.max)
    start.date = min(as.Date(unlist(start.date)))
    end.date = max(as.Date(unlist(end.date)))

    # Get list of unique source dates
    f.uniq =  function(x){return(unique(x$Source.date))}
    source.dates.opt = lapply(summary.lst, f.uniq)
    source.dates.opt = unique(as.Date(unlist(source.dates.opt)))

    # Get list of colors, one for each date
    cols = palette.colors(n=length(f.uniq),
                          palette="polychrome36",
                          recycle=T)
    names(cols) <- source.dates.opt

    # Expand bottom margin for legend
    mr = par()$mar
    mr[1] = 10
    mr[2] = 7.1
    mr[4] = 2.1
    par(mar=mr, xpd=T)

    # Empty plot
    plot(
      x = range(c(start.date, end.date)),
      y = c(0.5, length(vars)+0.5),
      type = "n",
      xaxt = "n",
      xlab = "Date",
      ylab = "",
      yaxt = "n"
    )

    # x-axis labels
    # 3. Add custom date axis line manually
    axis.Date(
      side = 1,
      format = "%Y-%m"
      )

    # Y-axis labels
    axis(
      side = 2,
      at = 1:length(vars),
      labels = vars,
      las = 1
    )

    # Draw rectangles
    for (ivar in vars) {

      data.tmp = summary.lst[[ivar]]

      data.rl = rle(as.character(data.tmp$Source.date))

      end_idx <- cumsum(data.rl$lengths)
      start_idx <- c(1, head(end_idx + 1, -1))

      periods <- data.frame(
        start = data.tmp$Date[start_idx],
        end   = data.tmp$Date[end_idx],
        source.date = data.rl$values
      )

      y <- ypos[ivar]

      for (irow in 1:nrow(periods)) {
        icol = cols[[ periods$source.date[irow] ]]

        rect(
          xleft   = periods$start[irow],
          xright  = periods$end[irow],
          ybottom = y - 0.4,
          ytop    = y + 0.4,
          col     = icol,
          border  = NA)

      }
    }

    legend(x='bottom',
           legend= names(cols),
           fill=cols,
           horiz = T,
           inset = c(0, -0.55),
           title = 'Source date')
  }

  return(summary.lst)
}
