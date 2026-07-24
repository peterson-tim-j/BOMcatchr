## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr)

## -----------------------------------------------------------------------------
date.from = as.Date("2010-01-01","%Y-%m-%d")
date.to = as.Date("2015-12-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
fname = grid_build(ncdfFilename = ncdfFilename,
                         updateFrom = date.from,
                         updateTo = date.to,
                         vars = c('precip', 'precip.RMSE', 'precip.monthly'))

## -----------------------------------------------------------------------------
catch <- extract_locations('catchment',c('407214','407220'))
site_ID <- catch$stationno

## -----------------------------------------------------------------------------
climateData.annual = extract_data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip', 'precip.monthly'),
                      locations = catch,
                      temporal.timestep = 'annual',
                      temporal.fn.inner = 'sum',
                      spatial.fn = 'var')

## -----------------------------------------------------------------------------
sqrd_sum <- function(x) {return(sum(x^2))}

climateData.annual.err = extract_data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip.RMSE'),
                      locations = catch,
                      temporal.timestep = 'annual',
                      temporal.fn.inner = sqrd_sum,
                      spatial.fn = 'var')

## -----------------------------------------------------------------------------
par(mfrow=c(2,1), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot the precipitation.
for (i in 1:length(site_ID)) {

  filt = climateData.annual$temporal$Location.ID == site_ID[i]

  # Plot precipitation from monthly data
  tmp.date = climateData.annual$temporal[filt,]
  x.data = tmp.date$year
  y.data = tmp.date$precip.monthly

  # Plot calendar year precipitation from daily data
  plot(x = x.data,
       y = y.data,
       type = "l",
       col = "red",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = c(0, ceiling(max(y.data) * 1.2)),
       ylab = "Precip. [mm/year]",
       xlab = "Calender year",
       xaxs = "r",
       bty = "l",
       yaxs = "r",
       main=paste('Catchment ID',site_ID[i]))

  # Get water year data from daily data
  tmp.date = climateData.annual$temporal[filt,]
  y.data = tmp.date$precip
  tmp.date = climateData.annual.err$temporal[filt,]
  y.data.low = y.data - 2*sqrt(tmp.date$precip.RMSE)
  y.data.hi = y.data + 2*sqrt(tmp.date$precip.RMSE)

  lines(x = x.data,
       y = y.data,
       type = "l",
       col = "blue",
       lwd = 1.2)

  # Add interpolation error bars for precipitation from daily data
  arrows(x0 = x.data,
         y0 = y.data.low,
         x1 = x.data,
         y1 = y.data.hi,
         angle=90,
         code=3,
         length=0.06,
         col="black")

  # Add legend
  legend("bottomleft",
         lwd = 2,
         bty = "n",
         inset = c(0.01, -0.01),
         lty = c(1, 1, 1), pch = c(NA, NA, NA),
         col = c("blue",  "black", "red"),
         legend = c("From daily data", "5-95 %ile error", "From monthly data"),
         xpd = NA,
         cex=0.75)
}

## -----------------------------------------------------------------------------
dates = seq.Date(date.from, date.to, by ='day')
wateryear.ind = which(as.numeric(format(dates, '%m')) == 3 & as.numeric(format(dates, '%d'))==1)

## -----------------------------------------------------------------------------
climateData.daily2wateryear = extract_data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip'),
                      locations = catch,
                      temporal.timestep = wateryear.ind,
                      temporal.fn.inner = 'sum',
                      spatial.fn = 'var')

## -----------------------------------------------------------------------------
climateData.daily2wateryear.err = extract_data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip.RMSE'),
                      locations = catch,
                      temporal.timestep = wateryear.ind,
                      temporal.fn.inner = sqrd_sum,
                      spatial.fn = 'var')

## -----------------------------------------------------------------------------
dates = seq.Date(date.from, date.to, by ='month')
wateryear.ind = which(as.numeric(format(dates, '%m')) == 3 & as.numeric(format(dates, '%d'))==1)

## -----------------------------------------------------------------------------
climateData.month2wateryear = extract_data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip.monthly'),
                      locations = catch,
                      temporal.timestep = wateryear.ind,
                      temporal.fn.inner = 'sum',
                      spatial.fn = 'var')

## -----------------------------------------------------------------------------
filt = climateData.daily2wateryear$temporal$days.per.timestep >= 365
climateData.daily2wateryear$temporal = climateData.daily2wateryear$temporal[filt, ]

filt = climateData.daily2wateryear.err$temporal$days.per.timestep >= 365
climateData.daily2wateryear.err$temporal = climateData.daily2wateryear.err$temporal[filt, ]

filt = climateData.month2wateryear$temporal$months.per.timestep == 12
climateData.month2wateryear$temporal = climateData.month2wateryear$temporal[filt, ]


## -----------------------------------------------------------------------------
par(mfrow=c(2,1), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot.
for (i in 1:length(site_ID)) {

  # Water years precipitation from monthly data.
  filt = climateData.month2wateryear$temporal$Location.ID == site_ID[i]
  tmp.date = climateData.month2wateryear$temporal[filt,]
  x.data = tmp.date$year
  y.data = tmp.date$precip.monthly

  plot(x = x.data,
       y = y.data,
       type = "l",
       col = "red",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = c(0, ceiling(max(y.data) * 1.2)),
       ylab = "Precip. [mm/year]",
       xlab = "Water year (end year)",
       xaxs = "r",
       bty = "l",
       yaxs = "r",
       main=paste('Catchment ID',site_ID[i]))

  # Get water year data from daily data.
  filt = climateData.daily2wateryear$temporal$Location.ID == site_ID[i]
  tmp.date = climateData.daily2wateryear$temporal[filt,]
  x.data = tmp.date$year
  y.data = tmp.date$precip
  tmp.date = climateData.daily2wateryear.err$temporal[filt,]
  y.data.low = y.data - 1.645 * sqrt(tmp.date$precip.RMSE)
  y.data.hi = y.data + 1.645 * sqrt(tmp.date$precip.RMSE)

  lines(x = x.data,
       y = y.data,
       type = "l",
       col = "blue",
       lwd = 1.2)

  # Add interpolation error bars for water years precipitation from daily data
  arrows(x0 = x.data,
         y0 = y.data.low,
         x1 = x.data,
         y1 = y.data.hi,
         angle=90,
         code=3,
         length=0.06,
         col="black")

  # Add legend
  legend("bottomleft",
         lwd = 2,
         bty = "n",
         inset = c(0.01, -0.01),
         lty = c(1, 1, 1), pch = c(NA, NA, NA),
         col = c("blue",  "black", "red"),
         legend = c("From daily data", "5-95 %ile error", "From monthly data"),
         xpd = NA,
         cex=0.75)
}

## -----------------------------------------------------------------------------
clim.seasonal = extract_data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip', 'precip.monthly'),
                      locations = catch,
                      temporal.timestep = 'seasonal',
                      temporal.fn.outer = 'clim',
                      temporal.fn.inner = 'sum',
                      spatial.fn = 'var')

## -----------------------------------------------------------------------------
par(mfrow=c(length(site_ID), 1),
    mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot.
for (i in 1:length(site_ID)) {

  # Water years precipitation from monthly data.
  filt = clim.seasonal$temporal$Location.ID == site_ID[i]
  tmp.date = clim.seasonal$temporal[filt,]
  x.data = as.Date(paste0(tmp.date$year,'-',
                          tmp.date$month,'-',
                          tmp.date$day))
  y.data = tmp.date$precip.monthly

  plot(x = x.data,
       y = y.data,
       type = "l",
       col = "red",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = c(0, ceiling(max(y.data) * 1.2)),
       ylab = "Precip. climatology [mm/season]",
       xlab = "Date",
       xaxs = "r",
       bty = "l",
       yaxs = "r",
       main=paste('Catchment ID',site_ID[i]))

  y.data = tmp.date$precip
  lines(x = x.data,
       y = y.data,
       type = "l",
       col = "blue",
       lwd = 1.2)

  # Add legend
  legend("bottomleft",
         lwd = 2,
         bty = "n",
         inset = c(0.01, -0.01),
         lty = c(1, 1), pch = c(NA, NA),
         col = c("blue", "red"),
         legend = c("From daily data", "From monthly data"),
         xpd = NA,
         cex=0.75)
}

## -----------------------------------------------------------------------------
annom.seasonal = extract_data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip', 'precip.monthly'),
                      locations = catch,
                      temporal.timestep = 'seasonal',
                      temporal.fn.outer = 'annom',
                      temporal.fn.inner = 'sum',
                      spatial.fn = 'var')

## -----------------------------------------------------------------------------
cumannom.seasonal = extract_data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip', 'precip.monthly'),
                      locations = catch,
                      temporal.timestep = 'seasonal',
                      temporal.fn.outer = 'cumannom',
                      temporal.fn.inner = 'sum',
                      spatial.fn = 'var')

## -----------------------------------------------------------------------------
par(mfrow=c(2,2), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot.
for (i in 1:length(site_ID)) {

  # Water years precipitation from monthly data.
  filt = annom.seasonal$temporal$Location.ID == site_ID[i]
  tmp.date = annom.seasonal$temporal[filt,]
  x.data = as.Date(paste0(tmp.date$year,'-',
                          tmp.date$month,'-',
                          tmp.date$day))
  y.data = tmp.date$precip.monthly

  shared_limits <- range(c(y.data, tmp.date$precip))

  plot(x = x.data,
       y = y.data,
       type = "l",
       col = "red",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = shared_limits,
       ylab = "Precip. annomoly [mm/season]",
       xlab = "Date",
       xaxs = "r",
       bty = "l",
       yaxs = "r",
       main=paste('Catchment ID',site_ID[i]))

  y.data = tmp.date$precip
  lines(x = x.data,
       y = y.data,
       type = "l",
       col = "blue",
       lwd = 1.2)

  lines(range(x.data), c(0, 0), col='grey', lty = "dashed")

    # Add legend
  legend("topright",
         lwd = 2,
         bty = "n",
         inset = c(0.01, -0.01),
         lty = c(1, 1), pch = c(NA, NA),
         col = c("blue", "red"),
         legend = c("From daily data", "From monthly data"),
         xpd = NA,
         cex=0.75)

  tmp.date = cumannom.seasonal$temporal[filt,]
  y.data = tmp.date$precip.monthly
  shared_limits <- range(c(y.data, tmp.date$precip))
  plot(x = x.data,
       y = y.data,
       type = "l",
       col = "red",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = shared_limits,
       ylab = "Precip. cum. annomoly [mm/season]",
       xlab = "Date",
       xaxs = "r",
       bty = "l",
       yaxs = "r",
       main=paste('Catchment ID', site_ID[i]))

  y.data = tmp.date$precip
  lines(x = x.data,
       y = y.data,
       type = "l",
       col = "blue",
       lwd = 1.2)

  lines(range(x.data), c(0, 0), col='grey', lty = "dashed")
}

## -----------------------------------------------------------------------------
runlength_precip <- function(x) {
  # Precip events >1 mm/day
  ind <- as.vector(x >=1)

  # Do run length analysis
  r <- rle(ind)

  # Return median event run length
  return(mean(r$lengths))
}

## -----------------------------------------------------------------------------
dates = seq.Date(date.from, date.to, by ='day')
wateryear.ind = which(as.numeric(format(dates, '%m')) == 3 & as.numeric(format(dates, '%d'))==1)

rle.wateryear <- extract_data(ncdfFilename = ncdfFilename,
             extractFrom = date.from,
             extractTo = date.to,
             vars = c('precip'),
             locations = catch,
             temporal.timestep = wateryear.ind,
             temporal.fn.outer = NA,
             temporal.fn.inner = runlength_precip,
             spatial.fn = 'var')

## -----------------------------------------------------------------------------
par(mfrow=c(length(site_ID), 1),
    mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot.
for (i in 1:length(site_ID)) {
  filt = rle.wateryear$temporal$Location.ID == site_ID[i]
  tmp.date = rle.wateryear$temporal[filt,]
  x.data = as.Date(paste0(tmp.date$year,'-',
                          tmp.date$month,'-',
                          tmp.date$day))
  y.data = tmp.date$precip

  plot(x = x.data,
       y = y.data,
       type = "l",
       col = "red",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylab = "Mean precip. event duration  [days]",
       xlab = "Date",
       xaxs = "r",
       bty = "l",
       yaxs = "r",
       main=paste('Catchment ID',site_ID[i]))

}

