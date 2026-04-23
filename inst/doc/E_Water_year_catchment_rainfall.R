## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(AWAPer, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
date.from = as.Date("2010-01-01","%Y-%m-%d")
date.to = as.Date("2015-12-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
fname = makeNetCDF_file(ncdfFilename = ncdfFilename,
                         updateFrom = date.from,
                         updateTo = date.to,
                         vars = c('precip', 'precip.RMSE', 'precip.monthly'))

## -----------------------------------------------------------------------------
data("catchments")

## -----------------------------------------------------------------------------
climateData.annual = extractCatchmentData(ncdfFilename=ncdfFilename,
                      extractFrom=date.from,
                      extractTo=date.to,
                      vars = c('precip', 'precip.monthly'),
                      locations=catchments,
                      temporal.timestep = 'annual',
                      temporal.function.name='sum',
                      spatial.function.name='var')

## -----------------------------------------------------------------------------
sqrd.sum <- function(x) {return(sum(x^2))}

climateData.annual.err = extractCatchmentData(ncdfFilename=ncdfFilename,
                      extractFrom=date.from,
                      extractTo=date.to,
                      vars = c('precip.RMSE'),
                      locations=catchments,
                      temporal.timestep = 'annual',
                      temporal.function.name = sqrd.sum,
                      spatial.function.name = 'var')

## -----------------------------------------------------------------------------
par(mfrow=c(2,1), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot the daily precipitation and PET.
for (i in 1:length(catchments$CatchID)) {

  filt = climateData.annual$temporal.sum$Location.ID == catchments$CatchID[i]

  # Get water year data from daily data
  tmp.date = climateData.annual$temporal.sum[filt,]
  x.data = as.Date(ISOdate(tmp.date$year, tmp.date$month, tmp.date$day))
  y.data = tmp.date$precip
  tmp.date = climateData.annual.err$temporal.sqrd.sum[filt,]
  y.data.low = y.data - 2*sqrt(tmp.date$precip.RMSE)
  y.data.hi = y.data + 2*sqrt(tmp.date$precip.RMSE)

  # Plot calendar year precipitation from daily data
  plot(x = x.data,
       y = y.data,
       type = "l",
       col = "blue",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = c(0, ceiling(max(y.data.hi))),
       ylab = "Precip. [mm/year]",
       xlab = "Calender year",
       xaxs = "i",
       bty = "l",
       yaxs = "i",
       main=paste('Catchment ID',catchments$CatchID[i]))

  # Add interpolation error bars for water years precipitation from daily data
  arrows(x0 = x.data,
         y0 = y.data.low,
         x1 = x.data,
         y1 = y.data.hi,
         angle=90,
         code=3,
         length=0.06,
         col="black")

  # Plot precipitation from monthly data
  y.data = tmp.date$precip.monthly

  lines(x = x.data,
       y = y.data,
       type = "l",
       col = "red",
       lwd = 1.2)

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
climateData.daily2wateryear = extractCatchmentData(ncdfFilename=ncdfFilename,
                      extractFrom=date.from,
                      extractTo=date.to,
                      vars = c('precip'),
                      locations=catchments,
                      temporal.timestep = wateryear.ind,
                      temporal.function.name='sum',
                      spatial.function.name='var')

## -----------------------------------------------------------------------------
climateData.daily2wateryear.err = extractCatchmentData(ncdfFilename=ncdfFilename,
                      extractFrom=date.from,
                      extractTo=date.to,
                      vars = c('precip.RMSE'),
                      locations=catchments,
                      temporal.timestep = wateryear.ind,
                      temporal.function.name = sqrd.sum,
                      spatial.function.name = 'var')

## -----------------------------------------------------------------------------
dates = seq.Date(date.from, date.to, by ='month')
wateryear.ind = which(as.numeric(format(dates, '%m')) == 3 & as.numeric(format(dates, '%d'))==1)

## -----------------------------------------------------------------------------
climateData.month2wateryear = extractCatchmentData(ncdfFilename=ncdfFilename,
                      extractFrom=date.from,
                      extractTo=date.to,
                      vars = c('precip.monthly'),
                      locations=catchments,
                      temporal.timestep = wateryear.ind,
                      temporal.function.name='sum',
                      spatial.function.name='var')

## -----------------------------------------------------------------------------
filt = climateData.daily2wateryear$temporal.sum$days.per.timestep >= 365
climateData.daily2wateryear$temporal.sum = climateData.daily2wateryear$temporal.sum[filt, ]

filt = climateData.daily2wateryear.err$temporal.sqrd.sum$days.per.timestep >= 365
climateData.daily2wateryear.err$temporal.sqrd.sum = climateData.daily2wateryear.err$temporal.sqrd.sum[filt, ]

filt = climateData.month2wateryear$temporal.sum$days.per.timestep == 12
climateData.month2wateryear$temporal.sum = climateData.month2wateryear$temporal.sum[filt, ]


## -----------------------------------------------------------------------------
par(mfrow=c(2,1), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot the daily precipitation and PET.
for (i in 1:length(catchments$CatchID)) {

  # Get water year data from daily data.
  filt = climateData.daily2wateryear$temporal.sum$Location.ID == catchments$CatchID[i]
  tmp.date = climateData.daily2wateryear$temporal.sum[filt,]
  x.data = as.Date(ISOdate(tmp.date$year, tmp.date$month, tmp.date$day))
  y.data = tmp.date$precip
  tmp.date = climateData.daily2wateryear.err$temporal.sqrd.sum[filt,]
  y.data.low = y.data - 1.645 * sqrt(tmp.date$precip.RMSE)
  y.data.hi = y.data + 1.645 * sqrt(tmp.date$precip.RMSE)

  # plot water years precipitation from daily data
  plot(x = x.data,
       y = y.data,
       type = "l",
       col = "blue",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = c(0, ceiling(max(y.data.hi))),
       ylab = "Precip. [mm/year]",
       xlab = "Water year",
       xaxs = "i",
       bty = "l",
       yaxs = "i",
       main=paste('Catchment ID',catchments$CatchID[i]))

  # Add interpolation error bars for water years precipitation from daily data
  arrows(x0 = x.data,
         y0 = y.data.low,
         x1 = x.data,
         y1 = y.data.hi,
         angle=90,
         code=3,
         length=0.06,
         col="lightblue")

  # Water years precipitation from monthly data.
  filt = climateData.month2wateryear$temporal.sum$Location.ID == catchments$CatchID[i]
  tmp.date = climateData.month2wateryear$temporal.sum[filt,]
  x.data = as.Date(ISOdate(tmp.date$year, tmp.date$month, tmp.date$day))
  y.data = tmp.date$precip.monthly

  lines(x = x.data,
       y = y.data,
       type = "l",
       col = "red",
       lwd = 1.2)

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

