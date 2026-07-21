## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr)

## -----------------------------------------------------------------------------
date.from = as.Date("2010-07-01","%Y-%m-%d")
date.to = as.Date("2010-10-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
fname = grid_build(ncdfFilename = ncdfFilename,
                         updateFrom = date.from,
                         updateTo = date.to,
                         vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad'))

## -----------------------------------------------------------------------------
catch <- extract_locations('catchment',c('407214','407220'))
site_ID <- catch$stationno

## -----------------------------------------------------------------------------
data(constants,package='Evapotranspiration')

## -----------------------------------------------------------------------------
climateData.daily = extract_data(ncdfFilename=ncdfFilename,
                      extractFrom=date.from,
                      extractTo=date.to,
                      vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                      locations=catch,
                      temporal.timestep = 'daily',
                      temporal.fn.inner ='sum',
                      spatial.fn = 'var',
                      ET.function='ET.MortonCRAE',
                      ET.timestep = 'monthly',
                      ET.Mortons.est='wet areal ET',
                      ET.constants= constants)

## -----------------------------------------------------------------------------
par(mfrow=c(2,1), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot the daily precipitation and PET.
for (i in 1:length(site_ID)) {

  filt = climateData.daily$temporal$Location.ID == site_ID[i]

  # Convert year, month and day columns from extractions to a date.
  climateData.daily.date = as.Date(paste0(climateData.daily$temporal$year[filt], "-",
                           climateData.daily$temporal$month[filt], "-",
                           climateData.daily$temporal$day[filt]))


  # Plot precipitation and standard deviation against observations
  # ---------------------------------------------------------
  max.y = max(climateData.daily$temporal$precip[filt] +
        sqrt(climateData.daily$spatial$precip[filt]))

  # Precipitation
  plot(climateData.daily.date,
        climateData.daily$temporal$precip[filt],
        type = "h",
        col = "#e31a1c",
        lwd = 3,
        mgp = c(2, 0.5, 0),
        main=paste('Catchment ID',site_ID[i]),
        ylim = c(0, 80),
        ylab = "",
        xlab = "2010",
        xaxs = "i",
        yaxt = "n",
        bty = "l",
        yaxs = "i")

  axis(side = 2, mgp = c(2, 0.5, 0), line = 0.5, at = seq(from = 0, to = 80, by = 20),
        labels = c("0", "20", "40", "60", "80mm"), col = "#e31a1c", col.axis = "#e31a1c")

  # Standard deviation
  for (j in 1:length(climateData.daily.date)) {
    x.plot = rep(climateData.daily.date[j], 2)
    y.plot = c(climateData.daily$temporal$precip[filt][j] +
        sqrt(climateData.daily$spatial$precip[filt][j]),
        climateData.daily$temporal$precip[filt][j] -
        sqrt(climateData.daily$spatial$precip[filt][j]))
    lines(x.plot, y.plot, col = "black", lwd = 1.2)
  }

  # Plot evap data.
  par(new = TRUE)
  plot(climateData.daily.date,
       climateData.daily$temporal$et[filt],
       col = "#bc80bd",
       lwd = 2,
       ylab = "",
       ylim = c(0, 4),
       lty = 1,
       xlab = "",
       xaxs = "i",
       yaxt = "n",
       xaxt = "n",
       type = "l",
       bty = "n",
       yaxs = "i")

  axis(side = 2, line = 2.3, mgp = c(2, 0.5, 0), labels = c("0", "1", "2", "3", "4mm"),
      at = seq(from = 0, to = 4, by = 1), col = "#bc80bd", col.axis = "#bc80bd")

  legend("topleft",
         cex = 0.8,
         lwd = 2,
         bty = "n",
         inset = c(0.01, -0.01),
         lty = c(1, 1),
         pch = c(NA, NA),
         col = c("#e31a1c",  "#bc80bd"),
         legend = c("Precipitation (bars +/- one standard dev.)", "Morton CRAE PET"), xpd = NA)
}

