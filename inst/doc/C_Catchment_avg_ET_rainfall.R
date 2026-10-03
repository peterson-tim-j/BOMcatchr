## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr)

## -----------------------------------------------------------------------------
date.from = as.Date("2010-07-01","%Y-%m-%d")
date.to = as.Date("2010-12-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
status = grid_build(ncdfFilename = ncdfFilename,
                         updateFrom = date.from,
                         updateTo = date.to,
                         vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad'))

## -----------------------------------------------------------------------------
catch <- extract_locations('catchment',c('407214','407220'))
site_ID <- catch$stationno

## -----------------------------------------------------------------------------
dr <- extract_locations('river_region',43637156)
riv <- extract_locations('river_simple',NA)
gauge <- extract_locations('water_gauge',NA)

## -----------------------------------------------------------------------------
gauge <- gauge[dr]
riv <- riv[dr]

## ----class.source = 'fold-hide'-----------------------------------------------
terra::plot(dr, border='black')
terra::plot(catch, border='red', add = TRUE)
terra::plot(riv, col='blue', add = TRUE)
terra::plot(gauge, col='grey', add = TRUE)

## -----------------------------------------------------------------------------
data(constants,package='Evapotranspiration')

## -----------------------------------------------------------------------------
climateData.daily = extract_data(ncdfFilename=ncdfFilename,
                      extractFrom=date.from,
                      extractTo=date.to,
                      vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                      locations=catch,
                      temporal.timestep = 'daily',
                      temporal.fn.inner ='sum',
                      spatial.fn = 'var',
                      ET.function='ET.MortonCRAE',
                      ET.timestep = 'monthly',
                      ET.Mortons.est='wet areal ET',
                      ET.constants= constants)

## ----class.source = 'fold-hide'-----------------------------------------------
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

## -----------------------------------------------------------------------------
PrecipData.monthly = extract_data(ncdfFilename=ncdfFilename,
                     extractFrom = date.from,
                     extractTo = date.to,
                     vars = c('precip'),
                     locations=catch,
                     spatial.fn = '',
                     temporal.timestep = 'monthly',
                     temporal.fn.inner = 'sum')

## ----class.source = 'fold-hide'-----------------------------------------------
panel.maps <- function(r, v, plot.labels, colourbar.label='') {

  n.plots = terra::nlyr(r)
  n.plotrows = ceiling(n.plots/2)
  layout(
    matrix(c(1:n.plots, n.plots+1, n.plots+1),
           nrow = n.plotrows+1, byrow = TRUE),
    heights = c( rep(0.9/n.plotrows, n.plotrows), 0.1)
  )

  zlim <- range(terra::values(r), na.rm = TRUE)
  n_colors <- 100
  pal = hcl.colors(n_colors, "viridis")

  for (i in 1:n.plots) {
    terra::plot(r[[i]],
               zlim = zlim,
               main = plot.labels[i],
               loc.main = 'topright',
               box=T,
               legend = FALSE)
    terra::plot(v, add = TRUE, border = "red", lwd = 2)
  }

  color_matrix = matrix(1:n_colors,
                        nrow = n_colors,
                        ncol = 1)
  par(mar = c(3, 1, 1, 1))
  image(x = seq(zlim[1], zlim[2], length.out = n_colors + 1),
        y    = c(0, 1),
        z    = color_matrix,
        col  = pal,
        axes = FALSE,
        xlab = "",
        ylab = ""
    )

    axis(1, at = pretty(seq(zlim[1], zlim[2], length.out = 5)), las = 1)
    mtext(colourbar.label, side = 1, line = 2, cex = 0.9)
}

## ----class.source = 'fold-hide'-----------------------------------------------
map.labels = gsub( 'precip_', '',names(PrecipData.monthly[[2:5]]))
map.labels = gsub( '_', '-',map.labels)

plot.data = PrecipData.monthly[[-1]]

panel.maps(plot.data, catch, map.labels, "Precip. (mm/month)")

## -----------------------------------------------------------------------------
metData.monthly = extract_data(ncdfFilename=ncdfFilename,
                     extractFrom=date.from,
                     extractTo=date.to,
                     vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                     locations=catch,
                     spatial.fn = '',
                     temporal.timestep = 'monthly',
                     temporal.fn.inner = 'sum',
                     ET.function = 'ET.MortonCRAE',
                     ET.timestep = 'monthly',
                     ET.Mortons.est='wet areal ET',
                     ET.constants= constants)

## ----class.source = 'fold-hide'-----------------------------------------------
colInd = which(startsWith(names(metData.monthly), "et_"))
plot.data = metData.monthly[[colInd]]

panel.maps(plot.data, catch, map.labels, "PET (mm/month)")

## ----class.source = 'fold-hide'-----------------------------------------------
colnames.all = names(metData.monthly)
colInd.P = which(startsWith(colnames.all, "precip_"))
colInd.PET = which(startsWith(colnames.all, "et_"))

for (i in 1:length(colInd.P)) {
  ind.P = colInd.P[i]
  ind.PET = colInd.PET[i]
  colname.P = colnames.all[ ind.P ]
  colname.tmp = sub('precip_','Deficit_mm_',colname.P)
  metData.monthly[[colname.tmp]] = metData.monthly[[ind.P]] - metData.monthly[[ind.PET]]
}

colInd = which(startsWith(names(metData.monthly), "Deficit_mm_"))
plot.data = metData.monthly[[colInd]]

panel.maps(plot.data, catch, map.labels, "P - PET (mm/month)")

## -----------------------------------------------------------------------------
centroid = terra::centroids(catch)

## -----------------------------------------------------------------------------
metData.monthly.weighted = extract_data(ncdfFilename=ncdfFilename,
                     extractFrom = date.from,
                     extractTo=date.to,
                     vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                     locations = catch,
                     spatial.fn = 'var',
                     temporal.timestep = 'monthly',
                     temporal.fn.inner = 'sum',
                     ET.function = 'ET.MortonCRAE',
                     ET.timestep = 'monthly',
                     ET.Mortons.est = 'wet areal ET',
                     ET.constants =  constants)

metData.monthly.centroid = extract_data(ncdfFilename=ncdfFilename,
                     extractFrom = date.from,
                     extractTo = date.to,
                     vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                     locations = centroid,
                     temporal.timestep = 'monthly',
                     temporal.fn.inner = 'sum',
                     ET.function='ET.MortonCRAE',
                     ET.timestep = 'monthly',
                     ET.Mortons.est = 'wet areal ET',
                     ET.constants =  constants)

## ----class.source = 'fold-hide'-----------------------------------------------
par(mfrow=c(2,3), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot the daily precipitation and PET.
for (i in 1:length(site_ID)) {

  filt = metData.monthly.weighted$temporal$Location.ID == site_ID[i]

  # Convert year, month and day columns from extractions to a date.
  metData.date = as.Date(paste0(metData.monthly.weighted$temporal$year[filt], "-",
       metData.monthly.weighted$temporal$month[filt], "-",
       metData.monthly.weighted$temporal$day[filt]))

  # Precipitation
  xdata = metData.date
  ydata_1 = metData.monthly.weighted$temporal$precip[filt]
  ydata_2 = metData.monthly.centroid$precip[filt]
  shared_limits <- range(c(ydata_1, ydata_2))
  shared_limits[1] = floor(shared_limits[1])
  shared_limits[2] = ceiling(shared_limits[2]*1.25)
  plot(x = xdata,
       y = ydata_1,
       type = "l",
       col = "#e31a1c",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = shared_limits,
       ylab = "Precip. [mm/month]",
       xlab = "2010",
       xaxs = "i",
       bty = "l",
       yaxs = "i",
       main=paste('Catchment ID',site_ID[i],': Precip.' ),
       )
  lines(x = xdata,
        y = ydata_2,
        col = "#bc80bd",
        lwd = 1.2)

  legend("bottomleft",
         lwd = 2,
         bty = "n",
         inset = c(0.01, -0.01),
         lty = c(1, 1), pch = c(NA, NA),
         col = c("#e31a1c",  "#bc80bd"),
         legend = c("Areal weighted", "Centroid"),
         xpd = NA)

  rmse = round(sqrt(mean((ydata_1 - ydata_2)^2)), 2)
  bias = round(mean(ydata_1 - ydata_2), 1)
  text(x=(xdata[1] + 5), y=shared_limits[2]*0.85, adj = c(0,0),
       labels = paste('RMSE =', rmse, 'mm/month\nBias =', bias, 'mm/month'))

  # PET
  xdata = metData.date
  ydata_1 = metData.monthly.weighted$temporal$et[filt]
  ydata_2 = metData.monthly.centroid$et[filt]
  shared_limits <- range(c(ydata_1, ydata_2))
  shared_limits[1] = floor(shared_limits[1])
  shared_limits[2] = ceiling(shared_limits[2]*1.25)
  plot(x = xdata,
       y = ydata_1,
       type = "l",
       col = "#e31a1c",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       main=paste('Catchment ID',site_ID[i],': PET'),
       ylim = shared_limits,
       ylab = "PET [mm/month]",
       xlab = "2010", xaxs = "i",
       bty = "l",
       yaxs = "i")

  lines(x = xdata,
       y = ydata_2,
        col = "#bc80bd", lwd = 1.2)

  rmse = round(sqrt(mean((ydata_1 - ydata_2)^2)), 2)
  bias = round(mean(ydata_1 - ydata_2), 1)
  text(x=(xdata[1] + 5), y=shared_limits[2]*0.85, adj = c(0,0),
       labels = paste('RMSE =', rmse, 'mm/month\nBias =', bias, 'mm/month'))

  # Deficit
  xdata = metData.date
  ydata_1 = metData.monthly.weighted$temporal$precip[filt] - metData.monthly.weighted$temporal$et[filt]
  ydata_2 = metData.monthly.centroid$precip[filt] - metData.monthly.centroid$et[filt]
  shared_limits <- range(c(ydata_1, ydata_2))
  shared_limits[1] = floor(shared_limits[1])
  shared_limits[2] = ceiling(shared_limits[2]*1.35)
  plot(x = xdata,
       y = ydata_1,
       type = "l",
       col = "#e31a1c",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = shared_limits,
       ylab = "P - PET [mm/month]",
       xlab = "2010",
       xaxs = "i",
       bty = "l",
       yaxs = "i",
       main=paste('Catchment ID',site_ID[i],': Deficit')
       )

  lines(xdata, ydata_2, col = "#bc80bd", lwd = 1.2)

  rmse = round(sqrt(mean((ydata_1 - ydata_2)^2)), 2)
  bias = round(mean(ydata_1 - ydata_2), 1)
  text(x=(xdata[1] + 5), y=shared_limits[2]*0.8, adj = c(0,0),
       labels = paste('RMSE =', rmse, 'mm/month\nBias =', bias, 'mm/month'))
}

