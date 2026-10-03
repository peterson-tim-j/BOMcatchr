## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr)

## -----------------------------------------------------------------------------
dataFrom = as.Date("2010-01-01","%Y-%m-%d")
dataTo = as.Date("2010-12-31","%Y-%m-%d")

## -----------------------------------------------------------------------------
ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
status = grid_build(ncdfFilename = ncdfFilename,
                         updateFrom = dataFrom,
                         updateTo = dataTo,
                         vars = c('tmax', 'precip', 'precip.monthly'))

## -----------------------------------------------------------------------------
data("weather_gauge_loc")

coords = terra::vect(weather_gauge_loc,
                     crs = 'EPSG:4283',
                     geom = c("longitude", "latitude")
                     )

n_sites = nrow(coords)

## -----------------------------------------------------------------------------
st = extract_locations('state',id=NA)

terra::plot(st)
terra::plot(coords, , col='red',add=T)
terra::text(coords, labels=coords$site_ID, halo=TRUE, jitter = 10)

terra::add_legend('bottomleft',
                  legend = c('States and Territories', 'Weather gauge'),
                  col=c('black','red'),
                  pch = c(NA,16),
                  lty=c(1,NA)
                  )

## ----class.source = 'fold-hide'-----------------------------------------------
par(mfrow = c(n_sites/2, 2),
    pty = "s",
    cex = 0.75)

# Extract rainfall terra:rast grid at one time point
r  = extract_layer(ncdfFilename = ncdfFilename,
                   extract.date = dataFrom,
                   var = c('precip'))

# Get coordinate of grid cell centre.
v = terra::xyFromCell(r, 1:(terra::ncol(r)*terra::nrow(r)) )
v = terra::vect(v,  crs = 'EPSG:4283')

# Map around each gauge
for (i in 1:n_sites){
  coord_box = c(weather_gauge_loc[i, 'longitude'] - 0.1,
                weather_gauge_loc[i, 'longitude'] + 0.1,
                weather_gauge_loc[i, 'latitude'] - 0.1,
                weather_gauge_loc[i, 'latitude'] + 0.1
                )
  coord_ext = terra::ext(coord_box)
  crd = terra::vect(weather_gauge_loc[i,],
                    crs = 'EPSG:4283',
                    geom = c('longitude', 'latitude')
                   )
  v_crp = terra::crop(v, coord_ext)
  r_crp = terra::crop(r, coord_ext)

  r_crp <- terra::as.polygons(r_crp, aggregate=FALSE)

  dist = sqrt( (weather_gauge_loc[i, 'longitude'] - terra::crds(v_crp)[, 1])^2 +
               (weather_gauge_loc[i, 'latitude'] - terra::crds(v_crp)[, 2])^2)
  dist = min(dist)

  terra::plot(v_crp,
              xlab = 'Long.',
              ylab = 'Lat.',
              main = paste('Gauge', weather_gauge_loc[i,'site_ID'], ',',round(dist,3),'deg. from centre'),
              col='black',
              pch='x',
              asp=1)
  terra::lines(r_crp, col='grey')
  terra::points(crd, pch='o', col='red')
}

## -----------------------------------------------------------------------------
extracted.data = extract_data(ncdfFilename = ncdfFilename,
                                        extractFrom = dataFrom,
                                        extractTo = dataTo,
                                        locations = coords,
                                        vars = c('tmax', 'precip'),
                                        ET.function='')

## ----class.source = 'fold-hide'-----------------------------------------------
par(mfrow = c(n_sites, 2), cex = 0.5)
for (i in 1:n_sites){
  filt = extracted.data$Location.ID == coords$site_ID[i]

  dates = ISOdate(extracted.data$year[filt],
                  extracted.data$month[filt],
                  extracted.data$day[filt])

  data2plot = extracted.data$precip[filt]
  plot(dates, data2plot,
       main = paste('Extracted precip. at',coords$site_ID[i]),
       ylab = 'Precip [mm/d]',
       xlab = 'Date',
       type = 'l')

  data2plot = extracted.data$tmax[filt]
  plot(dates, data2plot,
       main = paste('Extracted max. temperature at',coords$site_ID[i]),
       ylab = 'Max. temp [C]',
       xlab = 'Date',
       type = 'l')
}

## -----------------------------------------------------------------------------
data("weather_gauge_obs")

## ----class.source = 'fold-hide'-----------------------------------------------
par(mfrow = c(n_sites, 3),
    mar = c(5, 5, 1.5, 1.5),
    pty = "s",
    cex = 0.5)

for (i in 1:n_sites){

  filt_obs = weather_gauge_obs$site_ID == coords$site_ID[i]
  xdata = weather_gauge_obs[ which(filt_obs), ]$precip_mmday

  filt_grd = extracted.data$Location.ID == coords$site_ID[i]
  ydata = extracted.data[filt_grd, ]$precip

  filt_obsErr = !(is.na(xdata) | xdata == -999)
  xdata = xdata[ filt_obsErr ]
  ydata = ydata[ filt_obsErr ]

  shared_limits <- range(c(xdata, ydata))
  plot(xdata,
       ydata,
       xlim = shared_limits,
       ylim = shared_limits,
       asp = 1,
       main = paste('Site', coords$site_ID[i], 'precip.'),
       xlab = 'Obs. [mm/d]',
       ylab = 'Extracted precip. [mm/d]')
  abline(0,1, col='grey', lty=2)
  rmse = round(sqrt(mean((xdata - ydata)^2)), 2)
  bias = round(mean(xdata - ydata), 1)
  text(x=shared_limits[1], y=shared_limits[2]*0.9, adj = c(0,0),
       labels = paste('RMSE = ', rmse, 'mm/day'))
  text(x=shared_limits[1], y=shared_limits[2]*0.8, adj = c(0,0),
       labels = paste('Bias = ', bias, 'mm/day'))

  xdata = cumsum(xdata)
  ydata = cumsum(ydata)
  shared_limits <- range(c(xdata, ydata))
  plot(xdata,
       ydata,
       xlim = shared_limits,
       ylim = shared_limits,
       asp = 1,
       main = paste('Site', coords$site_ID[i], 'cum. precip.'),
       xlab = 'Obs. [mm]',
       ylab = 'Extracted precip. [mm]',
       type = 'l')
  abline(0,1, col='grey', lty=2)

  xdata = weather_gauge_obs[ which(filt_obs), ]$tmax_C
  ydata = extracted.data[ filt_grd, ]$tmax

  filt_obsErr = !(is.na(xdata) | xdata == -999)
  xdata = xdata[ filt_obsErr ]
  ydata = ydata[ filt_obsErr ]
  shared_limits <- range(c(xdata, ydata))
  plot(xdata,
       ydata,
       xlim = shared_limits,
       ylim = shared_limits,
       asp = 1,
       main =paste('Site', coords$site_ID[i], 'max. T.'),
       xlab ='Obs. [C]',
       ylab ='Extracted tmax [C]')
  abline(0,1, col='grey', lty=2)
  rmse = round(sqrt(mean((xdata - ydata)^2)), 2)
  bias = round(mean(xdata - ydata), 1)
  text(x=shared_limits[1], y=shared_limits[2]*0.9, adj = c(0,0),
       labels = paste('RMSE = ', rmse, 'C'))
  text(x=shared_limits[1], y=shared_limits[2]*0.8, adj = c(0,0),
       labels = paste('Bias = ', bias, 'C'))
}

## -----------------------------------------------------------------------------
extracted.daily2monthly = extract_data(ncdfFilename = ncdfFilename,
                                        extractFrom = dataFrom,
                                        extractTo = dataTo,
                                        locations = coords,
                                        vars = c('precip'),
                                        temporal.timestep = 'monthly',
                                        temporal.fn.inner = 'sum',
                                        ET.function='')

## -----------------------------------------------------------------------------
extracted.monthly = extract_data(ncdfFilename = ncdfFilename,
                                        extractFrom = dataFrom,
                                        extractTo = dataTo,
                                        locations = coords,
                                        vars = c('precip.monthly'),
                                        temporal.timestep = 'monthly',
                                        ET.function='')

## ----class.source = 'fold-hide'-----------------------------------------------
par(mfrow = c(n_sites, 3),
    mar = c(5, 5, 1.5, 1.5),
    pty = "s",
    cex = 0.5)

for (i in 1:n_sites){
  xdata = weather_gauge_obs[ weather_gauge_obs$site_ID == as.numeric(coords$site_ID[i]), ]
  filt_obsErr = !(is.na(xdata$precip_mmday) | xdata$precip_mmday == -999)
  xdata = xdata[filt_obsErr, ]

  xdata = xts::as.xts(xdata[, 'precip_mmday'], order.by=xdata[, 'date'])
  xdata = xts::apply.monthly(xdata, apply, 2, 'sum')
  xdata = as.numeric(xdata[])

  filt = extracted.daily2monthly$Location.ID == coords$site_ID[i]
  ydata = extracted.daily2monthly[ filt, 'precip']

  shared_limits <- range(c(xdata, ydata))
  plot(xdata,
       ydata,
       xlim = shared_limits,
       ylim = shared_limits,
       asp = 1,
       main = paste('Site', coords$site_ID[i], 'summed daily precip.'),
       xlab ='Obs. [mm/d]',
       ylab ='Summed extracted precip. [mm/month]')
  abline(0,1, col='grey', lty=2)
  rmse = round(sqrt(mean((xdata - ydata)^2)), 1)
  bias = round(mean(xdata - ydata), 1)
  text(x=shared_limits[1], y=shared_limits[2]*0.9, adj = c(0,0),
       labels = paste('RMSE= ', rmse, 'mm/month\nBias= ', bias, 'mm/month'))

  ydata = extracted.monthly$precip.monthly[filt]
  shared_limits <- range(c(xdata, ydata))
  plot(xdata,
       ydata,
       xlim = shared_limits,
       ylim = shared_limits,
       asp = 1,
       main = paste('Site', coords$site_ID[i], 'monthly precip.'),
       xlab ='Obs. [mm/d]',
       ylab ='Extracted precip. [mm/month]')
  abline(0,1, col='grey', lty=2)
  rmse = round(sqrt(mean((xdata - ydata)^2)), 1)
  bias = round(mean(xdata - ydata), 1)
  text(x=shared_limits[1], y=shared_limits[2]*0.9, adj = c(0,0),
       labels = paste('RMSE= ', rmse, 'mm/month\nBias= ', bias, 'mm/month'))

  xdata = extracted.monthly$precip.monthly[filt]
  ydata = extracted.daily2monthly[ filt, 'precip']
  shared_limits <- range(c(xdata, ydata))
  plot(xdata,
       ydata,
       xlim = shared_limits,
       ylim = shared_limits,
       asp = 1,
       main = paste('Site', coords$site_ID[i], 'gridded monthly vs daily'),
       xlab ='Extracted monthly precip. [mm/month]',
       ylab ='Summed daily precip. [mm/month]')
  abline(0,1, col='grey', lty=2)
  rmse = round(sqrt(mean((xdata - ydata)^2)), 1)
  bias = round(mean(xdata - ydata), 1)
  text(x=shared_limits[1], y=shared_limits[2]*0.9, adj = c(0,0),
       labels = paste('RMSE= ', rmse, 'mm/month\nBias= ', bias, 'mm/month'))
}

