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
fnames = build.grids(ncdfFilename = ncdfFilename,
                         updateFrom = dataFrom,
                         updateTo = dataTo,
                         vars = c('precip', 'precip.monthly'))

## -----------------------------------------------------------------------------
coordinates.data = data.frame( ID =c('Bore-10084446','Rain-63005'),
                               Longitude = c(153.551875, 149.5559),
                               Latitude =  c(-28.517974,-33.4289))

sp::coordinates(coordinates.data) <- ~Longitude + Latitude

sp::proj4string(coordinates.data) = '+proj=longlat +ellps=GRS80 +no_defs'

## -----------------------------------------------------------------------------
extracted.data = extract.data(ncdfFilename = ncdfFilename,
                                        extractFrom = dataFrom,
                                        extractTo = dataTo,
                                        locations = coordinates.data,
                                        vars = c('precip'),
                                        ET.function='')

## -----------------------------------------------------------------------------
par(mfrow = c(2, 1))
site.names = c('Bore-10084446','Rain-63005')
for (i in 1:nrow(coordinates.data)){
  filt = extracted.data$Location.ID == coordinates.data$ID[i]

  data2plot = extracted.data$precip[filt]
  dates = ISOdate(extracted.data$year[filt],
                  extracted.data$month[filt],
                  extracted.data$day[filt])

    plot(dates, data2plot,
         main = paste('Extracted precip. at',site.names[i]),
         ylab = 'Precip [mm/d]',
         xlab = 'Date',
         type = 'l')
}

## -----------------------------------------------------------------------------
data('raingauge')

## -----------------------------------------------------------------------------
par(mfrow = c(1, 2))

filt = extracted.data$Location.ID=='Rain-63005'

xdata = raingauge$Rainfall.amount..millimetres.
ydata = extracted.data$precip[filt]
plot(xdata,
     ydata,
     xlim = c(0,35),
     ylim = c(0,35),
     main ='Obs. vs. Extracted precip.',
     xlab ='Obs. [mm/d]',
     ylab ='Extracted precip. [mm/d]')
abline(0,1, col='grey', lty=2)
rmse = round(sqrt(mean((xdata - ydata)^2)), 2)
text(x=0, y=30, adj = c(0,0),
     labels = paste('RMSE = ', rmse, 'mm/day'))

plot(cumsum(xdata),
     cumsum(ydata),
     xlim = c(0,175),
     ylim = c(0,175),
     main = 'Cum. obs. vs. Extracted precip.',
     xlab = 'Obs. [mm]',
     ylab = 'Extracted precip. [mm]',
     type = 'l')
abline(0,1, col='grey', lty=2)

## -----------------------------------------------------------------------------
extracted.daily2monthly = extract.data(ncdfFilename = ncdfFilename,
                                        extractFrom = dataFrom,
                                        extractTo = dataTo,
                                        locations = coordinates.data,
                                        vars = c('precip'),
                                        temporal.timestep = 'monthly',
                                        temporal.function.name = 'sum',
                                        ET.function='')

## -----------------------------------------------------------------------------
extracted.monthly = extract.data(ncdfFilename = ncdfFilename,
                                        extractFrom = dataFrom,
                                        extractTo = dataTo,
                                        locations = coordinates.data,
                                        vars = c('precip.monthly'),
                                        temporal.timestep = 'monthly',
                                        ET.function='')

## -----------------------------------------------------------------------------

dates = as.Date(ISOdate( raingauge$Year,
                         raingauge$Month,
                         raingauge$Day))

raingauge.xts = xts::as.xts(raingauge$Rainfall.amount..millimetres. ,
                            order.by=dates)

raingauge.monthly = xts::apply.monthly(raingauge.xts, apply, 2, 'sum')


## -----------------------------------------------------------------------------
par(mfrow = c(3, 1))

filt1 = extracted.daily2monthly$Location.ID=='Rain-63005'
filt2 = extracted.monthly$Location.ID=='Rain-63005'

xdata = as.numeric(raingauge.monthly)
ydata = extracted.daily2monthly$precip[filt1]
plot(xdata,
     ydata,
     xlim = c(0,220),
     ylim = c(0,220),
     main='Obs. monthly vs extracted aggregated to monthly',
     xlab='Obs. [mm/month]',
     ylab='Aggregated precip. [mm/month]')
abline(0,1, col='grey', lty=2)
rmse = round(sqrt(mean((xdata - ydata)^2)), 2)
text(x=0, y=200, adj = c(0,0),
     labels = paste('RMSE = ', rmse, 'mm/month'))

xdata = as.numeric(raingauge.monthly)
ydata = extracted.monthly$precip[filt2]
plot(xdata,
     ydata,
     xlim = c(0,220),
     ylim = c(0,220),
     main='Obs. monthly vs extracted monthly',
     xlab='Obs. [mm/month]',
     ylab='Extracted precip. [mm/month]')
abline(0,1, col='grey', lty=2)
rmse = round(sqrt(mean((xdata - ydata)^2)), 2)
text(x=0, y=200, adj = c(0,0),
     labels = paste('RMSE = ', rmse, 'mm/month'))

xdata = extracted.monthly$precip[filt2]
ydata = extracted.daily2monthly$precip[filt1]
plot(xdata,
     ydata,
     xlim = c(0,220),
     ylim = c(0,220),
     main='Extracted monthly vs extracted aggregated to monthly',
     xlab='Extracted precip. [mm/month]',
     ylab='Aggregated precip. [mm/month]')
abline(0,1, col='grey', lty=2)

