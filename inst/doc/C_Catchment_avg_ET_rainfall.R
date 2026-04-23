## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(AWAPer, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
library(raster)
library(sp)

## -----------------------------------------------------------------------------
date.from = as.Date("2010-07-01","%Y-%m-%d")
date.to = as.Date("2010-10-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
fname = makeNetCDF_file(ncdfFilename = ncdfFilename,
                         updateFrom = date.from,
                         updateTo = date.to,
                         vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad'))

## -----------------------------------------------------------------------------
data("catchments")

## -----------------------------------------------------------------------------
data(constants,package='Evapotranspiration')

## -----------------------------------------------------------------------------
climateData.daily = extractCatchmentData(ncdfFilename=ncdfFilename,
                      extractFrom=date.from,
                      extractTo=date.to,
                      vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                      locations=catchments,
                      temporal.timestep = 'daily',
                      temporal.function.name='sum',
                      spatial.function.name='var',
                      ET.function='ET.MortonCRAE',
                      ET.timestep = 'monthly',
                      ET.Mortons.est='wet areal ET',
                      ET.constants= constants)

## -----------------------------------------------------------------------------
par(mfrow=c(2,1), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot the daily precipitation and PET.
for (i in 1:length(catchments$CatchID)) {

  filt = climateData.daily$temporal.sum$Location.ID == catchments$CatchID[i]

  # Convert year, month and day columns from extractions to a date.
  climateData.daily.date = as.Date(paste0(climateData.daily$temporal.sum$year[filt], "-",
                           climateData.daily$temporal.sum$month[filt], "-",
                           climateData.daily$temporal.sum$day[filt]))


  # Plot precipitation and standard deviation against observations
  # ---------------------------------------------------------
  max.y = max(climateData.daily$temporal.sum$precip[filt] +
        sqrt(climateData.daily$spatial.var$precip[filt]))

  # Precipitation
  plot(climateData.daily.date,
        climateData.daily$temporal.sum$precip[filt],
        type = "h", col = "#e31a1c", lwd = 3, mgp = c(2, 0.5, 0),
        main=paste('Catchment ID',catchments$CatchID[i]),
        ylim = c(0, 80), ylab = "", xlab = "2010", xaxs = "i",
yaxt = "n", bty = "l", yaxs = "i")

  axis(side = 2, mgp = c(2, 0.5, 0), line = 0.5, at = seq(from = 0, to = 80, by = 20),
        labels = c("0", "20", "40", "60", "80mm"), col = "#e31a1c", col.axis = "#e31a1c")

  # Standard deviation
  for (j in 1:length(climateData.daily.date)) {
    x.plot = rep(climateData.daily.date[j], 2)
    y.plot = c(climateData.daily$temporal.sum$precip[filt][j] +
        sqrt(climateData.daily$spatial.var$precip[filt][j]),
        climateData.daily$temporal.sum$precip[filt][j] -
        sqrt(climateData.daily$spatial.var$precip[filt][j]))
    lines(x.plot, y.plot, col = "black", lwd = 1.2)
  }

  # Plot evap data.
  par(new = TRUE)
  plot(climateData.daily.date, climateData.daily$temporal.sum$et[filt],
      col = "#bc80bd", lwd = 2, ylab = "", ylim = c(0, 4), lty = 1,
      xlab = "", xaxs = "i", yaxt = "n", xaxt = "n", type = "l", bty = "n", yaxs = "i")

  axis(side = 2, line = 2.3, mgp = c(2, 0.5, 0), labels = c("0", "1", "2", "3", "4mm"),
      at = seq(from = 0, to = 4, by = 1), col = "#bc80bd", col.axis = "#bc80bd")

  legend("topleft", cex = 0.8, lwd = 2, bty = "n", inset = c(0.01, -0.01),
      lty = c(1, 1), pch = c(NA, NA),
      col = c("#e31a1c",  "#bc80bd"),
      legend = c("Precipitation (bars +/- one standard dev.)", "Morton CRAE PET"), xpd = NA)
}

## -----------------------------------------------------------------------------
PrecipData.monthly = extractCatchmentData(ncdfFilename=ncdfFilename,
                     extractFrom=date.from,
                     extractTo=date.to,
                     vars = c('precip'),
                     locations=catchments,
                     spatial.function.name = '',
                     temporal.timestep = 'monthly',
                     temporal.function.name = 'sum')

## -----------------------------------------------------------------------------
v = list("sp.polygons", catchments, col = "red",first=FALSE)
colInd = which(startsWith(colnames(PrecipData.monthly@data), "precip_"))
sp::spplot(PrecipData.monthly,colInd, sp.layout = list(v),
        colorkey = list(title = "Precip (mm/month)"))

## -----------------------------------------------------------------------------
metData.monthly = extractCatchmentData(ncdfFilename=ncdfFilename,
                     extractFrom=date.from,
                     extractTo=date.to,
                     vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                     locations=catchments,
                     spatial.function.name = '',
                     temporal.timestep = 'monthly',
                     temporal.function.name = 'sum',
                     ET.function = 'ET.MortonCRAE',
                     ET.timestep = 'monthly',
                     ET.Mortons.est='wet areal ET',
                     ET.constants= constants)

## -----------------------------------------------------------------------------
colInd = which(startsWith(colnames(metData.monthly@data), "et_"))
sp::spplot(metData.monthly,colInd, sp.layout = list(v),
  colorkey = list(title = "PET (mm/month)"))

## -----------------------------------------------------------------------------
colnames.all = colnames(metData.monthly@data)
colInd.P = which(startsWith(colnames.all, "precip_"))
colInd.PET = which(startsWith(colnames.all, "et_"))

for (i in 1:length(colInd.P)) {
  ind.P = colInd.P[i]
  ind.PET = colInd.PET[i]
  colname.P = colnames.all[ ind.P ]
  colname.tmp = sub('precip_','Deficit_mm_',colname.P)
  metData.monthly[[colname.tmp]] = metData.monthly[[ind.P]] - metData.monthly[[ind.PET]]
}

colInd = which(startsWith(colnames(metData.monthly@data), "Deficit_mm_"))
sp::spplot(metData.monthly,colInd, sp.layout = list(v),
  colorkey = list(title = "Rainfall deficit, P - PET (mm/month)"))

## -----------------------------------------------------------------------------
centroid = matrix(0,2,2)

extn = extent(catchments[1,])
centroid[1,1] = extn@xmin + (extn@xmax - extn@xmin)/2
centroid[1,2] = extn@ymin + (extn@ymax - extn@ymin)/2

extn = extent(catchments[2,])
centroid[2,1] = extn@xmin + (extn@xmax - extn@xmin)/2
centroid[2,2] = extn@ymin + (extn@ymax - extn@ymin)/2


## -----------------------------------------------------------------------------
coordinates.data = data.frame( ID = catchments$CatchID,
                               Longitude = centroid[,1],
                               Latitude =  centroid[,2])

sp::coordinates(coordinates.data) <- ~Longitude + Latitude

sp::proj4string(coordinates.data) = '+proj=longlat +ellps=GRS80 +no_defs'

## -----------------------------------------------------------------------------
metData.monthly.weighted = extractCatchmentData(ncdfFilename=ncdfFilename,
                     extractFrom=date.from,
                     extractTo=date.to,
                     vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                     locations=catchments,
                     spatial.function.name = 'sum',
                     temporal.timestep = 'monthly',
                     temporal.function.name = 'sum',
                     ET.function='ET.MortonCRAE',
                     ET.timestep = 'monthly',
                     ET.Mortons.est='wet areal ET',
                     ET.constants= constants)

metData.monthly.centroid = extractCatchmentData(ncdfFilename=ncdfFilename,
                     extractFrom=date.from,
                     extractTo=date.to,
                     vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                     locations=coordinates.data,
                     temporal.timestep = 'monthly',
                     temporal.function.name = 'sum',
                     ET.function='ET.MortonCRAE',
                     ET.timestep = 'monthly',
                     ET.Mortons.est='wet areal ET',
                     ET.constants= constants)

## -----------------------------------------------------------------------------
par(mfrow=c(2,3), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot the daily precipitation and PET.
for (i in 1:length(catchments$CatchID)) {

  filt = metData.monthly.weighted$temporal.sum$Location.ID == catchments$CatchID[i]

  # Convert year, month and day columns from extractions to a date.
  metData.date = as.Date(paste0(metData.monthly.weighted$temporal.sum$year[filt], "-",
       metData.monthly.weighted$temporal.sum$month[filt], "-",
       metData.monthly.weighted$temporal.sum$day[filt]))

  # Precipitation
  plot(x = metData.date,
       y = metData.monthly.weighted$temporal.sum$precip[filt],
       type = "l",
       col = "#e31a1c",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = c(0, 200),
       ylab = "Precip. [mm/month]",
       xlab = "2010",
       xaxs = "i",
       bty = "l",
       yaxs = "i")

  lines(x = metData.date,
        y = metData.monthly.centroid$precip[filt],
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

  # PET
  plot(x = metData.date,
       y = metData.monthly.weighted$temporal.sum$et[filt],
       type = "l",
       col = "#e31a1c",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       main=paste('Catchment ID',catchments$CatchID[i]),
       ylim = c(0, 120),
       ylab = "PET [mm/month]",
       xlab = "2010", xaxs = "i",
       bty = "l",
       yaxs = "i")

  lines(metData.date, metData.monthly.centroid$et[filt],
        col = "#bc80bd", lwd = 1.2)

  # Deficit
  precip.deficit.weighted =  metData.monthly.weighted$temporal.sum$precip[filt] -
                             metData.monthly.weighted$temporal.sum$et[filt]

  plot(x = metData.date,
       y = precip.deficit.weighted,
       type = "l",
       col = "#e31a1c",
       lwd = 1.2,
       mgp = c(2, 0.5, 0),
       ylim = c(0, 140),
       ylab = "P - PET [mm/month]",
       xlab = "2010",
       xaxs = "i",
       bty = "l",
       yaxs = "i")

  precip.deficit.centroid =  metData.monthly.centroid$precip[filt] -
                             metData.monthly.centroid$et[filt]

  lines(metData.date, precip.deficit.centroid, col = "#bc80bd", lwd = 1.2)

  text(x = par("usr")[1]+5,
       y = par("usr")[3]+10,
       labels = paste('Mean diff. =',round(mean(precip.deficit.weighted -
              precip.deficit.centroid),1),'mm/month'),
       adj = c(0, 0))
}

