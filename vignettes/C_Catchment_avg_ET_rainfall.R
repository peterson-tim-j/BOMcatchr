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
fname = build.grids(ncdfFilename = ncdfFilename,
                         updateFrom = date.from,
                         updateTo = date.to,
                         vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad'))

## -----------------------------------------------------------------------------
catch = catchments()

## -----------------------------------------------------------------------------
data(constants,package='Evapotranspiration')

## -----------------------------------------------------------------------------
climateData.daily = extract.data(ncdfFilename=ncdfFilename,
                      extractFrom=date.from,
                      extractTo=date.to,
                      vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                      locations=catch,
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
for (i in 1:length(catch$CatchID)) {

  filt = climateData.daily$temporal.sum$Location.ID == catch$CatchID[i]

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
        type = "h",
        col = "#e31a1c",
        lwd = 3,
        mgp = c(2, 0.5, 0),
        main=paste('Catchment ID',catch$CatchID[i]),
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
    y.plot = c(climateData.daily$temporal.sum$precip[filt][j] +
        sqrt(climateData.daily$spatial.var$precip[filt][j]),
        climateData.daily$temporal.sum$precip[filt][j] -
        sqrt(climateData.daily$spatial.var$precip[filt][j]))
    lines(x.plot, y.plot, col = "black", lwd = 1.2)
  }

  # Plot evap data.
  par(new = TRUE)
  plot(climateData.daily.date,
       climateData.daily$temporal.sum$et[filt],
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
PrecipData.monthly = extract.data(ncdfFilename=ncdfFilename,
                     extractFrom = date.from,
                     extractTo = date.to,
                     vars = c('precip'),
                     locations=catch,
                     spatial.function.name = '',
                     temporal.timestep = 'monthly',
                     temporal.function.name = 'sum')

## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
map.labels = gsub( 'precip_', '',names(PrecipData.monthly[[2:5]]))
map.labels = gsub( '_', '-',map.labels)

plot.data = PrecipData.monthly[[-1]]

panel.maps(plot.data, catch, map.labels, "Precip. (mm/month)")

## -----------------------------------------------------------------------------
metData.monthly = extract.data(ncdfFilename=ncdfFilename,
                     extractFrom=date.from,
                     extractTo=date.to,
                     vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                     locations=catch,
                     spatial.function.name = '',
                     temporal.timestep = 'monthly',
                     temporal.function.name = 'sum',
                     ET.function = 'ET.MortonCRAE',
                     ET.timestep = 'monthly',
                     ET.Mortons.est='wet areal ET',
                     ET.constants= constants)

## -----------------------------------------------------------------------------
colInd = which(startsWith(names(metData.monthly), "et_"))
plot.data = metData.monthly[[colInd]]

panel.maps(plot.data, catch, map.labels, "PET (mm/month)")

## -----------------------------------------------------------------------------
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
metData.monthly.weighted = extract.data(ncdfFilename=ncdfFilename,
                     extractFrom = date.from,
                     extractTo=date.to,
                     vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                     locations = catch,
                     spatial.function.name = 'sum',
                     temporal.timestep = 'monthly',
                     temporal.function.name = 'sum',
                     ET.function = 'ET.MortonCRAE',
                     ET.timestep = 'monthly',
                     ET.Mortons.est = 'wet areal ET',
                     ET.constants =  constants)

metData.monthly.centroid = extract.data(ncdfFilename=ncdfFilename,
                     extractFrom = date.from,
                     extractTo = date.to,
                     vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                     locations = centroid,
                     temporal.timestep = 'monthly',
                     temporal.function.name = 'sum',
                     ET.function='ET.MortonCRAE',
                     ET.timestep = 'monthly',
                     ET.Mortons.est = 'wet areal ET',
                     ET.constants =  constants)

## -----------------------------------------------------------------------------
par(mfrow=c(2,3), mar =  c(5, 7.5, 4, 2.7) + 0.1)

# Loop through each catchment and plot the daily precipitation and PET.
for (i in 1:length(catch$CatchID)) {

  filt = metData.monthly.weighted$temporal.sum$Location.ID == catch$CatchID[i]

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
       main=paste('Catchment ID',catch$CatchID[i]),
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

