## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr)

## -----------------------------------------------------------------------------
startDate <- as.Date(Sys.Date()-25,"%Y-%m-%d")
endDate <- as.Date(Sys.Date()-5,"%Y-%m-%d")

## -----------------------------------------------------------------------------
ncdfFilename <- tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
ncdffile.name <- grid_build(ncdfFilename=ncdfFilename,
                updateFrom=startDate,
                updateTo=endDate,
                vars = c('precip','tmin','tmax'))

## -----------------------------------------------------------------------------
summary.df <- BOMcatchr::grid_summary(ncdffile.name)
summary.df

## -----------------------------------------------------------------------------
summary.ages <- BOMcatchr::grid_ages(ncdffile.name,
                                     today = Sys.Date()+12,
                                     plot.sourcedate = T)

## -----------------------------------------------------------------------------
nstations <- BOMcatchr::extract_nstations(ncdffile.name)

## -----------------------------------------------------------------------------
 nvars = length(nstations)
 par(mfrow=c(nvars,1), mar =  c(5, 7.5, 4, 2.7) + 0.1)

 for (i in 1:nvars) {
    xdata = nstations[[i]]$Date
    ydata = nstations[[i]]$n_stations

    plot(xdata,
         ydata,
         main = names(nstations)[i],
         ylab = 'Number stations',
         xlab = 'Date',
         type='l')
 }

## -----------------------------------------------------------------------------
ncdffile.name <- grid_build(ncdfFilename=ncdffile.name,
                updateFrom=startDate,
                updateTo=endDate)

## -----------------------------------------------------------------------------
ncdffile.name <- grid_build(ncdfFilename=ncdffile.name,
                updateFrom=startDate,
                updateTo=endDate,
                vars = c('vprp_3pm'))

## -----------------------------------------------------------------------------
summary.df <- BOMcatchr::grid_summary(ncdffile.name)
summary.df

## -----------------------------------------------------------------------------
startDate <- startDate - 5

## -----------------------------------------------------------------------------
ncdffile.name <- grid_build(ncdfFilename=ncdffile.name,
                updateFrom=startDate,
                updateTo=endDate,
                vars = c('solarrad'))

## -----------------------------------------------------------------------------
summary.df <- BOMcatchr::grid_summary(ncdffile.name)
summary.df

## -----------------------------------------------------------------------------
summary.ages <- BOMcatchr::grid_ages(ncdffile.name,
                                     today = Sys.Date()+12,
                                     plot.sourcedate = T)

