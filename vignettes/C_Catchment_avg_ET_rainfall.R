## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
library(raster)
library(sp)

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
data("catchments")

## -----------------------------------------------------------------------------
data(constants,package='Evapotranspiration')

