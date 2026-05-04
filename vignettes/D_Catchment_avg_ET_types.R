## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
date.from = as.Date("2010-01-01","%Y-%m-%d")
date.to = as.Date("2010-12-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
build.grids(ncdfFilename = ncdfFilename,
                   updateFrom = date.from,
                   updateTo = date.to,
                   vars = c('precip','tmin', 'tmax',
                   'vprp', 'solarrad'))

## -----------------------------------------------------------------------------
data("catchments")

## -----------------------------------------------------------------------------
data(constants,package='Evapotranspiration')

