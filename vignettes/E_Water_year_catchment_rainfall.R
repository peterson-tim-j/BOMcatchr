## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr)

## -----------------------------------------------------------------------------
date.from = as.Date("2010-01-01","%Y-%m-%d")
date.to = as.Date("2015-12-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
fname = build.grids(ncdfFilename = ncdfFilename,
                         updateFrom = date.from,
                         updateTo = date.to,
                         vars = c('precip', 'precip.RMSE', 'precip.monthly'))

## -----------------------------------------------------------------------------
catch = catchments()

## -----------------------------------------------------------------------------
climateData.annual = extract.data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip', 'precip.monthly'),
                      locations = catch,
                      temporal.timestep = 'annual',
                      temporal.function.name = 'sum',
                      spatial.function.name = 'var')

## -----------------------------------------------------------------------------
sqrd.sum <- function(x) {return(sum(x^2))}

climateData.annual.err = extract.data(ncdfFilename = ncdfFilename,
                      extractFrom = date.from,
                      extractTo = date.to,
                      vars = c('precip.RMSE'),
                      locations = catch,
                      temporal.timestep = 'annual',
                      temporal.function.name = sqrd.sum,
                      spatial.function.name = 'var')

