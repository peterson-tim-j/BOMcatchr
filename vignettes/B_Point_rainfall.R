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
fnames = grid_build(ncdfFilename = ncdfFilename,
                         updateFrom = dataFrom,
                         updateTo = dataTo,
                         vars = c('tmax', 'precip', 'precip.monthly'))

## -----------------------------------------------------------------------------
data("weather_gauge_loc")

coords = terra::vect(weather_gauge_loc,
                     crs = '+proj=longlat +ellps=GRS80 +no_defs',
                     geom = c("longitude", "latitude")
                     )

n_sites = nrow(coords)

