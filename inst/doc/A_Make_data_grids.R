## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr)

## -----------------------------------------------------------------------------
startDate <- as.Date(Sys.Date()-15,"%Y-%m-%d")
endDate <- as.Date(Sys.Date()-5,"%Y-%m-%d")

## -----------------------------------------------------------------------------
ncdfFilename <- tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
ncdffile.name <- grid_build(ncdfFilename=ncdfFilename,
                updateFrom=startDate, updateTo=endDate,
                vars = c('precip','tmin','tmax'))

## -----------------------------------------------------------------------------
summary.df <- BOMcatchr::grid_summary(ncdffile.name)
summary.df

## -----------------------------------------------------------------------------
summary.ages <- BOMcatchr::grid_ages(ncdffile.name, today = Sys.Date()+12, plot.sourcedate = T)

## -----------------------------------------------------------------------------
ncdffile.name <- grid_build(ncdfFilename=ncdffile.name,
                updateFrom=startDate, updateTo=endDate)

## -----------------------------------------------------------------------------
ncdffile.name <- grid_build(ncdfFilename=ncdffile.name,
                updateFrom=startDate, updateTo=endDate,
                vars = c('vprp_3pm'))

## -----------------------------------------------------------------------------
summary.df <- BOMcatchr::grid_summary(ncdffile.name)
summary.df

## -----------------------------------------------------------------------------
startDate <- startDate - 5

## -----------------------------------------------------------------------------
ncdffile.name <- grid_build(ncdfFilename=ncdffile.name,
                updateFrom=startDate, updateTo=endDate,
                vars = c('solarrad'))

## -----------------------------------------------------------------------------
summary.df <- BOMcatchr::grid_summary(ncdffile.name)
summary.df

