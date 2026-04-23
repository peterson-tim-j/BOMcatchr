## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(AWAPer)

## -----------------------------------------------------------------------------
startDate <- as.Date(Sys.Date()-15,"%Y-%m-%d")
endDate <- as.Date(Sys.Date()-5,"%Y-%m-%d")

## -----------------------------------------------------------------------------
ncdfFilename <- tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
ncdffile.name <- makeNetCDF_file(ncdfFilename=ncdfFilename,
                updateFrom=startDate, updateTo=endDate,
                vars = c('precip','tmin','tmax'))

## -----------------------------------------------------------------------------
summary.df <- AWAPer::file.summary(ncdffile.name)
summary.df

## -----------------------------------------------------------------------------
ncdffile.name <- makeNetCDF_file(ncdfFilename=ncdffile.name,
                updateFrom=startDate, updateTo=endDate)

## -----------------------------------------------------------------------------
ncdffile.name <- makeNetCDF_file(ncdfFilename=ncdffile.name,
                updateFrom=startDate, updateTo=endDate,
                vars = c('vprp'))

## -----------------------------------------------------------------------------
summary.df <- AWAPer::file.summary(ncdffile.name)
summary.df

## -----------------------------------------------------------------------------
startDate <- startDate - 5

## -----------------------------------------------------------------------------
ncdffile.name <- makeNetCDF_file(ncdfFilename=ncdffile.name,
                updateFrom=startDate, updateTo=endDate,
                vars = c('solarrad'))

## -----------------------------------------------------------------------------
summary.df <- AWAPer::file.summary(ncdffile.name)
summary.df

