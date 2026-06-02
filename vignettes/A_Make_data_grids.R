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

