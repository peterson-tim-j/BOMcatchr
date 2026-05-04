## code to prepare `catchments` dataset goes here
library(terra)

catchments=terra::vect("data-raw/catchments.shp")

terra::writeVector(catchments, "inst/extdata/catchments.gpkg", overwrite = TRUE)

#usethis::use_data(catchments, overwrite = TRUE)

