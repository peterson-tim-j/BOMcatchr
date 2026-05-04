#' @title Example catchment boundaries.
#' @docType data
#' @description Two example catchment boundaries as a SpatialPolygonsDataFrame. The catchments are Creswick Creek (ID 407214, Vic., Australia, see \url{http://www.bom.gov.au/water/hrs/#id=407214}) and
#' Bet Bet Creek (ID 407220, Vic., Australia, see \url{http://www.bom.gov.au/water/hrs/#id=407220}).
#'
#' The catchments can be used to extract catchment average climate data usng \code{extract.data}
#' @return terra::SpatVector
#' @seealso
#' \code{\link{extract.data}} for extracting catchment average climate data.
#' @export
catchments <- function() {
  path <- system.file("extdata", "catchments.gpkg", package = "BOMcatchr")

  if (path == "") {
    stop("Internal catchment boundary data file not found. Please reinstall the package.")
  }

  terra::vect(path)
}
