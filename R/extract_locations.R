#' @title Get Australian Hydrological Geospatial Fabric data
#' @description Selected Australian Hydrological Geospatial Fabric (Geofabric) spatial items are
#' downloaded, allowing an efficient workflow from location selection to data extraction.
#'
#' @details
#' The function downloads a selected set of BOM Australian Hydrological Geospatial Fabric spatial
#' vector data (points, lines and polygons) that likely to be relevant to hydrological, hydrogeological
#' and aquatic ecological studies. All of the daat is sourced from \url{https://www.bom.gov.au/water/geofabric/}.
#'
#' To find the required ID for the desired data type, either download all data and filter for the required
#' location (see example) or use the online mapping tool at \url{https://portal.wsapi.cloud.bom.gov.au/arcgis/home/webmap/viewer.html?useExisting=1&layers=35719064c4ea4ad79faa82f5c9c22068}
#'
#' @param type is a character for the type of geofabric item to download. The options are:
#' \itemize{
#'  \item{\code{'state'}: Australian state or territory, including islands. Geofabric ID = 0}
#'  \item{\code{'water_gauge'} : Surface water gauging station. Geofabric ID = 2}
#'  \item{\code{'river_simple'} : simplified line vectors of rivers.}
#'  \item{\code{'waterbody_simple'} : simplified poygons of stationary area of surface water (natural or constructed)}
#'  \item{\code{'drainage_basin'} : single large scale hydrological drainage basins. Geofabric ID = 34}
#'  \item{\code{'drainage_division'}: large scale collection of connected hydrological basins. Geofabric ID = 35}
#'  \item{\code{'catchment'} : single (often small) hydrological drainage basin with a stream gauge at the outlet. Geofabric ID = 49}
#'  \item{\code{'aquifer_upper'} : middle groundwater aquifer boundary and type. Geofabric ID = 58}
#'  \item{\code{'aquifer_mid'} : lower groundwater aquifer boundary and type. Geofabric ID = 59}
#'  \item{\code{'aquifer_lower'} : lower groundwater aquifer boundary and type. Geofabric ID = 59}
#' }
#' @param id is a scalar or vector of identifiers for the type of item to be downloaded. If \code{NA}, then all items will be downloaded.
#' Otherwise the input variable depends on the type. The options are:
#' \itemize{
#'  \item{\code{'state'}: character string for the state. The options are \code{'ACT', 'NSW', 'NT' ,'QLD', 'SA', 'TAS', 'VIC', 'WA'}}.
#'  \item{\code{'water_gauge'} : character string for the gauge ID.}
#'  \item{\code{'river_simple'} : integer for the Geofabric hydro ID.}
#'  \item{\code{'waterbody_simple'} : integer for the Geofabric hydro ID.}
#'  \item{\code{'drainage_basin'} :integer for the Geofabric divnumber ID. }
#'  \item{\code{'drainage_division'}: integer for the Geofabric level2num ID.}
#'  \item{\code{'catchment'} : character string for the gauge ID measuring the catchment.}
#'  \item{\code{'aquifer_upper'} : integer for the Geofabric hydro ID.}
#'  \item{\code{'aquifer_mid'} : integer for the Geofabric hydro ID.}
#'  \item{\code{'aquifer_lower'} : integer for the Geofabric hydro ID.}
#' }
#'
#' @return terra::SpatVector
#' @examples
#' vars = grid_sources()
#'
#' @seealso
#' \code{\link{extract_data}} for extracting climate data.
#' @export
extract_locations <- function(type= NA, id = NA) {

}
