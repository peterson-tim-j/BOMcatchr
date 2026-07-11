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
#'  \item{\code{'waterbody_simple'} : simplified polygons of stationary area of surface water (natural or constructed)}
#'  \item{\code{'drainage_basin'} : single large scale hydrological drainage basins. Geofabric ID = 34}
#'  \item{\code{'drainage_division'}: large scale collection of connected hydrological basins. Geofabric ID = 35}
#'  \item{\code{'catchment'} : single (often small) hydrological drainage basin with a stream gauge at the outlet. Geofabric ID = 49}
#'  \item{\code{'aquifer_upper'} : middle groundwater aquifer boundary and type. Geofabric ID = 57}
#'  \item{\code{'aquifer_mid'} : lower groundwater aquifer boundary and type. Geofabric ID = 58}
#'  \item{\code{'aquifer_lower'} : lower groundwater aquifer boundary and type. Geofabric ID = 59}
#' }
#' @param id is a scalar or vector of identifiers for the type of item to be downloaded. If \code{NA}, then all items will be downloaded.
#' Otherwise the input variable depends on the type. The options are:
#' \itemize{
#'  \item{\code{'state'}: character string for the state. The options are \code{'ACT', 'NSW', 'NT' ,'QLD', 'SA', 'TAS', 'VIC', 'WA'}}.
#'  \item{\code{'water_gauge'} : character string for the "stationno" ID.}
#'  \item{\code{'river_simple'} : integer for the Geofabric "hydroid".}
#'  \item{\code{'waterbody_simple'} : integer for the Geofabric "hydroid".}
#'  \item{\code{'drainage_basin'} :integer for the Geofabric "level2num" ID. }
#'  \item{\code{'drainage_division'}: integer for the Geofabric "divnumber" ID.}
#'  \item{\code{'catchment'} : character string for the Geofabric "stationno" ID at the catchment outlet.}
#'  \item{\code{'aquifer_upper'} : integer for the Geofabric "hydroid".}
#'  \item{\code{'aquifer_mid'} : integer for the Geofabric "hydroid".}
#'  \item{\code{'aquifer_lower'} : integer for the Geofabric "hydroid".}
#' }
#'
#' @return terra::SpatVector
#' @examples
#' # Get state boundary, all stream lines, all stream gauges and two catchment boundaries
#'  st <- extract_locations('state','VIC')
#'  riv <- extract_locations('river_simple',NA)
#'  gauge <- extract_locations('water_gauge',NA)
#'  catch <- extract_locations('catchment',c('407214','407220'))
#'
#'  # Filter streamlines and gauges to those within the catchment boundaries
#'  gauge <- gauge[catch]
#'  riv <- riv[catch]
#'
#'  # Map catchments in state
#'  terra::plot(st)
#'  terra::plot(catch, col='red', add=T)
#'
#'  # Map catchments, streamlines and gauges
#'  terra::plot(catch, col='red')
#'  terra::plot(riv, col='blue', add=T)
#'  terra::plot(gauge, col='grey', add=T)
#'
#' @seealso
#' \code{\link{extract_data}} for extracting climate data.
#' @export
extract_locations <- function(type= NA,
                              id = NA,
                              url_base = 'https://hosting.wsapi.cloud.bom.gov.au/arcgis/rest/services/') {

    # Test internet connection
    if (!curl::has_internet())
      .pretty_stop('No internet connection appears available. Check connection.')

    # Define valid types
    types_valid = c('state', 'water_gauge', 'river_simple', 'waterbody_simple',
                    'drainage_basin', 'drainage_division', 'catchment',
                    'aquifer_upper', 'aquifer_mid', 'aquifer_lower')

    # Check type is a valid data type
    if (!is.character(type))
      pretty.stop('The input variable type must be a character string.')
    if (length(type)>1)
      pretty.stop('The input variable type must be a single character string, not a vector.')

    # Get geofabric index for the required variable
    ind = switch (type,
      state = 0,
      water_gauge = 2,
      river_simple = 0,
      waterbody_simple = 0,
      drainage_basin = 34,
      drainage_division = 35,
      catchment = 49,
      aquifer_upper = 57,
      aquifer_mid = 58,
      aquifer_lower = 59,
      pretty.stop(paste('The following input type is invalid:', type))
    )

    # Set postfix URL for type
    url_postfix = switch(type,
                         state = 'base_layers/Aust_State_boundary/MapServer/',
                         river_simple = 'ahgf/Simplified_NetworkStream/MapServer/',
                         waterbody_simple = 'ahgf/Simplified_Waterbody/MapServer/',
                         'ahgf/Geofabric_V3x_All_Products/MapServer/'
                         )

    # Build filter for required locations
    if (length(id)==1 && is.na(id)) {
      where <- utils::URLencode('1=1', reserved=T)
    } else {
      if (type =='state') {
        id = switch(id,
                    ACT = 1,
                    NSW = 3,
                    NT = 4,
                    QLD = 5,
                    SA = 6,
                    TAS = 7,
                    VIC = 8,
                    WA = 9,
                    pretty.stop(paste('The following input id is unknown for the type "state":', id))
                    )

        base_string <- paste0("%s IN (", paste(rep("%s ", length(id)),collapse = ", "), ')')
        where <- utils::URLencode( do.call(sprintf, c(fmt = base_string, as.list( c('state',id) ))), reserved = T)

      } else {
        # Set the field name use to filter each data type
        fname = switch(type,
                       water_gauge = 'stationno',
                       river_simple = 'hydroid',
                       waterbody_simple = 'hydroid',
                       drainage_basin = 'level2num',
                       drainage_division = 'divnumber',
                       catchment = 'stationno',
                       aquifer_upper = 'hydroid',
                       aquifer_mid = 'hydroid',
                       aquifer_lower = 'hydroid'
        )

        # Build URL that does not download the geometry and then check the ID is in the returned objects.
        where <- utils::URLencode('1=1', reserved=T)
        url = paste0(url_base,
                     url_postfix,
                     ind, '/query?',
                     'where=', where,
                     '&outFields=*',
                     '&returnGeometry=false',
                     '&f=geojson')

        # Download data
        v = terra::vect(url)

        # Check field name
        if (!(fname %in% names(v)))
          pretty.stop(paste('The variable name corresponding to the input type was not in the spatial data:', fname))


        # Check if id values are in the corresponding field.
        indx = id %in% terra::values(v[,fname])[,1]
        if (any(!indx))
          pretty.stop(cat('The folowing id value are not in the spatial data:', id[!indx]))

        if (all(is.character(id))) {
          base_string <- paste0("%s IN (", paste(rep("'%s' ", length(id)),collapse = ", "), ')')
          where <- utils::URLencode( do.call(sprintf, c(fmt = base_string, as.list( c(fname,id) ))), reserved = T)
        }
        else {
          base_string <- paste0("%s IN (", paste(rep("%d ", length(id)),collapse = ", "), ')')
          where <- utils::URLencode( do.call(sprintf, c(fmt = base_string, as.list( c(fname,id) ))), reserved = T)
        }

      }
    }

    # Build URL to data set
    url = paste0(url_base,
                 url_postfix,
                 ind, '/query?',
                 'where=', where,
                 '&outFields=*',
                 '&returnGeometry=true',
                 '&f=geojson'
                 )

    # Download spatial data
    v = terra::vect(url)

    return(v)
}
