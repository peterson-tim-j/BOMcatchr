.vars_derived_inbuilt_funs <- function() {
  c('ET_HargreavesSamani',
    'ET_JensenHaise',
    'ET_Makkink',
    'ET_McGuinnessBordne',
    'ET_MortonCRAE',
    'ET_MortonCRWE',
    'ET_Turc')
}

.vars_derived_check <- function(extractFrom, extractTo, vars, vars_derived) {

  # Check derived vars are a list of lists
  if (!is.list(vars_derived) || !all(sapply(vars_derived, is.list)))
    .pretty_stop('The derived variables input must be a list of lists.')

  # Get list of all possible vars
  vars_all <- rownames(grid_sources())

  # Check each derived var
  for (ivars_derived in vars_derived) {
    # Test derived function  exists.
    FUN = ivars_derived[[1]]
    ivars_derived = ivars_derived[-1]
    if (!exists(FUN, mode = 'function'))
      .pretty_stop(paste('The following derived function could not be found:', FUN))

    # Get input vars to derived function that in vars_all
    FUN_vars = names(formals(FUN))
    ind_vars = FUN_vars %in% vars_all
    FUN_vars = FUN_vars[ind_vars]

    # Check input vars are to be extracted.
    ind_vars = !( FUN_vars %in% vars)
    if (any(ind_vars))
      .pretty_stop(paste0('The derived function, ', FUN,
                         ', requires the following additional variable to be extracted: ',
                         paste(FUN_vars[ind_vars], collapse = ', ')))

    # Is an inbuilt derived function?
    is_inbuilt = FUN %in% .vars_derived_inbuilt_funs()

    # Check inbuilt functions care called with the required inputs
    if (is_inbuilt) {

      # Check required inputs are provided to derived function.
      FUN_vars = names(formals(FUN))
      ind_vars = names(ivars_derived) %in% FUN_vars
      if (!all(ind_vars))
      .pretty_stop(paste0('The input derived function, ', FUN,
                          ', list item includes the variables that are not available within the built in function: ',
                          paste(names(ivars_derived)[ind_vars], collapse = ', ')))

      # Is extraction duration <2 years.
      is_le_2y= (extractTo - extractFrom)  < 2*365

      # Error if missing method is inappropriate for short record.
      if ('ET_missing_method' %in% names(formals(FUN))) {
        if ('ET_missing_method' %in% names(ivars_derived))
          ET_missing_method = ivars_derived$ET_missing_method
        else
          ET_missing_method = formals(FUN)$ET_missing_method

        # Check ET interpolation methods are appropriate if duration is <2 years
        if (is_le_2y  && ET_missing_method!='neighbouring average')
            .pretty_stop(paste('When the extraction duration is < 2 years, the in-built derived function,', FUN,
                         ', requires "ET_missing_method" and it must be specified and set to "neighbouring average".'))
        }

      # Error if abnormal method is inappropriate for short record.
      if ('ET_abnormal_method' %in% names(formals(FUN))) {
        if ('ET_abnormal_method' %in% names(ivars_derived))
          ET_abnormal_method = ivars_derived$ET_abnormal_method
        else
          ET_abnormal_method = formals(FUN)$ET_abnormal_method

        # Check ET interpolation methods are appropriate if duration is <2 years
        if (is_le_2y  && ET_abnormal_method!='neighbouring average')
          .pretty_stop(paste('When the extraction duration is < 2 years, the in-built derived function,', FUN,
                             ', requires "ET_abnormal_method" and it must be specified and set to "neighbouring average".'))
      }
    }
  }
}


.vars_derived_est_by_column <- function(timepoints, grid_data, coords, DEMpoints, vars_derived) {

  # Get name of derived function, and then collate other non-data inputs
  FUN = vars_derived[[1]]
  vars_derived = vars_derived[-1]
  vars_derived = c(list(timepoints=timepoints),
                   vars_derived)

  FUN_vars = names(formals(FUN))
  ind_vars = names(grid_data) %in% FUN_vars
  grid_data = grid_data[ind_vars]

  lat =  numeric()
  if ('lat' %in% FUN_vars)
    lat = coords[, 'y']

  elev = numeric()
  if ('elev' %in% FUN_vars)
    elev = DEMpoints

  grid_data = c(grid_data,
                list(lat = t(as.matrix(lat)),
                     elev = t(as.matrix(elev))
                     )
                )

  grid_data <- grid_data[lengths(grid_data) != 0]

  # Get number of grid cells or points
  npnts = ncol(grid_data[[1]])

  # Setup progress bar
  pbar <- progress::progress_bar$new(
      format = paste("   ",FUN,": :current cells of :total  [:bar] :percent in :elapsed"),
      total = npnts,
      clear = FALSE,
      width= 100,
      show_after = 0)

  # Return results from the analysis of each column in each list item of grid_data.
  # That is, take column 1 from each list item and pass to FUN. Repeat for column 2.
  # When all columns are done, bind into a matrix and return.
  lapply(
    seq_len(npnts),
    function(ind) {
      # Update progress bar
      pbar$tick()

      # Call derived function to calculate one column (ie 1 grid cell).
      do.call(
        FUN,
        c(
          lapply(grid_data, function(grid_data_col) grid_data_col[, ind]),
          vars_derived
         )
      )
    }
  )
}

ET_HargreavesSamani <- function(timepoints = NA,
                                tmin = NA,
                                tmax = NA,
                                lat = NA,
                                elev = NA,
                                ET_constants = NA,
                                ET_timestep = 'monthly',
                                ET_missing_method = "DoY average",
                                ET_abnormal_method = "DoY average") {

  # Built and infill data frame require for evaporation package.
  dataPP <- .ET_buildInputs(timepoints,
                            tmin = tmin,
                            tmax = tmax,
                            ET_missing_method = ET_missing_method,
                            ET_abnormal_method = ET_abnormal_method)

  # Update constants for the current location
  ET_constants = c(ET_constants, Elev = elev,  lat = lat, lat_rad = ET_constants$lat / 180.0*pi)

  # Call  ET package
  ET_est <- Evapotranspiration::ET.HargreavesSamani(dataPP,
                                                    ET_constants,
                                                    ts = ET_timestep,
                                                    AdditionalStats = 'no',
                                                    message = 'no')

  # Down scale to daily if estimated at monthly
  ET_est <- .ET_downscale(timepoints, ET_est, ET_timestep)

  return(ET_est)
}

ET_JensenHaise <- function(timepoints = NA,
                           tmin = NA,
                           tmax = NA,
                           solarrad = NA,
                           lat = NA,
                           elev = NA,
                           ET_constants = NA,
                           ET_timestep = 'monthly',
                           ET_missing_method = "DoY average",
                           ET_abnormal_method = "DoY average") {

  # Built and infill data frame require for evaporation package.
  dataPP <- .ET_buildInputs(timepoints,
                            tmin = tmin,
                            tmax = tmax,
                            solarrad = solarrad,
                            ET_missing_method = ET_missing_method,
                            ET_abnormal_method = ET_abnormal_method)

  # Update constants for the current location
  ET_constants = c(ET_constants, Elev = elev,  lat = lat, lat_rad = ET_constants$lat / 180.0*pi)

  # Call  ET package
  ET_est <- Evapotranspiration::ET.JensenHaise(dataPP,
                                               ET_constants,
                                               ts = ET_timestep,
                                               solar="data",
                                               AdditionalStats = 'no',
                                               message = 'no')

  # Down scale to daily if estimated at monthly
  ET_est <- .ET_downscale(timepoints, ET_est, ET_timestep)

  return(ET_est)
}

ET_Makkink <- function(timepoints = NA,
                       tmin = NA,
                       tmax = NA,
                       solarrad = NA,
                       lat = NA,
                       elev = NA,
                       ET_constants = NA,
                       ET_timestep = 'monthly',
                       ET_missing_method = "DoY average",
                       ET_abnormal_method = "DoY average") {

  # Built and infill data frame require for evaporation package.
  dataPP <- .ET_buildInputs(timepoints,
                            tmin = tmin,
                            tmax = tmax,
                            solarrad = solarrad,
                            ET_missing_method = ET_missing_method,
                            ET_abnormal_method = ET_abnormal_method)

  # Update constants for the current location
  ET_constants = c(ET_constants, Elev = elev,  lat = lat, lat_rad = ET_constants$lat / 180.0*pi)

  # Call  ET package
  ET_est <- Evapotranspiration::ET.Makkink(dataPP,
                                           ET_constants,
                                           ts = ET_timestep,
                                           solar="data",
                                           AdditionalStats = 'no',
                                           message = 'no')

  # Down scale to daily if estimated at monthly
  ET_est <- .ET_downscale(timepoints, ET_est, ET_timestep)

  return(ET_est)
}

ET_McGuinnessBordne <- function(timepoints = NA,
                       tmin = NA,
                       tmax = NA,
                       lat = NA,
                       elev = NA,
                       ET_constants = NA,
                       ET_timestep = 'monthly',
                       ET_missing_method = "DoY average",
                       ET_abnormal_method = "DoY average") {

  # Built and infill data frame require for evaporation package.
  dataPP <- .ET_buildInputs(timepoints,
                            tmin = tmin,
                            tmax = tmax,
                            ET_missing_method = ET_missing_method,
                            ET_abnormal_method = ET_abnormal_method)

  # Update constants for the current location
  ET_constants = c(ET_constants, Elev = elev,  lat = lat, lat_rad = ET_constants$lat / 180.0*pi)

  # Call  ET package
  ET_est <- Evapotranspiration::ET.McGuinnessBordne( dataPP,
                                                     ET_constants,
                                                     ts = ET_timestep,
                                                     solar="data",
                                                     AdditionalStats = 'no',
                                                     message = 'no')

  # Down scale to daily if estimated at monthly
  ET_est <- .ET_downscale(timepoints, ET_est, ET_timestep)

  return(ET_est)
}

ET_MortonCRAE <- function(timepoints = NA,
                          tmin = NA,
                          tmax = NA,
                          precip = NA,
                          vprp_3pm = NA,
                          solarrad = NA,
                          lat = NA,
                          elev = NA,
                          ET_Mortons_est = 'wet areal ET',
                          ET_constants = NA,
                          ET_timestep = 'monthly',
                          ET_missing_method = "DoY average",
                          ET_abnormal_method = "DoY average") {

  # Built and infill data frame require for evaporation package.
  dataPP <- .ET_buildInputs(timepoints,
                            tmin = tmin,
                            tmax = tmax,
                            precip = precip,
                            vprp = vprp_3pm,
                            solarrad = solarrad,
                            ET_missing_method = ET_missing_method,
                            ET_abnormal_method = ET_abnormal_method)

  # Update constants for the current location
  ET_constants = c(ET_constants, Elev = elev,  lat = lat, lat_rad = ET_constants$lat / 180.0*pi)

  # Call  ET package
  ET_est <- Evapotranspiration::ET.MortonCRAE( dataPP,
                                               ET_constants,
                                               ts = ET_timestep,
                                               solar="data",
                                               Tdew=FALSE,
                                               est=ET_Mortons_est,
                                               AdditionalStats = 'no',
                                               message = 'no')

  # Down scale to daily if estimated at monthly
  ET_est <- .ET_downscale(timepoints, ET_est, ET_timestep)

  return(ET_est)
}

ET_MortonCRWE <- function(timepoints = NA,
                          tmin = NA,
                          tmax = NA,
                          precip = NA,
                          vprp_3pm = NA,
                          solarrad = NA,
                          lat = NA,
                          elev = NA,
                          ET_Mortons_est = 'shallow lake ET',
                          ET_constants = NA,
                          ET_timestep = 'monthly',
                          ET_missing_method = "DoY average",
                          ET_abnormal_method = "DoY average") {

  # Built and infill data frame require for evaporation package.
  dataPP <- .ET_buildInputs(timepoints,
                            tmin = tmin,
                            tmax = tmax,
                            precip = precip,
                            vprp = vprp_3pm,
                            solarrad = solarrad,
                            ET_missing_method = ET_missing_method,
                            ET_abnormal_method = ET_abnormal_method)

  # Update constants for the current location
  ET_constants = c(ET_constants, Elev = elev,  lat = lat, lat_rad = ET_constants$lat / 180.0*pi)

  # Call  ET package
  ET_est <- Evapotranspiration::ET.MortonCRWE( dataPP,
                                               ET_constants,
                                               ts = ET_timestep,
                                               solar="data",
                                               Tdew=FALSE,
                                               est=ET_Mortons_est,
                                               AdditionalStats = 'no',
                                               message = 'no')

  # Down scale to daily if estimated at monthly
  ET_est <- .ET_downscale(timepoints, ET_est, ET_timestep)

  return(ET_est)
}

ET_Turc <- function(timepoints = NA,
                    tmin = NA,
                    tmax = NA,
                    solarrad = NA,
                    lat = NA,
                    elev = NA,
                    ET_constants = NA,
                    ET_timestep = 'monthly',
                    ET_missing_method = "DoY average",
                    ET_abnormal_method = "DoY average") {

  # Built and infill data frame require for evaporation package.
  dataPP <- .ET_buildInputs(timepoints,
                            tmin = tmin,
                            tmax = tmax,
                            solarrad = solarrad,
                            ET_missing_method = ET_missing_method,
                            ET_abnormal_method = ET_abnormal_method)

  # Update constants for the current location
  ET_constants = c(ET_constants, Elev = elev,  lat = lat, lat_rad = ET_constants$lat / 180.0*pi)

  # Call  ET package
  ET_est <- Evapotranspiration::ET.Turc( dataPP,
                                         ET_constants,
                                         ts = ET_timestep,
                                         solar="data",
                                         AdditionalStats = 'no',
                                         message = 'no')

  # Down scale to daily if estimated at monthly
  ET_est <- .ET_downscale(timepoints, ET_est, ET_timestep)

  return(ET_est)
}

# ET handler functions
#-------------------------------------------------------------------------------
.ET_buildInputs <- function (timepoints = NA,
                             tmin = NA,
                             tmax = NA,
                             precip = NA,
                             vprp  = NA,
                             solarrad = NA,
                             ET_missing_method,
                             ET_abnormal_method) {

    # Find variables with data
    has_data = c( Tmin = all(!is.na(tmin)),
                  Tmax = all(!is.na(tmax)),
                  Rs = all(!is.na(solarrad)),
                  Precip = all(!is.na(precip)),
                  va = all(!is.na(vprp))
                )
    # Build data from of daily climate data
    ntimepoints = length(timepoints)
    dataRAW = data.frame(Year =  as.integer(format.Date(timepoints,"%Y")),
                         Month= as.integer(format.Date(timepoints,"%m")),
                         Day= as.integer(format.Date(timepoints,"%d")),
                         Tmin   = if ( has_data['Tmin'  ] ) tmin     else rep(NA,ntimepoints),
                         Tmax   = if ( has_data['Tmax'  ] ) tmax     else rep(NA,ntimepoints),
                         Rs     = if ( has_data['Rs'    ] ) solarrad else rep(NA,ntimepoints),
                         Precip = if ( has_data['Precip'] ) precip   else rep(NA,ntimepoints),
                         va     = if ( has_data['va'    ] ) vprp/10  else rep(NA,ntimepoints)
                         )

    # Remove columns without extracted data
    if ( !has_data['Tmin'  ])
      dataRAW$Tmin <- NULL
    if ( !has_data['Tmax'  ])
      dataRAW$Tmax <- NULL
    if ( !has_data['Rs'    ])
      dataRAW$Rs <- NULL
    if ( !has_data['va'    ])
      dataRAW$va <- NULL
    if ( !has_data['Precip'])
      dataRAW$Precip <- NULL

    # Convert to required format for ET package.
    ET_vars = c('Tmax', 'Tmin', 'Rs', 'Precip', 'va')
    ET_vars = ET_vars[has_data]
    dataPP=Evapotranspiration::ReadInputs(ET_vars, dataRAW, constants=NA, stopmissing = c(99,99,99),
                                          interp_missing_days=T, interp_missing_entries=T, interp_abnormal=T,
                                          missing_method=ET_missing_method, abnormal_method=ET_abnormal_method, message = "no")

    return(dataPP)
}

# Interpolate monthly or annual data
.ET_downscale <- function(timepoints, ET_est, ET_timestep) {
  if (ET_timestep=='monthly' || ET_timestep=='annual') {

    # Get the last day of each month
    last.day.month = zoo::as.Date(zoo::as.yearmon(stats::time(ET_est$ET.Monthly)), frac = 1)

    # Get days per month
    days.per.month = as.integer(format.Date(zoo::as.Date(zoo::as.yearmon(stats::time(ET_est$ET.Monthly)), frac = 1),'%d'))

    # Set the first month to the start date for extraction.
    start.day.month = as.numeric(format(timepoints,"%d"))[1]
    days.per.month[1] = days.per.month[1] - start.day.month + 1

    # Set the last month to the end date for extraction.
    end.day.month = as.numeric(format(timepoints,"%d"))[length(timepoints)]
    days.per.month[length(days.per.month)] = end.day.month

    # Calculate average ET per day of each month
    monthly.ET.as.daily = zoo::zoo( as.numeric(ET_est$ET.Monthly/days.per.month), last.day.month)

    # Spline interpolate Monthly average ET
    timepoints.as.zoo = zoo::zoo(NA,timepoints);
    ET.est.tmp = zoo::na.approx(merge(monthly.ET.as.daily, dates=timepoints.as.zoo)[, 1],
                                rule=2)
    filt = stats::time(ET.est.tmp)>=stats::start(timepoints.as.zoo) &
      stats::time(ET.est.tmp)<=stats::end(timepoints.as.zoo)
    ET.est.tmp = pmax(0.0, as.numeric( ET.est.tmp));
    ET.est.tmp = ET.est.tmp[filt]
    ET_est = ET.est.tmp;
  } else {
    ET_est = ET_est$ET.Daily
  }

  return(ET_est)
}

