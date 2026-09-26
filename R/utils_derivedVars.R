.vars_derived_est_by_column <- function(timepoints, grid_data, coords, DEMpoints, vars_derived) {

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

.ET_buildInputs <- function (timepoints = NA,
                             tmin = NA,
                             tmax = NA,
                             vprm = NA,
                             solarrad = NA,
                             precip = NA,
                             ET_missing_method,
                             ET_abnormal_method) {

    # Find variables with data
    has_data = c( Tmin = all(!is.na(tmin)),
                  Tmax = all(!is.na(tmax)),
                  Rs = all(!is.na(solarrad)),
                  Precip = all(!is.na(precip)),
                  va = all(!is.na(vprm))
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
                         va     = if ( has_data['va'    ] ) vprm/10  else rep(NA,ntimepoints)
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

