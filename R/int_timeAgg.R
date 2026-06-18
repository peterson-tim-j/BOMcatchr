# Time aggregation internal functions
#-------------------------------------------

do.TemporalAggregation = function( data=NA,
                                   location.ID,
                                   location.lookup,
                                   time.from,
                                   time.to,
                                   time.step.in,
                                   time.step.out,
                                   FUN.outer,
                                   FUN.inner,
                                   ind=NA) {

  # Build dates vector here because variables can have a daily
  # or monthly time step.
  dates <-
    switch(time.step.in,
           days =  seq( time.from, by='day', to=time.to),
           months =  seq( time.from, by='month', to=time.to),
           years =  seq( time.from, by='year', to=time.to)
    )

  # If data is NA then build a vector of ones for length of dates.
  # This is done to allow return of the number of time steps within
  # time aggregated period. Else, real data is handled.
  if (length(data)==1 && is.na(data)) {
    data = matrix(rep(1, length(dates)), ncol=1)
    data.xts = xts::as.xts(data, order.by=dates)
  } else {
    cell.index = location.lookup[location.ID,1]:location.lookup[location.ID,2]
    data.xts = xts::as.xts(data[, cell.index], order.by=dates)
  }

  # Aggregate over time using user defined time step and function
  if (is.function(FUN.outer)) {

    fn.nargs = length(formals(FUN.outer))

    if (fn.nargs==1)
      data.xts <- agg.by(x = data.xts, by = time.step.out, ind=ind, FUN = FUN.outer)
    else if (is.na(FUN.inner) && fn.nargs<4)
      data.xts <- do.call(FUN.outer, list(x = data.xts, by = time.step.out, ind=ind))
    else
      data.xts <- do.call(FUN.outer, list(x = data.xts, by = time.step.out, ind=ind, FUN = FUN.inner))

  } else if (!is.na(FUN.inner)) {
    data.xts <- do.call(FUN.outer, list(x = data.xts, by = time.step.out, ind=ind, FUN = FUN.inner))
  } else {
    data.xts <- agg.by(x = data.xts, by = time.step.out, ind=ind, FUN = FUN.outer)
  }

  return(data.xts)
}

agg.options <- function() {
  opt = list( temporal.timestep = c('daily','weekly','monthly','quarterly', 'seasonal', 'wetdry.seasonal','tropical.seasonal', 'annual', 'period') ,
              temporal.function.name = c('sum', 'mean', 'min', 'max', 'median', 'sd', 'var', 'IQR'),
              spatial.function.name = c() )
  return(opt)
}

# Aggregate source time step (daily or monthly) to user defined time step
agg.by <- function(x, by, ind, FUN = 'sum') {
  x.agg <-
    switch(by,
           daily = x,
           weekly = xts::apply.weekly(x, apply, 2, FUN),
           monthly = xts::apply.monthly(x, apply, 2, FUN),
           quarterly = xts::apply.quarterly(x, apply, 2, FUN),
           annual = xts::apply.yearly(x, apply, 2, FUN),
           seasonal = xts::period.apply(x, INDEX=by.season(x)$ind, apply, 2, FUN),
           wetdry.seasonal = xts::period.apply(x, INDEX=by.wetdrySeason(x)$ind, apply, 2, FUN),
           tropical.seasonal = xts::period.apply(x, INDEX=by.tropicalSeason(x)$ind, apply, 2, FUN),
           period = xts::period.apply(x, INDEX=ind, apply, 2, FUN),
    )
  return(x.agg)
}

annom <- function(x, by, ind, FUN) {

  # Aggregate using user defined time step and function
  x.agg <- agg.by(x, by = by, ind, FUN = FUN)

  # Get zoo index to each within year time step
  x.agg.avg <-
    switch(by,
           daily = aggregate(x.agg, by=format(zoo::index(x.agg), "%j"), mean),
           weekly = aggregate(x.agg, by=format(zoo::index(x.agg),'%V'), mean),
           monthly = aggregate(x.agg, by=format(zoo::index(x.agg), "%m"), mean),
           quarterly = aggregate(x.agg, by=format(zoo::as.yearqtr(zoo::index(t)),'%q'), mean),
           seasonal = xts::period.apply(x, INDEX=by.season(x)$lbl, apply, 2, FUN),
           tropical.seasonal = xts::period.apply(x, INDEX=by.tropicalSeason(x)$lbl, apply, 2, FUN),
           annual = mean(x.agg),
           period = mean(x.agg)
    )

  # Calc residual
  x = x.agg - x.agg.avg

  return(x)
}

cumannom <- function(x, by, ind, FUN) {
  x = annom(x, by, ind, FUN)

  return(cumsum(x))

}

# Get time index from netCDF grid
get.ncdf.date.index <- function(date.datum, date.target, ncdf.start=NA, ncdf.end=NA) {

  date.target = format(date.target, '%Y-%m-%d 00:00:00')
  ind = floor(RNetCDF::utinvcal.nc(date.datum, date.target))+1

  if (!is.na(ncdf.start) && !is.na(ncdf.end)) {
    ncdf.start = format(ncdf.start, '%Y-%m-%d 00:00:00')
    ncdf.end = format(ncdf.end, '%Y-%m-%d 00:00:00')

    ind.start = floor(RNetCDF::utinvcal.nc(date.datum, ncdf.start))+1
    ind.end = floor(RNetCDF::utinvcal.nc(date.datum, ncdf.end))+1

    filt= ind >= ind.start & ind <= ind.end

    return(ind[filt])
  } else
    return(ind)

}

get.endOfMonth <- function(dates) {
  for (i in 1:length(dates)) {
    idate = dates[i]
    imonth = as.numeric(format(idate,'%m'))
    if (imonth<12)
      dates[i] = as.Date(paste(format(idate,'%Y'), imonth+1,'01', sep='-')) - 1
    else
      dates[i] = as.Date(paste(format(idate,'%Y'), '12','31', sep='-'))
  }
  return(dates)
}

get.endOfLastMonth <- function(dates) {
  for (i in 1:length(dates)) {
    idate = dates[i]
    imonth = as.numeric(format(idate,'%m'))
    if (imonth>1)
      dates[i] = as.Date(paste(format(idate,'%Y'), imonth,'01', sep='-')) - 1
    else
      dates[i] = as.Date(paste(format(idate,'%Y'), '01','31', sep='-'))
  }
  return(dates)
}

get.Season <- function(dates, season) {
  season.end.month = switch(season,
                            summer = 2,
                            autumn = 5,
                            winter = 8,
                            spring = 11,
                            wet = 9,
                            dry = 4
  )
  date.as.month = as.numeric(format(dates,'%m'))

  # If daily, get end date of month, else assume input data is monthly,
  if (all(diff(dates)==1))
    dates.end.season = unique(get.endOfMonth(dates[which( date.as.month == season.end.month)]))
  else
    dates.end.season = dates[which( date.as.month == season.end.month)]

  # Get index to end of season dates.
  season.ind = which(dates %in% dates.end.season)

  # Combine all seasons
  season = data.frame(dates = dates[season.ind],
                      ind = season.ind,
                      lbl = season
  )

  return(season)
}

by.season <- function(dates) {

  dates = zoo::index(dates)

  seasons = rbind(get.Season(dates,'summer'),
                  get.Season(dates,'autumn'),
                  get.Season(dates,'winter'),
                  get.Season(dates,'spring')
  )
  seasons = seasons[order(seasons$dates),]
  return(seasons)
}
by.wetdrySeason <- function(dates) {

  dates = zoo::index(dates)

  seasons = rbind(get.Season(dates,'autumn'),
                  get.Season(dates,'spring')
  )
  seasons = seasons[order(seasons$dates),]
  return(seasons)
}
by.tropicalSeason <- function(dates) {

  dates = zoo::index(dates)

  seasons = c(get.Season(dates,'dry'),
              get.Season(dates,'wet')
  )
  seasons = seasons[order(seasons$dates),]
  return(seasons)
}


get.ncdf.dates <- function(date.from, date.to, date.time.step) {

  # Convert date.from and date.to to the last day of the time step.
  date.from = switch(date.time.step,
                     days = date.from,
                     months = as.Date(format( as.Date(date.from,'%Y-%m-%d'),"%Y-%m-01"), "%Y-%m-%d")
  )

  date.to = switch(date.time.step,
                   days = date.to,
                   months = get.endOfMonth(date.to)
  )

  # Build sequence of dates as required timw step
  date.target = switch(date.time.step,
                       days   = seq( from=date.from, to=date.to, by="day"),
                       months = seq( from=date.from, to=date.to, by="month"))

  # Shift dates to the end of the month.
  if (date.time.step == 'months')
    date.target = get.endOfMonth(date.target)

  # Filter to be less than today
  filt = date.target <= (Sys.Date() - 1)
  return(date.target[filt])
}

