# Time aggregation internal functions
#-------------------------------------------

agg.options <- function() {
  opt = list( temporal.timestep = c('daily','weekly','monthly','quarterly', 'seasonal', 'wetdry_seasonal','tropical_seasonal', 'annual', 'period') ,
              temporal.function.name = c('sum', 'mean', 'min', 'max', 'median', 'sd', 'var', 'IQR'),
              spatial.function.name = c() )
  return(opt)
}

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

# Aggregate source time step (daily or monthly) to user defined time step
agg.by <- function(x, by, ind, FUN = 'sum') {
  x.agg <-
    switch(by,
           daily = x,
           weekly            = xts::apply.weekly(x, apply, 2, FUN),
           monthly           = xts::apply.monthly(x, apply, 2, FUN),
           quarterly         = xts::apply.quarterly(x, apply, 2, FUN),
           annual            = xts::apply.yearly(x, apply, 2, FUN),
           seasonal          = xts::period.apply(x, INDEX = by_season(x, 'seasonal', end.of.season = T)$ind          , apply, 2, FUN),
           wetdry_seasonal   = xts::period.apply(x, INDEX = by_season(x, 'wetdry_seasonal', end.of.season = T)$ind  , apply, 2, FUN),
           tropical_seasonal = xts::period.apply(x, INDEX = by_season(x, 'tropical_seasonal', end.of.season = T)$ind, apply, 2, FUN),
           period = xts::period.apply(x, INDEX=ind, apply, 2, FUN),
    )
  return(x.agg)
}

annom <- function(x, by, ind, FUN) {

  # Aggregate using user defined time step and function
  x.agg <- agg.by(x, by = by, ind, FUN = FUN)

  # Get timestep lalels for agg data
  dates = zoo::index(x.agg)
  x.agg.fmt <- switch(by,
                      daily             = format(dates, "%j"),
                      weekly            = format(dates, '%V'),
                      monthly           = format(dates, "%m"),
                      quarterly         = format(dates, '%q'),
                      seasonal          = by_season(dates, 'seasonal', end.of.season = F)$lbl,
                      wetdry_seasonal   = by_season(dates, 'wetdry_seasonal', end.of.season = F)$lbl,
                      tropical_seasonal = by_season(dates, 'tropical_seasonal', end.of.season = F)$lbl,
                      annual = 'year',
                      period = 'period'
  )

  # Get zoo index to each within year time step
  x.agg.avg <- aggregate(x.agg, by=x.agg.fmt, mean)
  x.agg.avg.df = data.frame(avg = zoo::coredata(x.agg.avg), row.names = zoo::index(x.agg.avg))

  # M<ake df of zoo agg data and timestep labels
  x.agg.df = data.frame(agg = zoo::coredata(x.agg), lbl = x.agg.fmt)

  # Map seasonal means to all agg dates and bind
  x.agg.avg.df = x.agg.avg.df[x.agg.df$lbl,]

  # Calc residual
  x.agg.df[,'lbl'] <- NULL
  zoo::coredata(x.agg) <- as.matrix(x.agg.df - x.agg.avg.df)
  return(x.agg)
}

cumannom <- function(x, by, ind, FUN) {
  x = annom(x, by, ind, FUN)

  return(apply(x,2, cumsum))

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

by_season <- function(dates, season, end.of.season = T) {
  season.month = switch(season,
                        seasonal = data.frame(lbl = c('summer', 'summer', 'autumn', 'winter', 'spring'),
                                              from = c(12, 0, 3, 6, 9),
                                              to = c(Inf, 2, 5, 8,11)),
                        wetdry_seasonal = data.frame(lbl = c('dry', 'dry', 'wet'),
                                                     from = c(12, 0, 6),
                                                     to = c(Inf, 5, 11)),
                        tropical_seasonal = data.frame(lbl = c('dry', 'wet', 'wet'),
                                                       from = c(5, 10, 0),
                                                       to = c(9, Inf, 4))
                        )

  if ('zoo' %in% class(dates))
    dates = zoo::index(dates)

  date.as.month = as.numeric(format(dates,'%m'))
  dates = data.frame(dates, lbl=NA, ind = 1:length(dates))
  for (i in 1:nrow(season.month)) {
    ind = date.as.month >= season.month$from[i] & date.as.month <= season.month$to[i]
    dates[ind,'lbl'] = season.month$lbl[i]
  }

  if (end.of.season) {
    ind = rle(dates$lbl)$lengths
    ind = cumsum(ind)
    dates = dates[ind,]
  }
  return(dates)
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

