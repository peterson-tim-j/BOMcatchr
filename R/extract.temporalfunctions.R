agg.options <- function() {
  opt = list( temporal.timestep = c('daily','weekly','monthly','quarterly', 'seasonal', 'tropical.seasonal', 'annual', 'period') ,
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
           seasonal = xts::period.apply(x, INDEX=get.Dates2Seaons(x), apply, 2, FUN),
           tropical.seasonal = xts::period.apply(x, INDEX=get.Dates2TropicSeaons(x), apply, 2, FUN),
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


