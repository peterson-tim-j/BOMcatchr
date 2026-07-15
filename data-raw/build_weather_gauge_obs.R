fnames = c( "IDCJAC0009_10286_2010_Data.csv",
            "IDCJAC0009_14932_2010_Data.csv",
            "IDCJAC0009_33195_2010_Data.csv",
            "IDCJAC0009_62100_2010_Data.csv",
            "IDCJAC0009_87163_2010_Data.csv",
            "IDCJAC0009_91126_2010_Data.csv")

fnames = file.path('data-raw', fnames)

nfiles = length(fnames)
data = data.frame(site_ID=numeric() , date=as.Date(character(0)) , precip_mmday=numeric(), tmax_C=numeric())
for (i in 1:nfiles) {

  data_tmp =  read.csv(fnames[i])

  if (nrow(data_tmp) != 365)
    stop('Read error. Source .csv data sgould have 365 rows.')

  data_tmp$Product.code <- NULL
  data_tmp$Period.over.which.rainfall.was.measured..days. <- NULL
  data_tmp$Quality <- NULL

  names(data_tmp) <- c('site_ID', 'year', 'month', 'day', 'precip_mmday')

  # Open tmax data
  fname_tmp = fnames[i]
  fname_tmp = sub('IDCJAC0009', 'IDCJAC0010', fname_tmp)
  data_tmp_tmax =  read.csv(fname_tmp)

  if (nrow(data_tmp_tmax) != 365)
    stop('Read error. Source .csv data sgould have 365 rows.')

  data_tmp_tmax$Product.code <- NULL
  data_tmp_tmax$Days.of.accumulation.of.maximum.temperature <- NULL
  data_tmp_tmax$Quality <- NULL

  names(data_tmp_tmax) <- c('site_ID', 'year', 'month', 'day', 'tmax_C')

  data_tmp = cbind(data_tmp, tmax_C = data_tmp_tmax$tmax_C)

  data_tmp$date = as.Date(paste0(data_tmp$year, '-',data_tmp$month,'-',data_tmp$day),format = "%Y-%m-%d")
  data_tmp$year <- NULL
  data_tmp$month <- NULL
  data_tmp$day <- NULL

  data = rbind(data, data_tmp)
}

data = data[, c('site_ID', 'date','precip_mmday', 'tmax_C')]

weather_gauge_obs = data
usethis::use_data(weather_gauge_obs)

