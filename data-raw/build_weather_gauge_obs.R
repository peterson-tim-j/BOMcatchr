fnames = c( c(
  "IDCJAC0009_9741_2010_Data.csv",
  "IDCJAC0009_15603_2010_Data.csv",
  "IDCJAC0009_33247_2010_Data.csv",
  "IDCJAC0009_70263_2010_Data.csv",
  "IDCJAC0009_79103_2010_Data.csv",
  "IDCJAC0009_91259_2010_Data.csv"
))

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

# Create weather location data
weather_gauge_loc = data.frame(site_ID = numeric(),
                               longitude = numeric(),
                               latitude = numeric()
                               )


weather_gauge_loc[1, 'site_ID'] = 33247
weather_gauge_loc[1, 'longitude'] = 148.555
weather_gauge_loc[1, 'latitude'] = -20.4925

weather_gauge_loc[2, 'site_ID'] = 15603
weather_gauge_loc[2, 'longitude'] = 133.3027
weather_gauge_loc[2, 'latitude'] = -25.8428

weather_gauge_loc[3, 'site_ID'] = 70263
weather_gauge_loc[3, 'longitude'] = 149.7034
weather_gauge_loc[3, 'latitude'] = -34.7495

weather_gauge_loc[4, 'site_ID'] = 9741
weather_gauge_loc[4, 'longitude'] = 117.8022
weather_gauge_loc[4, 'latitude'] = -34.9414

weather_gauge_loc[5, 'site_ID'] = 79103
weather_gauge_loc[5, 'longitude'] = 142.6039
weather_gauge_loc[5, 'latitude'] = -37.295

weather_gauge_loc[6, 'site_ID'] = 91259
weather_gauge_loc[6, 'longitude'] = 145.1517
weather_gauge_loc[6, 'latitude'] = -41.1492

usethis::use_data(weather_gauge_loc)
