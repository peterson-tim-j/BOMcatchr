## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(BOMcatchr)

## -----------------------------------------------------------------------------
date.from = as.Date("2010-01-01","%Y-%m-%d")
date.to = as.Date("2010-12-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
grid_build(ncdfFilename = ncdfFilename,
                   updateFrom = date.from,
                   updateTo = date.to,
                   vars = c('precip','tmin', 'tmax',
                   'vprp_3pm', 'solarrad'))

## -----------------------------------------------------------------------------
catch <- extract_locations('catchment',c('407214','407220'))
site_ID <- catch$stationno

## -----------------------------------------------------------------------------
data(constants,package='Evapotranspiration')

## -----------------------------------------------------------------------------
climateData.ET.HargreavesSamani = extract_data(ncdfFilename = ncdfFilename,
                                extractFrom = date.from,
                                extractTo = date.to,
                                vars = c('tmax', 'tmin', 'et'),
                                locations = catch,
                                spatial.fn = 'IQR',
                                ET.function = 'ET.HargreavesSamani',
                                ET.timestep = 'daily',
                                ET.constants = constants)

climateData.ET.JensenHaise = extract_data(ncdfFilename = ncdfFilename,
                                extractFrom = date.from,
                                extractTo = date.to,
                                vars = c('tmax', 'tmin', 'solarrad', 'et'),
                                locations = catch,
                                spatial.fn = 'IQR',
                                ET.function = 'ET.JensenHaise',
                                ET.timestep = 'daily',
                                ET.constants = constants)

climateData.ET.Makkink = extract_data(ncdfFilename = ncdfFilename,
                                extractFrom = date.from,
                                extractTo = date.to,
                                vars = c('tmax', 'tmin','solarrad', 'et'),
                                locations = catch,
                                spatial.fn = 'IQR',
                                ET.function = 'ET.Makkink',
                                ET.timestep = 'daily',
                                ET.constants = constants)

climateData.ET.McGuinnessBordne = extract_data(ncdfFilename = ncdfFilename,
                               extractFrom = date.from,
                               extractTo = date.to,
                               vars = c('tmax', 'tmin', 'et'),
                               locations = catch,
                               spatial.fn = 'IQR',
                               ET.function = 'ET.McGuinnessBordne',
                               ET.timestep = 'daily',
                               ET.constants = constants)

climateData.ET.MortonCRAE = extract_data(ncdfFilename = ncdfFilename,
                               extractFrom = date.from,
                               extractTo = date.to,
                               vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                               locations = catch,
                               spatial.fn = 'IQR',
                               ET.function = 'ET.MortonCRAE',
                               ET.timestep = 'monthly',
                               ET.constants = constants)

climateData.ET.MortonCRAE.potentialET = extract_data(ncdfFilename = ncdfFilename,
                               extractFrom = date.from,
                               extractTo = date.to,
                               vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                               locations = catch,
                               spatial.fn = 'IQR',
                               ET.function = 'ET.MortonCRAE',
                               ET.timestep = 'monthly',
                               ET.Mortons.est = 'potential ET',
                               ET.constants = constants)

climateData.ET.MortonCRAE.wetarealET = extract_data(ncdfFilename = ncdfFilename,
                               extractFrom = date.from,
                               extractTo = date.to,
                               vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                               locations = catch,
                               spatial.fn = 'IQR',
                               ET.function = 'ET.MortonCRAE',
                               ET.timestep = 'monthly',
                               ET.Mortons.est = 'wet areal ET',
                               ET.constants = constants)

climateData.ET.MortonCRAE.actualarealET = extract_data(ncdfFilename = ncdfFilename,
                               extractFrom = date.from,
                               extractTo = date.to,
                               vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                               locations = catch,
                               spatial.fn = 'IQR',
                               ET.function = 'ET.MortonCRAE',
                               ET.timestep = 'monthly',
                               ET.Mortons.est = 'actual areal ET',
                               ET.constants = constants)

climateData.ET.MortonCRWE = extract_data(ncdfFilename = ncdfFilename,
                               extractFrom = date.from,
                               extractTo = date.to,
                               vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                               locations = catch,
                               spatial.fn = 'IQR',
                               ET.function = 'ET.MortonCRWE',
                               ET.timestep = 'monthly',
                               ET.Mortons.est = 'potential ET',
                               ET.constants = constants)

climateData.ET.MortonCRWE.shallowLake = extract_data(ncdfFilename = ncdfFilename,
                               extractFrom = date.from,
                               extractTo = date.to,
                               vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                               locations = catch,
                               spatial.fn = 'IQR',
                               ET.function = 'ET.MortonCRWE',
                               ET.timestep = 'monthly',
                               ET.Mortons.est = 'shallow lake ET',
                               ET.constants = constants)

climateData.ET.Turc = extract_data(ncdfFilename = ncdfFilename,
                               extractFrom = date.from,
                               extractTo = date.to,
                               vars = c('tmax', 'tmin', 'solarrad', 'et'),
                               locations = catch,
                               spatial.fn = 'IQR',
                               ET.function = 'ET.Turc',
                               ET.timestep = 'daily',
                               ET.constants = constants)

## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
for (i in 1:length(site_ID)) {
  filt = climateData.ET.HargreavesSamani$temporal$Location.ID == site_ID[i]
  d = ISOdate(climateData.ET.HargreavesSamani$temporal$year,
              climateData.ET.HargreavesSamani$temporal$month,
              climateData.ET.HargreavesSamani$temporal$day)
  plot(d[filt], climateData.ET.HargreavesSamani$temporal$et[filt],
              col='black',lty=1, xlim = c(ISOdate(2010,1,1), ISOdate(2010,12,1)),
              ylim=c(0, 30),type='l', ylab='ET [mm/d]',xlab='Date')

  filt = climateData.ET.JensenHaise$temporal$Location.ID == site_ID[i]
  d = ISOdate(climateData.ET.JensenHaise$temporal$year,
              climateData.ET.JensenHaise$temporal$month,
              climateData.ET.JensenHaise$temporal$day)
  lines(d[filt], climateData.ET.JensenHaise$temporal$et[filt],
              col='red',lty=1)

  filt = climateData.ET.Makkink$temporal$Location.ID==407214
  d = ISOdate(climateData.ET.Makkink$temporal$year,
              climateData.ET.Makkink$temporal$month,
              climateData.ET.Makkink$temporal$day)
  lines(d[filt], climateData.ET.Makkink$temporal$et[filt],
              col='green',lty=1)

  filt = climateData.ET.McGuinnessBordne$temporal$Location.ID == site_ID[i]
  d = ISOdate(climateData.ET.McGuinnessBordne$temporal$year,
              climateData.ET.McGuinnessBordne$temporal$month,
              climateData.ET.McGuinnessBordne$temporal$day)
  lines(d[filt], climateData.ET.McGuinnessBordne$temporal$et[filt],
              col='blue',lty=1)

  filt = climateData.ET.MortonCRAE.potentialET$temporal$Location.ID == site_ID[i]
  d = ISOdate(climateData.ET.MortonCRAE.potentialET$temporal$year,
              climateData.ET.MortonCRAE.potentialET$temporal$month,
              climateData.ET.MortonCRAE.potentialET$temporal$day)
  lines(d[filt], climateData.ET.MortonCRAE.potentialET$temporal$et[filt],
              col='black',lty=2)

  filt = climateData.ET.MortonCRAE.wetarealET$temporal$Location.ID == site_ID[i]
  d = ISOdate(climateData.ET.MortonCRAE.wetarealET$temporal$year,
              climateData.ET.MortonCRAE.wetarealET$temporal$month,
              climateData.ET.MortonCRAE.wetarealET$temporal$day)
  lines(d[filt], climateData.ET.MortonCRAE.wetarealET$temporal$et[filt],
              col='red',lty=2)

  filt = climateData.ET.MortonCRAE.actualarealET$temporal$Location.ID == site_ID[i]
  d = ISOdate(climateData.ET.MortonCRAE.actualarealET$temporal$year,
              climateData.ET.MortonCRAE.actualarealET$temporal$month,
              climateData.ET.MortonCRAE.actualarealET$temporal$day)
  lines(d[filt], climateData.ET.MortonCRAE.actualarealET$temporal$et[filt],
              col='green',lty=2)

  filt = climateData.ET.MortonCRWE$temporal$Location.ID == site_ID[i]
  d = ISOdate(climateData.ET.MortonCRWE$temporal$year,
              climateData.ET.MortonCRWE$temporal$month,
              climateData.ET.MortonCRWE$temporal$day)
  lines(d[filt], climateData.ET.MortonCRWE$temporal$et[filt],
              col='blue',lty=2)

  filt = climateData.ET.MortonCRWE.shallowLake$temporal$Location.ID == site_ID[i]
  d = ISOdate(climateData.ET.MortonCRWE.shallowLake$temporal$year,
              climateData.ET.MortonCRWE.shallowLake$temporal$month,
              climateData.ET.MortonCRWE.shallowLake$temporal$day)
  lines(d[filt], climateData.ET.MortonCRWE.shallowLake$temporal$et[filt],
              col='black',lty=3)

  filt = climateData.ET.Turc$temporal$Location.ID == site_ID[i]
  d = ISOdate(climateData.ET.Turc$temporal$year,
              climateData.ET.Turc$temporal$month,
              climateData.ET.Turc$temporal$day)
  lines(d[filt], climateData.ET.Turc$temporal$et[filt],
              col='red',lty=3)

}

legend(x='topright', legend=c(
  'Hargreaves Samani (ref. crop)',
  'Jensen Haise (PET)',
  'Makkink (ref. crop)',
  'McGuinness Bordne (PET)',
  'Morton CRAE (PET)',
  'Morton CRAE (wet areal ET)',
  'Morton CRAE (actual areal ET)',
  'Morton CRWE (PET)',
  'Morton CRWE (shallow Lake)',
  'Turc (ref. crop, non-humid)'),
  lty = c(1,1,1,1,2,2,2,2,3,3),
  col=c('black','red','green','blue','black','red','green','blue','black','red')
)

