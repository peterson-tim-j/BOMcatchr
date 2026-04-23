## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----setup--------------------------------------------------------------------
library(AWAPer, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
date.from = as.Date("2010-01-01","%Y-%m-%d")
date.to = as.Date("2010-12-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')

## -----------------------------------------------------------------------------
makeNetCDF_file(ncdfFilename = ncdfFilename,
                   updateFrom = date.from,
                   updateTo = date.to,
                   vars = c('precip','tmin', 'tmax',
                   'vprp', 'solarrad'))

## -----------------------------------------------------------------------------
data("catchments")

## -----------------------------------------------------------------------------
data(constants,package='Evapotranspiration')

## -----------------------------------------------------------------------------
climateData.ET.HargreavesSamani = extractCatchmentData(ncdfFilename= ncdfFilename,
                                extractFrom= date.from,
                                extractTo= date.to,
                                vars = c('tmax', 'tmin', 'et'),
                                locations=catchments,
                                spatial.function.name='IQR',
                                ET.function='ET.HargreavesSamani',
                                ET.timestep = 'daily',
                                ET.constants= constants)

climateData.ET.JensenHaise = extractCatchmentData(ncdfFilename= ncdfFilename,
                                extractFrom= date.from,
                                extractTo= date.to,
                                vars = c('tmax', 'tmin', 'solarrad', 'et'),
                                locations=catchments,
                                spatial.function.name='IQR',
                                ET.function='ET.JensenHaise',
                                ET.timestep = 'daily',
                                ET.constants= constants)

climateData.ET.Makkink = extractCatchmentData(ncdfFilename= ncdfFilename,
                                extractFrom= date.from,
                                extractTo= date.to,
                                vars = c('tmax', 'tmin','solarrad', 'et'),
                                locations=catchments,
                                spatial.function.name='IQR',
                                ET.function='ET.Makkink',
                                ET.timestep = 'daily',
                                ET.constants= constants)

climateData.ET.McGuinnessBordne = extractCatchmentData(ncdfFilename= ncdfFilename,
                               extractFrom= date.from,
                               extractTo= date.to,
                               vars = c('tmax', 'tmin', 'et'),
                               locations=catchments,
                               spatial.function.name='IQR',
                               ET.function='ET.McGuinnessBordne',
                               ET.timestep = 'daily',
                               ET.constants= constants)

climateData.ET.MortonCRAE = extractCatchmentData(ncdfFilename= ncdfFilename,
                               extractFrom= date.from,
                               extractTo= date.to,
                               vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                               locations=catchments,
                               spatial.function.name='IQR',
                               ET.function='ET.MortonCRAE',
                               ET.timestep = 'monthly',
                               ET.constants= constants)

climateData.ET.MortonCRAE.potentialET = extractCatchmentData(ncdfFilename= ncdfFilename,
                               extractFrom= date.from,
                               extractTo= date.to,
                               vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                               locations=catchments,
                               spatial.function.name='IQR',
                               ET.function='ET.MortonCRAE',
                               ET.timestep = 'monthly',
                               ET.Mortons.est='potential ET',
                               ET.constants= constants)

climateData.ET.MortonCRAE.wetarealET = extractCatchmentData(ncdfFilename= ncdfFilename,
                                extractFrom= date.from,
                                extractTo= date.to,
                                vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                                locations=catchments,
                                spatial.function.name='IQR',
                                ET.function='ET.MortonCRAE',
                                ET.timestep = 'monthly',
                                ET.Mortons.est='wet areal ET',
                                ET.constants= constants)

climateData.ET.MortonCRAE.actualarealET = extractCatchmentData(ncdfFilename= ncdfFilename,
                                 extractFrom= date.from,
                                 extractTo= date.to,
                                 vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                                 locations=catchments,
                                 spatial.function.name='IQR',
                                 ET.function='ET.MortonCRAE',
                                 ET.timestep = 'monthly',
                                 ET.Mortons.est='actual areal ET',
                                 ET.constants= constants)

climateData.ET.MortonCRWE = extractCatchmentData(ncdfFilename= ncdfFilename,
                                 extractFrom= date.from,
                                 extractTo= date.to,
                                 vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                                 locations=catchments,
                                 spatial.function.name='IQR',
                                 ET.function='ET.MortonCRWE',
                                 ET.timestep = 'monthly',
                                 ET.Mortons.est = 'potential ET',
                                 ET.constants= constants)

climateData.ET.MortonCRWE.shallowLake = extractCatchmentData(ncdfFilename= ncdfFilename,
                                 extractFrom= date.from,
                                 extractTo= date.to,
                                 vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                                 locations=catchments,
                                 spatial.function.name='IQR',
                                 ET.function='ET.MortonCRWE',
                                 ET.timestep = 'monthly',
                                 ET.Mortons.est = 'shallow lake ET',
                                 ET.constants= constants)

climateData.ET.Turc = extractCatchmentData(ncdfFilename= ncdfFilename,
                                 extractFrom= date.from,
                                 extractTo= date.to,
                                 vars = c('tmax', 'tmin', 'solarrad', 'et'),
                                 locations=catchments,
                                 spatial.function.name='IQR',
                                 ET.function='ET.Turc',
                                 ET.timestep = 'daily',
                                 ET.constants= constants)

## -----------------------------------------------------------------------------
filt = climateData.ET.HargreavesSamani$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.HargreavesSamani$temporal.mean$year,
            climateData.ET.HargreavesSamani$temporal.mean$month,
            climateData.ET.HargreavesSamani$temporal.mean$day)
plot(d[filt], climateData.ET.HargreavesSamani$temporal.mean$et[filt],
            col='black',lty=1, xlim = c(ISOdate(2010,1,1), ISOdate(2010,12,1)),
            ylim=c(0, 30),type='l', ylab='ET [mm/d]',xlab='Date')

filt = climateData.ET.JensenHaise$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.JensenHaise$temporal.mean$year,
            climateData.ET.JensenHaise$temporal.mean$month,
            climateData.ET.JensenHaise$temporal.mean$day)
lines(d[filt], climateData.ET.JensenHaise$temporal.mean$et[filt],
            col='red',lty=1)

filt = climateData.ET.Makkink$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.Makkink$temporal.mean$year,
            climateData.ET.Makkink$temporal.mean$month,
            climateData.ET.Makkink$temporal.mean$day)
lines(d[filt], climateData.ET.Makkink$temporal.mean$et[filt],
            col='green',lty=1)

filt = climateData.ET.McGuinnessBordne$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.McGuinnessBordne$temporal.mean$year,
            climateData.ET.McGuinnessBordne$temporal.mean$month,
            climateData.ET.McGuinnessBordne$temporal.mean$day)
lines(d[filt], climateData.ET.McGuinnessBordne$temporal.mean$et[filt],
            col='blue',lty=1)

filt = climateData.ET.MortonCRAE.potentialET$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.MortonCRAE.potentialET$temporal.mean$year,
            climateData.ET.MortonCRAE.potentialET$temporal.mean$month,
            climateData.ET.MortonCRAE.potentialET$temporal.mean$day)
lines(d[filt], climateData.ET.MortonCRAE.potentialET$temporal.mean$et[filt],
            col='black',lty=2)

filt = climateData.ET.MortonCRAE.wetarealET$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.MortonCRAE.wetarealET$temporal.mean$year,
            climateData.ET.MortonCRAE.wetarealET$temporal.mean$month,
            climateData.ET.MortonCRAE.wetarealET$temporal.mean$day)
lines(d[filt], climateData.ET.MortonCRAE.wetarealET$temporal.mean$et[filt],
            col='red',lty=2)

filt = climateData.ET.MortonCRAE.actualarealET$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.MortonCRAE.actualarealET$temporal.mean$year,
            climateData.ET.MortonCRAE.actualarealET$temporal.mean$month,
            climateData.ET.MortonCRAE.actualarealET$temporal.mean$day)
lines(d[filt], climateData.ET.MortonCRAE.actualarealET$temporal.mean$et[filt],
            col='green',lty=2)

filt = climateData.ET.MortonCRWE$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.MortonCRWE$temporal.mean$year,
            climateData.ET.MortonCRWE$temporal.mean$month,
            climateData.ET.MortonCRWE$temporal.mean$day)
lines(d[filt], climateData.ET.MortonCRWE$temporal.mean$et[filt],
            col='blue',lty=2)

filt = climateData.ET.MortonCRWE.shallowLake$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.MortonCRWE.shallowLake$temporal.mean$year,
            climateData.ET.MortonCRWE.shallowLake$temporal.mean$month,
            climateData.ET.MortonCRWE.shallowLake$temporal.mean$day)
lines(d[filt], climateData.ET.MortonCRWE.shallowLake$temporal.mean$et[filt],
            col='black',lty=3)

filt = climateData.ET.Turc$temporal.mean$Location.ID==407214
d = ISOdate(climateData.ET.Turc$temporal.mean$year,
            climateData.ET.Turc$temporal.mean$month,
            climateData.ET.Turc$temporal.mean$day)
lines(d[filt], climateData.ET.Turc$temporal.mean$et[filt],
            col='red',lty=3)

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

