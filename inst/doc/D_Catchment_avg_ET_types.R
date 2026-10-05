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
status = grid_build(ncdfFilename = ncdfFilename,
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
derived_var_fns <- list(ET_HS = list( FUN = 'ET_HargreavesSamani',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'daily'),
                        ET_JH = list( FUN = 'ET_JensenHaise',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'daily'),
                        ET_M =  list( FUN = 'ET_Makkink',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'daily'),
                        ET_MB = list( FUN = 'ET_McGuinnessBordne',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'daily'),
                        ET_T =  list( FUN = 'ET_Turc',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'daily'),
                        ET_CRAE_PET = list( FUN = 'ET_MortonCRAE',
                                            ET_Mortons_est = 'potential ET',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'monthly'),
                        ET_CRAE_aPET = list(FUN = 'ET_MortonCRAE',
                                            ET_Mortons_est = 'wet areal ET',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'monthly'),
                        ET_CRAE_aET = list( FUN = 'ET_MortonCRAE',
                                            ET_Mortons_est = 'actual areal ET',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'monthly'),
                        ET_CRWE_lake = list(FUN = 'ET_MortonCRWE',
                                            ET_Mortons_est = 'shallow lake ET',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'monthly'),
                        ET_CRWE_PET  = list(FUN = 'ET_MortonCRWE',
                                            ET_Mortons_est = 'potential ET',
                                            ET_missing_method = 'neighbouring average',
                                            ET_abnormal_method = 'neighbouring average',
                                            ET_constants = constants,
                                            ET_timestep = 'monthly')
                        )

## -----------------------------------------------------------------------------
climateData = extract_data(ncdfFilename=ncdfFilename,
                               extractFrom = date.from,
                               extractTo = date.to,
                               vars = c('precip', 'tmin', 'tmax', 'vprp_3pm','solarrad'),
                               vars_derived = derived_var_fns,
                               locations = catch,
                               temporal.timestep = 'daily',
                               temporal.fn.inner ='sum',
                               spatial.fn = 'IQR')


## ----class.source = 'fold-hide'-----------------------------------------------
par(mfrow=c(2,1))
for (i in 1:length(site_ID)) {
  filt = climateData$temporal$Location.ID == site_ID[i]

  d = ISOdate(climateData$temporal$year,
              climateData$temporal$month,
              climateData$temporal$day)
  plot(d[filt],
       climateData$temporal$ET_HS[filt],
       col='black',
       lty=1,
       xlim = c(ISOdate(2010,1,1),
       ISOdate(2010,12,1)),
       ylim=c(0, 30),
       type='l',
       ylab='ET [mm/d]',
       xlab='Date',
       main=paste('Catchment ID',site_ID[i])
       )

  lines(d[filt],
        climateData$temporal$ET_JH[filt],
        col='red',
        lty=1)

  lines(d[filt],
        climateData$temporal$ET_M[filt],
        col='green',
        lty=1)

  lines(d[filt],
        climateData$temporal$ET_MB[filt],
        col='blue',
        lty=1)

  lines(d[filt], climateData$temporal$ET_T[filt],
        col='purple',
        lty=3)

  lines(d[filt],
        climateData$temporal$ET_CRAE_PET[filt],
        col='black',
        lty=2)

  lines(d[filt],
        climateData$temporal$ET_CRAE_aPET[filt],
        col='red',
        lty=2)

  lines(d[filt],
        climateData$temporal$ET_CRAE_aET[filt],
        col='green',
        lty=2)

  lines(d[filt],
        climateData$temporal$ET_CRWE_lake[filt],
        col='blue',
        lty=2)

  lines(d[filt],
        climateData$temporal$ET_CRWE_PET[filt],
        col='black',
        lty=3)

}

legend(x='topright', legend=c(
  'Hargreaves Samani (ref. crop)',
  'Jensen Haise (PET)',
  'Makkink (ref. crop)',
  'McGuinness Bordne (PET)',
  'Turc (ref. crop, non-humid)',
  'Morton CRAE (PET)',
  'Morton CRAE (wet areal ET)',
  'Morton CRAE (actual areal ET)',
  'Morton CRWE (PET)',
  'Morton CRWE (shallow Lake)'),
  lty = c(1,1,1,1,1,2,2,2,3,3),
  col=c('black','red','green','blue','purple','black','red','green','blue','black')
)

