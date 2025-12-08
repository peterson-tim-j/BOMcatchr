# Extract various measures of evapotranspiration

``` r
library(AWAPer, warn.conflicts = FALSE)
```

This example calculates and plot various estimates of
evaportranspiration. Ten different estimates of area weighted
evapotranspiration over one year at catchment 407214 (Victoria,
Australia) are derived. \## Make netCDF files Like the other vignettes,
the netCDF data grids need to be built.

First, let’s define the start and end dates for data grids and the file
names.

``` r
date.from = as.Date("2010-01-01","%Y-%m-%d")
date.to = as.Date("2010-12-31","%Y-%m-%d")

ncdfFilename = tempfile(fileext='.nc')
ncdfSolarFilename = tempfile(fileext='.nc')
```

Next, let’s make the data grids over this period.

``` r
makeNetCDF_file(ncdfFilename = ncdfFilename,
                   ncdfSolarFilename = ncdfSolarFilename,
                   updateFrom = date.from,
                   updateTo = date.to)
#> Starting to build both netCDF files.
#> ... Testing downloading of AWAP precip. grid
#> ... Getting grid gemoetry from file.
#> ... Deleting /home/runner/work/AWAPer/AWAPer/vignettes/precip.20000101.grid.gz
#> ... Testing downloading of AWAP tmin grid
#> ... Testing downloading of AWAP tmax grid
#> ... Testing downloading of AWAP vapour pressure grid
#> ... Testing downloading of AWAP solar grid
#> ... Getting grid gemoetry from file.
#> ... Deleting /home/runner/work/AWAPer/AWAPer/vignettes/solarrad.20000101.grid.gz
#> ... Building AWAP netcdf file.
#>     NetCDF data will be updated from  2010-01-01  to  2010-12-31
#> ... Downloading non-solar data and importing to netcdf file:
#> Syncing 365 days of data to netCDF file. The time point to be synched is: 2010-12-31
#>     NetCDF Solar data will be updated from  2010-01-01  to  2010-12-31
#> ... Downloading solar data and importing to netcdf file:
#> Syncing 365 days of data to netCDF file. The time point to be synched is: 2010-12-31
#> Data construction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:26:21
#> $ncdfFilename
#> [1] "/tmp/RtmpzGMmEa/file67655ae21b02.nc"
#> 
#> $ncdfSolarFilename
#> [1] "/tmp/RtmpzGMmEa/file676531b7f1f0.nc"
```

## Load a catchment boundary

Now that we have the meteorological data we can begin extracting data
for the catchment. Here the catchment boundaries built into the package
are used.

``` r
data("catchments")
```

## Extract daily precipitation and PET data

Next, the 11 different measures of evapotranspiration that can be
derived from the available gridded data are calculated for 12 months.

The estimation of ET uses the *evapotranspiration* package. It requires
a set of constants, which are loaded as follows.

``` r
data(constants,package='Evapotranspiration')
```

Next, all 11 ET measures are derived. For each measure, only the
following commands change: *ET.function* , *ET.timestep* and
*ET.Mortons.est* (when Morton’s estimate is derived).

``` r
climateData.ET.HargreavesSamani = extractCatchmentData(ncdfFilename= ncdfFilename,
                                ncdfSolarFilename= ncdfSolarFilename,
                                extractFrom= date.from,
                                extractTo= date.to,
                                locations=catchments,
                                spatial.function.name='IQR',
                                ET.function='ET.HargreavesSamani',
                                ET.timestep = 'daily',
                                ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:11

climateData.ET.JensenHaise = extractCatchmentData(ncdfFilename= ncdfFilename,
                                ncdfSolarFilename= ncdfSolarFilename,
                                extractFrom= date.from,
                                extractTo= date.to,
                                locations=catchments,
                                spatial.function.name='IQR',
                                ET.function='ET.JensenHaise',
                                ET.timestep = 'daily',
                                ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:11

climateData.ET.Makkink = extractCatchmentData(ncdfFilename= ncdfFilename,
                                ncdfSolarFilename= ncdfSolarFilename,
                                extractFrom= date.from,
                                extractTo= date.to,
                                locations=catchments,
                                spatial.function.name='IQR',
                                ET.function='ET.Makkink',
                                ET.timestep = 'daily',
                                ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:11

climateData.ET.McGuinnessBordne = extractCatchmentData(ncdfFilename= ncdfFilename,
                               ncdfSolarFilename= ncdfSolarFilename,
                               extractFrom= date.from,
                               extractTo= date.to,
                               locations=catchments,
                               spatial.function.name='IQR',
                               ET.function='ET.McGuinnessBordne',
                               ET.timestep = 'daily',
                               ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:14

climateData.ET.MortonCRAE = extractCatchmentData(ncdfFilename= ncdfFilename,
                               ncdfSolarFilename= ncdfSolarFilename,
                               extractFrom= date.from,
                               extractTo= date.to,
                               locations=catchments,
                               spatial.function.name='IQR',
                               ET.function='ET.MortonCRAE',
                               ET.timestep = 'monthly',
                               ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:18

climateData.ET.MortonCRAE.potentialET = extractCatchmentData(ncdfFilename= ncdfFilename,
                               ncdfSolarFilename= ncdfSolarFilename,
                               extractFrom= date.from,
                               extractTo= date.to,
                               locations=catchments,
                               spatial.function.name='IQR',
                               ET.function='ET.MortonCRAE',
                               ET.timestep = 'monthly',
                               ET.Mortons.est='potential ET',
                               ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:17

climateData.ET.MortonCRAE.wetarealET = extractCatchmentData(ncdfFilename= ncdfFilename,
                                ncdfSolarFilename= ncdfSolarFilename,
                                extractFrom= date.from,
                                extractTo= date.to,
                                locations=catchments,
                                spatial.function.name='IQR',
                                ET.function='ET.MortonCRAE',
                                ET.timestep = 'monthly',
                                ET.Mortons.est='wet areal ET',
                                ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:16

climateData.ET.MortonCRAE.actualarealET = extractCatchmentData(ncdfFilename= ncdfFilename,
                                 ncdfSolarFilename= ncdfSolarFilename,
                                 extractFrom= date.from,
                                 extractTo= date.to,
                                 locations=catchments,
                                 spatial.function.name='IQR',
                                 ET.function='ET.MortonCRAE',
                                 ET.timestep = 'monthly',
                                 ET.Mortons.est='actual areal ET',
                                 ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:17

climateData.ET.MortonCRWE = extractCatchmentData(ncdfFilename= ncdfFilename,
                                 ncdfSolarFilename= ncdfSolarFilename,
                                 extractFrom= date.from,
                                 extractTo= date.to,
                                 locations=catchments,
                                 spatial.function.name='IQR',
                                 ET.function='ET.MortonCRWE',
                                 ET.timestep = 'monthly',
                                 ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:17

climateData.ET.MortonCRWE.shallowLake = extractCatchmentData(ncdfFilename= ncdfFilename,
                                 ncdfSolarFilename= ncdfSolarFilename,
                                 extractFrom= date.from,
                                 extractTo= date.to,
                                 locations=catchments,
                                 spatial.function.name='IQR',
                                 ET.function='ET.MortonCRWE',
                                 ET.timestep = 'monthly',
                                 ET.Mortons.est = 'shallow lake ET',
                                 ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:16

climateData.ET.Turc = extractCatchmentData(ncdfFilename= ncdfFilename,
                                 ncdfSolarFilename= ncdfSolarFilename,
                                 extractFrom= date.from,
                                 extractTo= date.to,
                                 locations=catchments,
                                 spatial.function.name='IQR',
                                 ET.function='ET.Turc',
                                 ET.timestep = 'daily',
                                 ET.constants= constants);
#> Extraction data summary:
#>     NetCDF non-solar radiation climate data exists from 2010-01-01 to 2010-12-31
#>     NetCDF solar radiation data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#>     WARNING: The extraction duration is < 2 years and getET = TRUE.
#>              Hence, ET.missing_method and ET.abnormal_method is changed to "neighbouring average".
#> Starting data extraction:
#> ... Building catchment weights
#> ... Extracted DEM elevations from AWS.
#> Mosaicing & Projecting
#> Note: Elevation units are in meters
#> ... Starting to extract data across all locations:
#> ... Calculating mean daily solar radiation <1990-1-1
#> ... Linearly interpolating gaps in daily solar.
#> ... Calculating area weighted daily data.
#>     Working on ET for location 1 of 2
#>     Working on ET for location 2 of 2
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:14
```

Next each estimate is plotted over time.

``` r
filt = climateData.ET.HargreavesSamani$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.HargreavesSamani$catchmentTemporal.mean$year,
            climateData.ET.HargreavesSamani$catchmentTemporal.mean$month,
            climateData.ET.HargreavesSamani$catchmentTemporal.mean$day)
plot(d[filt], climateData.ET.HargreavesSamani$catchmentTemporal.mean$ET_mm[filt],
            col='black',lty=1, xlim = c(ISOdate(2010,1,1), ISOdate(2010,12,1)),
            ylim=c(0, 30),type='l', ylab='ET [mm/d]',xlab='Date')

filt = climateData.ET.JensenHaise$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.JensenHaise$catchmentTemporal.mean$year,
            climateData.ET.JensenHaise$catchmentTemporal.mean$month,
            climateData.ET.JensenHaise$catchmentTemporal.mean$day)
lines(d[filt], climateData.ET.JensenHaise$catchmentTemporal.mean$ET_mm[filt],
            col='red',lty=1)

filt = climateData.ET.Makkink$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.Makkink$catchmentTemporal.mean$year,
            climateData.ET.Makkink$catchmentTemporal.mean$month,
            climateData.ET.Makkink$catchmentTemporal.mean$day)
lines(d[filt], climateData.ET.Makkink$catchmentTemporal.mean$ET_mm[filt],
            col='green',lty=1)

filt = climateData.ET.McGuinnessBordne$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.McGuinnessBordne$catchmentTemporal.mean$year,
            climateData.ET.McGuinnessBordne$catchmentTemporal.mean$month,
            climateData.ET.McGuinnessBordne$catchmentTemporal.mean$day)
lines(d[filt], climateData.ET.McGuinnessBordne$catchmentTemporal.mean$ET_mm[filt],
            col='blue',lty=1)

filt = climateData.ET.MortonCRAE.potentialET$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.MortonCRAE.potentialET$catchmentTemporal.mean$year,
            climateData.ET.MortonCRAE.potentialET$catchmentTemporal.mean$month,
            climateData.ET.MortonCRAE.potentialET$catchmentTemporal.mean$day)
lines(d[filt], climateData.ET.MortonCRAE.potentialET$catchmentTemporal.mean$ET_mm[filt],
            col='black',lty=2)

filt = climateData.ET.MortonCRAE.wetarealET$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.MortonCRAE.wetarealET$catchmentTemporal.mean$year,
            climateData.ET.MortonCRAE.wetarealET$catchmentTemporal.mean$month,
            climateData.ET.MortonCRAE.wetarealET$catchmentTemporal.mean$day)
lines(d[filt], climateData.ET.MortonCRAE.wetarealET$catchmentTemporal.mean$ET_mm[filt],
            col='red',lty=2)

filt = climateData.ET.MortonCRAE.actualarealET$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.MortonCRAE.actualarealET$catchmentTemporal.mean$year,
            climateData.ET.MortonCRAE.actualarealET$catchmentTemporal.mean$month,
            climateData.ET.MortonCRAE.actualarealET$catchmentTemporal.mean$day)
lines(d[filt], climateData.ET.MortonCRAE.actualarealET$catchmentTemporal.mean$ET_mm[filt],
            col='green',lty=2)

filt = climateData.ET.MortonCRWE$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.MortonCRWE$catchmentTemporal.mean$year,
            climateData.ET.MortonCRWE$catchmentTemporal.mean$month,
            climateData.ET.MortonCRWE$catchmentTemporal.mean$day)
lines(d[filt], climateData.ET.MortonCRWE$catchmentTemporal.mean$ET_mm[filt],
            col='blue',lty=2)

filt = climateData.ET.MortonCRWE.shallowLake$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.MortonCRWE.shallowLake$catchmentTemporal.mean$year,
            climateData.ET.MortonCRWE.shallowLake$catchmentTemporal.mean$month,
            climateData.ET.MortonCRWE.shallowLake$catchmentTemporal.mean$day)
lines(d[filt], climateData.ET.MortonCRWE.shallowLake$catchmentTemporal.mean$ET_mm[filt],
            col='black',lty=3)

filt = climateData.ET.Turc$catchmentTemporal.mean$CatchID==407214
d = ISOdate(climateData.ET.Turc$catchmentTemporal.mean$year,
            climateData.ET.Turc$catchmentTemporal.mean$month,
            climateData.ET.Turc$catchmentTemporal.mean$day)
lines(d[filt], climateData.ET.Turc$catchmentTemporal.mean$ET_mm[filt],
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
  'Morton CRWE (shallowLake)',
  'Turc (ref. crop, non-humid'),
  lty = c(1,1,1,1,2,2,2,2,3,3),
  col=c('black','red','green','blue','black','red','green','blue','black','red')
)
```

![](D_Catchment_avg_ET_types_files/figure-html/unnamed-chunk-7-1.png)
