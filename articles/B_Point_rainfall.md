# Extract daily and monthly point precipitation

``` r
library(BOMcatchr)
```

This example shows how to build the required data files and then extract
the daily rainfall at two sites. Here, one of the sites is at a rainfall
gauge and the results are compared against observations.

## Make netCDF file

The first step is to create the netCDF files. Here the netCDF file is
created and only for daily and monthly precipitation data. Note, the
monthly precipitation product can slightly differ from the monthly sum
of the daily precipitation product. See for details.

Here the data is only downloaded between the dates *dataFrom* and
*dataTo*. If the latter two dates were not input then data would be
downloaded from 1/1/1900 to yesterday.

``` r
dataFrom = as.Date("2010-01-01","%Y-%m-%d")
dataTo = as.Date("2010-12-31","%Y-%m-%d")
```

The netCDF file contains grids of daily rainfall for all of Australia
and is used below to extract data at points of interest. Often users run
*build.grids* once to build netCDF data files that contain all variables
over the entire record length (which requires ~5GB disk storage) and
then use the netCDFs grids for multiple projects, rather than
re-building the netCDF for each project. Also, if *build.grids* is run
with the the netCDF file name pointing to existing files and
*updateFrom=NA* then the netCDF files will be updated to yesterday.

To start to create the grids, the file name for the netCDF grids needs
to be defined. Here we’ll just use a temporary file. You should change
this to a non-temporary file name and folder.

``` r
ncdfFilename = tempfile(fileext='.nc')
```

Now with the inputs defined we can download the gridded data and build
the netCDF file. Note, here *vars* is set to only download daily
precipitation.

``` r
fnames = build.grids(ncdfFilename = ncdfFilename,
                         updateFrom = dataFrom,
                         updateTo = dataTo,
                         vars = c('precip', 'precip.monthly'))
#> ... Testing downloading of each variable.
#>     Testing precip grid data.
#>     Testing precip.monthly grid data.
#> ... NetCDF file will be updated as follows:
#>        - New variables to add: precip  precip.monthly
#>        - Existing variables to modify: (none)
#>        - Data will be updated from  2010-01-01  to  2010-12-31
#> ... Downloading data for each variable and importing to netcdf file:
#> Data construction FINISHED.
#> Summary of time points successfully imported (and errors).
#>                Imported Errors
#> precip              365      0
#> precip.monthly       12      0
#> Total run time (DD:HH:MM:SS): 00:00:04:10
```

## Set the points for data extraction

with the data grids built, let’s set two data points to extract data at.
The locations are at a groundwater bore and one rainfall gauge. Below,
the coordinates are defined and then they’re convert to a spatial object
and set projection to GDA94.

``` r
coordinates.data = data.frame( ID =c('Bore-10084446','Rain-63005'),
                               Longitude = c(153.551875, 149.5559),
                               Latitude =  c(-28.517974,-33.4289))

sp::coordinates(coordinates.data) <- ~Longitude + Latitude

sp::proj4string(coordinates.data) = '+proj=longlat +ellps=GRS80 +no_defs'
```

## Extract daily precipitation data

Now let’s extract the daily precipitation at the two locations. The data
is extracted from the netCDF file *ncdfFilename* and between the dates
*extractFrom* and *extractTo*.

Note the netCDF file *ncdfFilename* must be in the working directory or
the full file path must be given.

``` r
extracted.data = extract.data(ncdfFilename = ncdfFilename,
                                        extractFrom = dataFrom,
                                        extractTo = dataTo,
                                        locations = coordinates.data,
                                        vars = c('precip'),
                                        ET.function='')
#> Extraction data summary:
#>     NetCDF climate data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#> Starting data extraction:
#> ... Building catchment weights for each grid.
#> ... Starting to extract data across all variable and locations:
#> Loading required namespace: ncdf4
#> ... Linearly interpolating gaps
#> ... Backfilling dates prior to the start of observations
#> ... Calculating area weighted results at required time-step.
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:00:25
```

## Plot the daily precipitation at each site

Now let’s plot time series of the extracted daily precipitation.

``` r
par(mfrow = c(2, 1))
site.names = c('Bore-10084446','Rain-63005')
for (i in 1:nrow(coordinates.data)){
  filt = extracted.data$Location.ID == coordinates.data$ID[i]

  data2plot = extracted.data$precip[filt]
  dates = ISOdate(extracted.data$year[filt],
                  extracted.data$month[filt],
                  extracted.data$day[filt])

    plot(dates, data2plot,
         main = paste('Extracted precip. at',site.names[i]),
         ylab = 'Precip [mm/d]',
         xlab = 'Date',
         type = 'l')
}
```

![](B_Point_rainfall_files/figure-html/unnamed-chunk-7-1.png)

## Check extracted data against daily precipitation gauge data

To assess the reliability of the extracted rain gauge data, let’s use
the following observed precipitation for 2010 (from gauge_63005),
sourced from the Australian Bureau of Meteorology.

``` r
data('raingauge')
```

Now let’s compare the extracted rainfall against the gauge rainfall
data.

The left plot shows that the gauge and extracted data plot along the
1:1. This shows that the extracted data is unbiased. The scatter does,
however, show that there is some difference in the estimates. This
arises because the source grids are at a 0.05 degree resolution, or
about 5 km x 5 km, and this introduces some error with the gauge is not
at the centre of the grid cell and the location to be extracted is not
also at the centre.

``` r
par(mfrow = c(1, 2))

filt = extracted.data$Location.ID=='Rain-63005'

xdata = raingauge$Rainfall.amount..millimetres.
ydata = extracted.data$precip[filt]
plot(xdata,
     ydata,
     xlim = c(0,35),
     ylim = c(0,35),
     main ='Obs. vs. Extracted precip.',
     xlab ='Obs. [mm/d]',
     ylab ='Extracted precip. [mm/d]')
abline(0,1, col='grey', lty=2)
rmse = round(sqrt(mean((xdata - ydata)^2)), 2)
text(x=0, y=30, adj = c(0,0),
     labels = paste('RMSE = ', rmse, 'mm/day'))

plot(cumsum(xdata),
     cumsum(ydata),
     xlim = c(0,175),
     ylim = c(0,175),
     main = 'Cum. obs. vs. Extracted precip.',
     xlab = 'Obs. [mm]',
     ylab = 'Extracted precip. [mm]',
     type = 'l')
abline(0,1, col='grey', lty=2)
```

![](B_Point_rainfall_files/figure-html/unnamed-chunk-9-1.png)

## Check extracted data against monthly precipitation gauge data

Now let’s compare the gauge data at a monthly time step with the
extracted monthly data and aggregated daily data to a monthly time
scale.

First, let’s extract the daily data at a monthly scale

``` r
extracted.daily2monthly = extract.data(ncdfFilename = ncdfFilename,
                                        extractFrom = dataFrom,
                                        extractTo = dataTo,
                                        locations = coordinates.data,
                                        vars = c('precip'),
                                        temporal.timestep = 'monthly',
                                        temporal.function.name = 'sum',
                                        ET.function='')
#> Extraction data summary:
#>     NetCDF climate data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#> Starting data extraction:
#> ... Building catchment weights for each grid.
#> ... Starting to extract data across all variable and locations:
#> ... Linearly interpolating gaps
#> ... Backfilling dates prior to the start of observations
#> ... Calculating area weighted results at required time-step.
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:00:22
```

Next let’s extract the monthly data product.

``` r
extracted.monthly = extract.data(ncdfFilename = ncdfFilename,
                                        extractFrom = dataFrom,
                                        extractTo = dataTo,
                                        locations = coordinates.data,
                                        vars = c('precip.monthly'),
                                        temporal.timestep = 'monthly',
                                        ET.function='')
#> Extraction data summary:
#>     NetCDF climate data exists from 2010-01-01 to 2010-12-31
#>     Data will be extracted from  2010-01-01  to  2010-12-31  at  2  locations
#> Starting data extraction:
#> ... Building catchment weights for each grid.
#> ... Starting to extract data across all variable and locations:
#> ... Linearly interpolating gaps
#> ... Backfilling dates prior to the start of observations
#> ... Calculating area weighted results at required time-step.
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:00:00
```

Now let’s aggregate the daily observed data to a monthly sum.

``` r

dates = as.Date(ISOdate( raingauge$Year,
                         raingauge$Month,
                         raingauge$Day))

raingauge.xts = xts::as.xts(raingauge$Rainfall.amount..millimetres. ,
                            order.by=dates)

raingauge.monthly = xts::apply.monthly(raingauge.xts, apply, 2, 'sum')
```

Now we can compare the extracted estimates of monthly precipitation with
the gauge monthly precipitation.

The top plot below shows the monthly gauged precipitation vs the
extracted daily precipitation aggregated to monthly. It shows that the
daily gridded data has a modest bias when aggregated to monthly (at
least for 2010).

The centre plot shows the monthly gauged precipitation vs the extracted
monthly precipitation. It shows a slighter greater bias at and above 100
mm/month.

The bottom plot show the extracted monthly precipitation vs the
extracted daily precipitation aggregated to monthly. It shows that the
daily aggregated precipitation estimates a slightly higher monthly
precipitation.

``` r
par(mfrow = c(3, 1))

filt1 = extracted.daily2monthly$Location.ID=='Rain-63005'
filt2 = extracted.monthly$Location.ID=='Rain-63005'

xdata = as.numeric(raingauge.monthly)
ydata = extracted.daily2monthly$precip[filt1]
plot(xdata,
     ydata,
     xlim = c(0,220),
     ylim = c(0,220),
     main='Obs. monthly vs extracted aggregated to monthly',
     xlab='Obs. [mm/month]',
     ylab='Aggregated precip. [mm/month]')
abline(0,1, col='grey', lty=2)
rmse = round(sqrt(mean((xdata - ydata)^2)), 2)
text(x=0, y=200, adj = c(0,0),
     labels = paste('RMSE = ', rmse, 'mm/month'))

xdata = as.numeric(raingauge.monthly)
ydata = extracted.monthly$precip[filt2]
plot(xdata,
     ydata,
     xlim = c(0,220),
     ylim = c(0,220),
     main='Obs. monthly vs extracted monthly',
     xlab='Obs. [mm/month]',
     ylab='Extracted precip. [mm/month]')
abline(0,1, col='grey', lty=2)
rmse = round(sqrt(mean((xdata - ydata)^2)), 2)
text(x=0, y=200, adj = c(0,0),
     labels = paste('RMSE = ', rmse, 'mm/month'))

xdata = extracted.monthly$precip[filt2]
ydata = extracted.daily2monthly$precip[filt1]
plot(xdata,
     ydata,
     xlim = c(0,220),
     ylim = c(0,220),
     main='Extracted monthly vs extracted aggregated to monthly',
     xlab='Extracted precip. [mm/month]',
     ylab='Aggregated precip. [mm/month]')
abline(0,1, col='grey', lty=2)
```

![](B_Point_rainfall_files/figure-html/unnamed-chunk-13-1.png)
