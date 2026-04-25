# Make source data grids

``` r
library(BOMcatchr)
```

This example shows how to build the required data file and then update
it.

## Make netCDF file

This example shows the steps required to build the netCDF data grid
file.

First, let’s define the dates for the data grids. Here the data grids
are created for data between the dates *updateTo* and *updateTo*. If the
latter two dates were not input then data would be downloaded from
1/1/1900 to yesterday.

Note, in practice users often run *build.grids* once to build the netCDF
data file that contain all variables over the entire record length
(which requires ~5GB disk storage) and then use the netCDF grids for
multiple projects, rather than re-building the netCDF grids for each
project.

``` r
startDate <- as.Date(Sys.Date()-15,"%Y-%m-%d")
endDate <- as.Date(Sys.Date()-5,"%Y-%m-%d")
```

Next the file name for the netCDF grids need to be defined. Here we’ll
just use a temporary file. You should change this to a non-temporary
file name and folder.

``` r
ncdfFilename <- tempfile(fileext='.nc')
```

Now we’re ready to download and build the netCDF grids.

The netCDF data file contains grids of daily rainfall, minimum and
maximum temperature. Below, this will be updated to include vapour
pressure and solar radiation for all of Australia.

``` r
ncdffile.name <- build.grids(ncdfFilename=ncdfFilename,
                updateFrom=startDate, updateTo=endDate,
                vars = c('precip','tmin','tmax'))
#> ... Testing downloading of each variable.
#>     Testing precip grid data.
#>     Testing tmin grid data.
#>     Testing tmax grid data.
#> ... NetCDF file will be updated as follows:
#>        - New variables to add: precip  tmin  tmax
#>        - Existing variables to modify: (none)
#>        - Data will be updated from  2026-04-10  to  2026-04-20
#> ... Downloading data for each variable and importing to netcdf file:
#> Data construction FINISHED.
#> Summary of time points successfully imported (and errors).
#>        Imported Errors
#> precip       11      0
#> tmin         11      0
#> tmax         11      0
#> Total run time (DD:HH:MM:SS): 00:00:00:21
```

Now let’s get a summary of the netCDF file that we’ve created. Note that
the output data.frame shows the expected variables over the expected
date range.

``` r
summary.df <- BOMcatchr::grid.summary(ncdffile.name)
summary.df
#>        group   var.string       from         to time.step
#> precip grid1 grid1/precip 2026-04-10 2026-04-20      days
#> tmin   grid1   grid1/tmin 2026-04-10 2026-04-20      days
#> tmax   grid1   grid1/tmax 2026-04-10 2026-04-20      days
#>                                    time.datum  units              ellipsoid.crs
#> precip days since 1900-01-01 00:00:00.0 -0:00 mm/day +proj=longlat +ellps=GRS80
#> tmin   days since 1900-01-01 00:00:00.0 -0:00  deg_C +proj=longlat +ellps=GRS80
#> tmax   days since 1900-01-01 00:00:00.0 -0:00  deg_C +proj=longlat +ellps=GRS80
```

## Update an existing variable

Now that we’ve built the above file, the updating can be demonstrated.
The BoM gridded data undergoes a detailed review and update process (see
[https://www.bom.gov.au/climate/austmaps/about-rain-maps.shtml](https://peterson-tim-j.github.io/BOMcatchr/articles/here)
and
[https://www.bom.gov.au/climate/austmaps/update-schedule.shtml](https://peterson-tim-j.github.io/BOMcatchr/articles/here)
). Hence, the netCDF grids that one may have built some time ago may be
require updating with revised BoM data.

The package allows for such updating between user defined dates. Here
we’ll update the data between the dates defined prior. Also, note that
is not defined. This will cause all variables in the original file to be
updated. That said, individual variables can be updated.

``` r
ncdffile.name <- build.grids(ncdfFilename=ncdffile.name,
                updateFrom=startDate, updateTo=endDate)
#> ... Testing downloading of each variable.
#>     Testing precip grid data.
#>     Testing tmin grid data.
#>     Testing tmax grid data.
#> ... NetCDF file will be updated as follows:
#>        - New variables to add: (none)
#>        - Existing variables to modify: precip  tmin  tmax
#>        - Data will be updated from  2026-04-10  to  2026-04-20
#> ... Downloading data for each variable and importing to netcdf file:
#> Data construction FINISHED.
#> Summary of time points successfully imported (and errors).
#>        Imported Errors
#> precip       11      0
#> tmin         11      0
#> tmax         11      0
#> Total run time (DD:HH:MM:SS): 00:00:00:12
```

## Add a variable to existing data grids

Here we’ll update the data grids to also include vapour pressure. Here
the same date range will be used. Because the date range of the new
variable equals the date range of the existing variables, only vapour
pressure data is updated. This feature can be used to update the data of
a single variable as required.

``` r
ncdffile.name <- build.grids(ncdfFilename=ncdffile.name,
                updateFrom=startDate, updateTo=endDate,
                vars = c('vprp'))
#> ... Testing downloading of each variable.
#>     Testing vprp grid data.
#>     Testing precip grid data.
#>     Testing tmin grid data.
#>     Testing tmax grid data.
#> ... NetCDF file will be updated as follows:
#>        - New variables to add: vprp
#>        - Existing variables to modify: (none)
#>        - Data will be updated from  2026-04-10  to  2026-04-20
#> ... Downloading data for each variable and importing to netcdf file:
#> Data construction FINISHED.
#> Summary of time points successfully imported (and errors).
#>      Imported Errors
#> vprp       11      0
#> Total run time (DD:HH:MM:SS): 00:00:00:09
```

Now let’s check that the file includes both the original three variable
plus vapour pressure.

``` r
summary.df <- BOMcatchr::grid.summary(ncdffile.name)
summary.df
#>        group   var.string       from         to time.step
#> precip grid1 grid1/precip 2026-04-10 2026-04-20      days
#> tmin   grid1   grid1/tmin 2026-04-10 2026-04-20      days
#> tmax   grid1   grid1/tmax 2026-04-10 2026-04-20      days
#> vprp   grid1   grid1/vprp 2026-04-10 2026-04-20      days
#>                                    time.datum  units              ellipsoid.crs
#> precip days since 1900-01-01 00:00:00.0 -0:00 mm/day +proj=longlat +ellps=GRS80
#> tmin   days since 1900-01-01 00:00:00.0 -0:00  deg_C +proj=longlat +ellps=GRS80
#> tmax   days since 1900-01-01 00:00:00.0 -0:00  deg_C +proj=longlat +ellps=GRS80
#> vprp   days since 1900-01-01 00:00:00.0 -0:00    hpa +proj=longlat +ellps=GRS80
```

## Add a variable to existing data grids and update other variables

Now lets add solar radiation. Here an earlier start date will be used.
However, because the date range of all variables within the netCDF file
must be equal, the date range of the other variables must also be
updated.

``` r
startDate <- startDate - 5
```

``` r
ncdffile.name <- build.grids(ncdfFilename=ncdffile.name,
                updateFrom=startDate, updateTo=endDate,
                vars = c('solarrad'))
#> ... Testing downloading of each variable.
#>     Testing solarrad grid data.
#>     Testing precip grid data.
#>     Testing tmin grid data.
#>     Testing tmax grid data.
#>     Testing vprp grid data.
#> ... NetCDF file will be updated as follows:
#>        - New variables to add: solarrad
#>        - Existing variables to modify: precip  tmin  tmax  vprp
#>        - Data will be updated from  2026-04-05  to  2026-04-20
#> ... Downloading data for each variable and importing to netcdf file:
#> Data construction FINISHED.
#> Summary of time points successfully imported (and errors).
#>          Imported Errors
#> solarrad       16      0
#> precip         16      0
#> tmin           16      0
#> tmax           16      0
#> vprp           16      0
#> Total run time (DD:HH:MM:SS): 00:00:00:47
```

Now let’s check that the file includes both the prior four variables
plus solar radiation.

``` r
summary.df <- BOMcatchr::grid.summary(ncdffile.name)
summary.df
#>          group     var.string       from         to time.step
#> precip   grid1   grid1/precip 2026-04-05 2026-04-20      days
#> tmin     grid1     grid1/tmin 2026-04-05 2026-04-20      days
#> tmax     grid1     grid1/tmax 2026-04-05 2026-04-20      days
#> vprp     grid1     grid1/vprp 2026-04-05 2026-04-20      days
#> solarrad grid2 grid2/solarrad 2026-04-05 2026-04-20      days
#>                                      time.datum  units
#> precip   days since 1900-01-01 00:00:00.0 -0:00 mm/day
#> tmin     days since 1900-01-01 00:00:00.0 -0:00  deg_C
#> tmax     days since 1900-01-01 00:00:00.0 -0:00  deg_C
#> vprp     days since 1900-01-01 00:00:00.0 -0:00    hpa
#> solarrad days since 1900-01-01 00:00:00.0 -0:00 MJ/m^2
#>                       ellipsoid.crs
#> precip   +proj=longlat +ellps=GRS80
#> tmin     +proj=longlat +ellps=GRS80
#> tmax     +proj=longlat +ellps=GRS80
#> vprp     +proj=longlat +ellps=GRS80
#> solarrad +proj=longlat +ellps=GRS80
```
