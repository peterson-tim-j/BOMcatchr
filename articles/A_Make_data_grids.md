# Make source data grids

``` r
library(AWAPer, warn.conflicts = FALSE)
```

This example shows how to build the required data files and then update
them.

## Make netCDF file

This example shows the steps required to build the netCDF data grid
files.

First, let’s define the dates for the data grids. Here the data grids
are created for data between the dates *updateTo* and *updateTo*. If the
latter two dates were not input then data would be downloaded from
1/1/1900 to yesterday.

``` r
startDate = as.Date(Sys.Date()-15,"%Y-%m-%d")
endDate = as.Date(Sys.Date()-5,"%Y-%m-%d")
```

Next the file name for the netCDF grids need to be defined. Here we’ll
just use temporary files. You should change this to a non-temporary file
name and folder.

``` r
# Set names for netCDF files (in the system temp. directory).
ncdfFilename = tempfile(fileext='.nc')
ncdfSolarFilename = tempfile(fileext='.nc')
```

Now we’re ready to download and build the netCDF grids.

The netCDF data files contains grids of daily rainfall, temperature,
vapour pressure deficit and solar radiation for all of Australia.

``` r
file.names = makeNetCDF_file(ncdfFilename=ncdfFilename,
             ncdfSolarFilename=ncdfSolarFilename,
             updateFrom=startDate, updateTo=endDate)
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
#>     NetCDF data will be updated from  2025-11-24  to  2025-12-04
#> ... Downloading non-solar data and importing to netcdf file:
#>     NetCDF Solar data will be updated from  2025-11-24  to  2025-12-04
#> ... Downloading solar data and importing to netcdf file:
#> Data construction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:01:13
```

## Update existing data grids

Now that we’ve built the above files, the updating can be demonstrated.
Here we’ll updating the data grids to one day ago.

Often users run *makeNetCDF_file* once to build netCDF data files that
contain all variables over the entire record length (which requires ~5GB
disk storage) and then use the netCDFs grids for multiple projects,
rather than re-building the netCDF for each project.

Also, if *makeNetCDF_file* is run with the the file names pointing to
existing files and *updateFrom=NA* then the netCDF files will be updated
to yesterday.

``` r
file.names = makeNetCDF_file(ncdfFilename=ncdfFilename,
             ncdfSolarFilename=ncdfSolarFilename,
             updateFrom=NA)
#> Starting to update both netCDF files.
#> ... Testing downloading of AWAP precip. grid
#> ... Getting grid gemoetry from file.
#> ... Deleting /home/runner/work/AWAPer/AWAPer/vignettes/precip.20000101.grid.gz
#> ... Testing downloading of AWAP tmin grid
#> ... Testing downloading of AWAP tmax grid
#> ... Testing downloading of AWAP vapour pressure grid
#> ... Testing downloading of AWAP solar grid
#> ... Getting grid gemoetry from file.
#> ... Deleting /home/runner/work/AWAPer/AWAPer/vignettes/solarrad.20000101.grid.gz
#>     NetCDF data will be updated from  2025-12-04  to  2025-12-07
#> ... Downloading non-solar data and importing to netcdf file:
#>     NetCDF Solar data will be updated from  2025-12-04  to  2025-12-07
#> ... Downloading solar data and importing to netcdf file:
#> Data construction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:00:26
```
