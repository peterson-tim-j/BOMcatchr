# Build a netCDF file of climate data.

`build.grids` builds one netCDF file containing Australian climate data.

## Usage

``` r
build.grids(
  ncdfFilename = file.path(getwd(), "AWAP.nc"),
  updateFrom = as.Date("1900-01-01", "%Y-%m-%d"),
  updateTo = as.Date(Sys.Date() - 2, "%Y-%m-%d"),
  vars = "",
  keepFiles = FALSE,
  compressionLevel = 5,
  vars.sourceData = grid.sources()
)
```

## Arguments

- ncdfFilename:

  is a file path (as string) and name to the netCDF file. If only a file
  name is given, then the file is assumed to exist / be created in
  [`getwd()`](https://rdrr.io/r/base/getwd.html). The default file name
  and path is `file.path(getwd(),'AWAP.nc')`.

- updateFrom:

  is a date string specifying the start date for the AWAP data. If
  `ncdfFilename` is specified and exist, then the netCDF grids will be
  updated with new data from `updateFrom`. To update the file from the
  end of the last day in the file set `updateFrom=NA`. The default is
  `"1900-1-1"`.

- updateTo:

  is a date string specifying the end date for the AWAP data. If
  `ncdfFilename` is specified and exist, then the netCDF grids will be
  updated with new data to `updateFrom`. The default is two days ago as
  YYYY-MM-DD.

- vars:

  is a vector of variables names to build or update. The available
  variables are: daily precipitation, monthly precipitation, daily
  minimum temperature, daily maximum temperature, daily 3pm vapour
  pressure grids and daily solar radiation. Any or all of the defaults
  are available. If `vars=''` and the netCDF does not exist, then the
  default is
  `c('precip', 'precip.monthly','tmin', 'tmax', 'vprp', 'solarrad')` and
  provided by `rownames(grid.sources())`. However, if `vars=''` and the
  netCDF file does exist, then default is to use the variable names in
  the file.

- keepFiles:

  is a logical scalar to keep the downloaded AWAP grid files. The
  default is `FALSE`.

- compressionLevel:

  is the netCDF compression level between 1 (low) and 9 (high), and `NA`
  for no compression. Note, data extraction runtime may slightly
  increase with the level of compression. The default is `5`.

- vars.sourceData:

  is a data.frame of variable unit, time step and source URLs. This
  input is provided in-case the default URLs need to be changed. The
  default is `grid.sources())`

## Value

A string containing the full file name to the netCDF file.

## Details

build.grids creates one netCDF file of daily climate data.

One netCDF file is created than contains precipitation, minimum daily
temperature, maximum daily temperature and vapour pressure and the solar
radiation data. It should span from 1/1/1900 to yesterday and requires
~20GB of hard-drive space (using default compression). For the solar
radiation, spatial gaps are infilled using a 3x3 moving average repeated
3 times. To minimise the runtime in extracting data, the netCDF file
should be stored locally and not on a network drive. Also, building the
file requires installation of 7zip.

The climate data is sourced from the Bureau of Meteorology Australian
Water Availability Project (<http://www.bom.gov.au/jsp/awap/>. For
details see Jones et al. (2009).

The output from this function is required for all data extraction
functions within this package and must be ran prior.

The function can be used to a build netCDF file from scratch or to
update an existing netCDF file previously derived from this function. To
not build or update a variable, set its respective URL to `NA`.

## References

David A. Jones, William Wang and Robert Fawcett, (2009), High-quality
spatial climate data-sets for Australia, Australian Meteorological and
Oceanographic Journal, 58 , p233-248.

## See also

[`extract.data`](https://peterson-tim-j.github.io/BOMcatchr/reference/extract.data.md)
for extracting catchment daily average and variance data.

## Examples

``` r
# The example shows how to build the netCDF data cubes.
# For extra example see \url{https://github.com/peterson-tim-j/BOMcatchr/blob/master/README.md}
#---------------------------------------

# Set dates for building netCDFs and extracting data from 15 to 5 days ago.
startDate = as.Date(Sys.Date()-15,"%Y-%m-%d")
endDate = as.Date(Sys.Date()-5,"%Y-%m-%d")

# Set names for the netCDF file (in the system temp. directory).
ncdfFilename = tempfile(fileext='.nc')

# \donttest{
# Build netCDF grids for daily precipitation and only over the defined time period.
file.names = build.grids(ncdfFilename=ncdfFilename,
             updateFrom=startDate,
             updateTo=endDate,
             vars = c('precip'))
#> ... Testing downloading of each variable.
#>     Testing precip grid data.
#> ... NetCDF file will be updated as follows:
#>        - New variables to add: precip  
#>        - Existing variables to modify: (none)
#>        - Data will be updated from  2026-04-10  to  2026-04-20
#> ... Downloading data for each variable and importing to netcdf file:
#> Data construction FINISHED.
#> Summary of time points successfully imported (and errors).
#>        Imported Errors
#> precip       11      0
#> Total run time (DD:HH:MM:SS): 00:00:00:09

# Now, to demonstrate updating the netCDF grids to one day ago, rerun with
# the same file names but \code{updateFrom=NA}.
file.names = build.grids(ncdfFilename=ncdfFilename,
             updateFrom=NA)
#> ... Testing downloading of each variable.
#>     Testing precip grid data.
#> ... updateFrom reduced to ensure all variables have the same start date.
#> ... updateTo increased to ensure all variables have the same end date.
#> ... NetCDF file will be updated as follows:
#>        - New variables to add: (none)
#>        - Existing variables to modify: precip  
#>        - Data will be updated from  2026-04-10  to  2026-04-20
#> ... Downloading data for each variable and importing to netcdf file:
#> Data construction FINISHED.
#> Summary of time points successfully imported (and errors).
#>        Imported Errors
#> precip       11      0
#> Total run time (DD:HH:MM:SS): 00:00:00:03

 # Remove temp. file
 unlink(ncdfFilename)
# }
```
