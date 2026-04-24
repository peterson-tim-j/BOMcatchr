# Extract data over catchment area and duration .

extractCatchmentData extracts the AWAP climate data for each point or
polygon. For the latter, either the daily spatial mean and variance (or
user defined function) of each climate metric is calculated or the
spatial data is returned.

## Usage

``` r
extractCatchmentData(
  ncdfFilename = file.path(getwd(), "AWAP.nc"),
  extractFrom = as.Date("1900-01-01", "%Y-%m-%d"),
  extractTo = as.Date(Sys.Date(), "%Y-%m-%d"),
  vars = "",
  locations = NULL,
  temporal.timestep = "daily",
  temporal.function.name = "mean",
  spatial.function.name = "var",
  interp.method = "",
  missing.method = c("5", "linear", "mean"),
  ET.function = "ET.MortonCRAE",
  ET.DEM.res = 10,
  ET.Mortons.est = "wet areal ET",
  ET.Turc.humid = F,
  ET.timestep = "monthly",
  ET.missing_method = "DoY average",
  ET.abnormal_method = "DoY average",
  ET.constants = list()
)
```

## Arguments

- ncdfFilename:

  is a full file name (as string) to the netCDF file.

- extractFrom:

  is a date string specifying the start date for data extraction. The
  default is `"1900-1-1"`.

- extractTo:

  is a date string specifying the end date for the data extraction. The
  default is today's date as YYYY-MM-DD.

- vars:

  is a vector of variables names to extract. The available variables
  are: daily precipitation, daily minimum temperature, daily maximum
  temperature, daily 3pm vapour pressure grids and daily solar radiation
  and evapotranspiration. The input vector for these options are
  `c('tmax', 'tmin', 'precip', 'precip.monthly', 'vprp', 'solarrad', 'et')`.
  Importantly, the input `et` is calculated from the available gridded
  data (see `ET.` inputs below). To calculate the ET, all of the
  required inputs for the calculation ET must also be extracted (i.e.
  the input for such would generally be
  `c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et')`. Any or all of
  the defaults are available. The default `''` and this will result in
  all of the variables in the netCDF file and provided by
  `rownames(AWAPer::file.summary(ncdfFilename))`.

- locations:

  is either the full file name to an ESRI shape file of points or
  polygons (latter assumed to be catchment boundaries) or a shape file
  already imported using readShapeSpatial(). Either way the shape file
  must be in long/lat (i.e. not projected), use the ellipsoid GRS 80,
  and the first column must be a unique ID.

- temporal.timestep:

  character string for the time step of the output data. The options are
  `daily`, `weekly`, `monthly`, `quarterly`, `annual` or a user-defined
  index for, say, water-years (see
  [`xts::period.apply`](https://rdrr.io/pkg/xts/man/period.apply.html)).
  The default is `daily`.

- temporal.function.name:

  character string for the function name applied to aggregate the daily
  data to `temporal.timestep`. Note, NA values are not removed from the
  aggregation calculation. If this is required then consider writing
  your own function. The default is `mean`.

- spatial.function.name:

  character string for the function name applied to estimate the daily
  spatial spread in each variable. If `NA` or `""` and `locations` is a
  polygon, then the spatial data is returned. The default is `var`.

- interp.method:

  character string defining the method for interpolating the gridded
  data (see
  [`raster::extract`](https://rspatial.github.io/terra/reference/extract.html)).
  The options are: `'simple'`, `'bilinear'` and `''`. The default is
  `''`. This will set the interpolation to `'simple'` when `locations`
  is a polygon(s) and to `'bilinear'` when `locations` are points.

- missing.method:

  three character vector for the settings to fill gaps in the source
  data. The three inputs control the following. \# 1) the infilling of
  small holes in the source grids using focal(), which takes the mean of
  the non-NA surrounding grid cells. The user input controls the maximum
  hole size filled, in units of number of grid cells. Where the hole is
  greater than the input, a gap within the hole will remain. The default
  maximum hole size infilled is 5x5 grid cells. 2) Gaps that remain
  after the hole infilling, or time steps with no observations, are
  interpolated over time. Only gaps with observations prior to the gap
  are interpolated. The interpolation method is user-defined and
  includes `'constant'`, `'linear'`, `'fmm'`, `'periodic'`, `'natural'`,
  `'monoH.FC'` and `'hyman'`. The default is `'linear'`. See
  [`approx`](https://rdrr.io/r/stats/approxfun.html) and
  [`splinefun`](https://rdrr.io/r/stats/splinefun.html) for details. 3)
  Gaps that remain (often due to the extraction date being prior to the
  start of the observation record of a variable) are estimated from,
  say, the mean for each day of the year. Specifically, the extracted
  observed data is allocated to each calender day. If, say, there are
  ten years of daily data then each day of the year will have ten
  observations. All gaps of the same corresponding calender day will
  then be assigned a value from a user-defined function from these
  observations (NB: when only one observed value exists for the day,
  then the observed value is returned). The default function is `mean`.
  Other standard functions (e.g. `median`) or user defined functions can
  be used. The default for this input is `c('5', 'linear', 'mean')`. All
  gap filling method can be turned off with the input `c('0', '', '')`,
  which is useful to identify the interpolated data points.

- ET.function:

  character string for the evapotranspiration function to be used. The
  methods that can be derived from the AWAP data are are
  [`ET.Abtew`](https://rdrr.io/pkg/Evapotranspiration/man/ET.Abtew.html),
  [`ET.HargreavesSamani`](https://rdrr.io/pkg/Evapotranspiration/man/ET.HargreavesSamani.html),
  [`ET.JensenHaise`](https://rdrr.io/pkg/Evapotranspiration/man/ET.JensenHaise.html),
  [`ET.Makkink`](https://rdrr.io/pkg/Evapotranspiration/man/ET.Makkink.html),
  [`ET.McGuinnessBordne`](https://rdrr.io/pkg/Evapotranspiration/man/ET.McGuinnessBordne.html),
  [`ET.MortonCRAE`](https://rdrr.io/pkg/Evapotranspiration/man/ET.MortonCRAE.html)
  ,
  [`ET.MortonCRWE`](https://rdrr.io/pkg/Evapotranspiration/man/ET.MortonCRWE.html),
  [`ET.Turc`](https://rdrr.io/pkg/Evapotranspiration/man/ET.Turc.html).
  Default is
  [`ET.MortonCRAE`](https://rdrr.io/pkg/Evapotranspiration/man/ET.MortonCRAE.html)
  i.e. the complementary relationship for areal evapotranspiration .

- ET.DEM.res:

  is the zoom resolution for the land surface elevation and is required
  to calculate the ET. `elevatr` package is used to extract elevation
  (metres) from AWS Open Data Terrain Tiles. This input controls the
  zoom resolution. Higher values increase accuracy, but are
  significantly slower. See details. Default is 10.

- ET.Mortons.est:

  character string for the type of Morton's ET estimate. For
  `ET.MortonCRAE`, the options are `potential ET`,`wet areal ET` or
  `actual areal ET`. For `ET.MortonCRWE`, the options are `potential ET`
  or `shallow lake ET`. The default is `wet areal ET`, which when
  `ET.function = 'ET.MortonCRAE'` it provides an estimate of the wet
  areal potential evapotranspiration.

- ET.Turc.humid:

  logical variable for the Turc function using the humid adjustment.See
  [`ET.Turc`](https://rdrr.io/pkg/Evapotranspiration/man/ET.Turc.html).
  For now this is fixed at `F`.

- ET.timestep:

  character string for the evapotranpiration time step. Options are
  `daily`, `monthly`, `annual` but the options are dependent upon the
  chosen `ET.function`. The default is `'monthly'`.

- ET.missing_method:

  character string for interpolation method for missing variables
  required for ET calculation. The options are `'monthly average'`,
  `'seasonal average'`, `'DoY average'` and `'neighbouring average'`.
  Default is `'DoY average'` but when the extraction duration is less
  than two years, the default is `'neighbouring average'`. See
  [`ReadInputs`](https://rdrr.io/pkg/Evapotranspiration/man/ReadInputs.html)

- ET.abnormal_method:

  character string for interpolation method for abnormal variables
  required for ET calculation (e.g. Tmin \> Tmax). Options and defaults
  are as for `ET.missing_method`. See
  [`ReadInputs`](https://rdrr.io/pkg/Evapotranspiration/man/ReadInputs.html)

- ET.constants:

  list of constants from Evapotranspiration package required for ET
  calculations. To get the data use the command `data(constants)`.
  Default is [`list()`](https://rdrr.io/r/base/list.html).

## Value

When `locations` are polygons and `spatial.function.name` is not `NA` or
`""`, then the returned variable is a list variable containing two
data.frames. The first is the areal aggregated climate metrics named
`catchmentTemporal.` with a suffix as defined by
`temporal.function.name`). The second is the measure of spatial
variability named `catchmentSpatial.` with a suffix as defined by
`spatial.function.name`).

When `locations` are polygons and `spatial.function.name` does equal
`NA` or `""`, then the returned variable is a
[`sp::SpatialPixelsDataFrame`](https://edzer.github.io/sp/reference/SpatialGridDataFrame.html)
where the first column is the location/catchment IDs and the latter
columns are the results for each variable at each time point as defined
by `temporal.timestep`.

When `locations` are points, the returned variable is a data.frame
containing daily climate data at each point.

## Details

Daily data is extracted and can be aggregated to a weekly, monthly,
quarterly, annual or a user-defined timestep using a user-defined
function (e.g. sum, mean, min, max as defined by
`temporal.function.name`). The temporally aggregated data at each grid
cell is then used to derive the spatial mean or the spatial variance (or
any other function as defined by `spatial.function.name`).

The calculation of the spatial mean uses the fraction of each AWAP grid
cell within the catchment polygon. The variance calculation (or user
defined function) does not use the fraction of the grid cell and returns
NA if there are \<2 grid cells in the catchment boundary. Prior to the
spatial aggregation, evapotranspiration (ET) can also calculated; after
which, say, the mean and variance PET can be calculated.

The data extraction will by default be undertaken from 1/1/1900 to
yesterday, even if the netCDF grids were only built for a subset of this
time period. If the latter situation applies, it is recommended that the
extraction start and end dates are input by the user.

The ET can be calculated using one of eight methods at a user defined
calculation time-step; that is the `ET.timestep` defines the time step
at which the estimates are derived and differs from the output timestep
as defined by `temporal.function.name`). When `ET.timestep` is monthly
or annual then the ET estimate is linearly interpolated to a daily time
step (using zoo:na.spline()) and then constrained to \>=0. In
calculating ET, the input data is pre-processed using
Evapotranspiration::ReadInputs() such that missing days, missing entries
and abnormal values are interpolated (by default) with the former two
interpolated using the "DoY average", i.e. replacement with same
day-of-the-year average. Additionally, when AWAP solar radiation is
required for the ET function, data is only available from 1/1/1990. To
derive ET values \<1990, the average solar radiation for each day of the
year from 1/1/990 to "extractTo" is derived (i.e. 365 values) and then
applied to each day prior to 1990. Importantly, in this situation the
estimates of ET \<1990 are dependent upon the end date extracted.
Re-running the estimation of ET with a later extractTo data will change
the estimates of ET prior to 1990.

Some measures of ET require land surface elevation. Here, elevation at
the centre of each 0.05 degree grid cell is obtained using the `elevatr`
package, which here uses data from the Amazon Web Service AWS Open Data
Terrain Tiles. The data sources change with the user set `ET.DEM.res`
zoom. The options are 1 to 15. The default of 10 is reasonably
computationally efficient and has a resolution of about 108 m, with is
acceptable given the 0.05 degree resolution of the BOM source data grids
equates to about 5 km x 5 km. For details see
<https://github.com/tilezen/joerd/blob/master/docs/data-sources.md>

Also, when "locations" is points (not polygons), then the netCDF grids
are interpolate using bilinear interpolation of the closest 4 grid
cells.

Lastly, data is extracted for all time points and no temporal infilling
is undertaken if the grid cells are blank.

## See also

[`makeNetCDF_file`](https://peterson-tim-j.github.io/AWAPer/reference/makeNetCDF_file.md)
for building the NetCDF files of daily climate data.

## Examples

``` r
# The example shows how to extract and save data.
#---------------------------------------
library(sp)

# Set dates for building netCDFs and extracting data.
# Note, to reduce runtime this is done only a fortnight (14 days).
startDate = as.Date("2000-01-01","%Y-%m-%d")
endDate = as.Date("2000-01-14","%Y-%m-%d")

# Set names for netCDF file.
ncdfFilename = tempfile(fileext='.nc')

# Build netCDF grids and over a defined time period.
# Only precip data is to be added to the netCDF files.
# This is because the URLs for the other variables are set to zero.
# \donttest{
file.name = makeNetCDF_file(ncdfFilename=ncdfFilename,
             updateFrom=startDate,
             updateTo=endDate,
             vars = c('precip'))
#> ... Testing downloading of each variable.
#>     Testing precip grid data.
#> ... NetCDF file will be updated as follows:
#>        - New variables to add: precip  
#>        - Existing variables to modify: (none)
#>        - Data will be updated from  2000-01-01  to  2000-01-14
#> ... Downloading data for each variable and importing to netcdf file:
#> Data construction FINISHED.
#> Summary of time points successfully imported (and errors).
#>        Imported Errors
#> precip       14      0
#> Total run time (DD:HH:MM:SS): 00:00:00:11

# Load example catchment boundaries and remove all but the first.
# Note, this is done only to speed up the example runtime.
data("catchments")
catchments = catchments[1,]

# Extract daily precip. data (not Tmin, Tmax, VPD, ET).
# Note, the input "locations" can also be a file to a ESRI shape file.
climateData = extractCatchmentData(ncdfFilename=file.name,
              extractFrom=startDate,
              extractTo=endDate,
              vars = c('precip'),
              locations=catchments,
              temporal.timestep = 'daily')
#> Extraction data summary:
#>     NetCDF climate data exists from 2000-01-01 to 2000-01-14
#>     Data will be extracted from  2000-01-01  to  2000-01-14  at  1  locations 
#> Starting data extraction:
#> ... Building catchment weights for each grid.
#> Loading required namespace: ncdf4
#> ... Starting to extract data across all variable and locations:
#> ... Linearly interpolating gaps
#> ... Backfilling dates prior to the start of observations
#> ... Calculating area weighted results at required time-step.
#> Data extraction FINISHED.
#> Total run time (DD:HH:MM:SS): 00:00:00:04

# Extract the daily catchment average data.
climateDataAvg = climateData$catchmentTemporal.mean

# Extract the daily catchment variance data.
climateDataVar = climateData$catchmentSpatial.var

# Remove temp. files
unlink(ncdfFilename)
# }
```
