# Source data URLs and attributes.

`get.variableSource` get available variables, units and URLs to BoM
gridded data.

## Usage

``` r
get.variableSource()
```

## Value

data.frame of the source data location and properties required by the
package:

- `label`: string description of the variable,

- `units`: string for units of the variable.

- `time.step` : string for the time step of the data. `days` or `months`
  are accepted.

- `data.URL` : string of URL to the source gridded data.

- `data.file.extension` : string for the file extension of the
  downloaded compressed source data.

- `data.file.format` : string for file extension to the file required
  within the downloaded file.

- `ncdf.name` : string for the name of the variable once input to the
  package netCDF file.

- `ellipsoid.crs` : string for Coordinate Reference System (CRS) for the
  gridded data ellipsoid.

## Details

This function returns a list of available variables, unit, time step and
URLs used to download the meteorological data.

## Examples

``` r
vars = get.variableSource()
```
