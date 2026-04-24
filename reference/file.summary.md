# Summarise existing netCDF file.

`file.summary` sumarises the netCDF variables, units and date ranges.

## Usage

``` r
file.summary(ncfile)
```

## Arguments

- ncfile:

  file name of the netCDF data file built by this package.

## Value

data.frame summarising the attributes of each variable in the netCDF
file. The data.frame includes the following columns:

- `group`: string for the netCDF group in which the variable is placed,

- `var.string`: string for the group and variable name.

- `from` : Date variable for the start of the first time step containing
  data.

- `to` : Date variable for the end of the last time step containing
  data.

- `time.step` : string for the time step of the data.

- `time.datum` : string for time datum from which netCDF layers are
  indexed.

- `units` : string for units of the variable.

- `ellipsoid.crs` : string for Coordinate Reference System (CRS) for the
  gridded data ellipsoid.

## Details

This function opens an existing netCDF file built using the package and
returns a data.frame of variables, unit, stand and end dates of the
data.
