# Getting started

## Introduction

To get started, the key steps are:

1.  Build the required netCDF grids of weather data.
2.  Extract the required weather data at user defined locations,
    duration and time-step.
3.  Update the data grids as new data becomes available.

Each of the steps is outlined below.

## Build netCDF data

The netCDF data grids are created using the function
[build.grids](https://peterson-tim-j.github.io/BOMcatchr/reference/build.grids.html).
This requires user input of the following:

1.  *ncdfFilename* to define where the netCDF file should be created.
    Note, using the default compression settings, each meteorological
    variable requires ~5GB of hard-drive storage for the full record
    (1900 to now). Additionally, the netCDF files should be stored
    locally, and not over a network, to minimise the time for data
    extraction.
2.  *updateFrom* : start date for the data. Users often set this to the
    default date `as.Date('1900-01-01')` to extract all available data.
3.  *updateTo* : end date for the data. Users often set this to the
    default date of two days ago to extract all available data.
4.  *urlPrecip* etc to define which meteorological variables are to be
    input to the netCDF data grids. Setting any of them to *NA* causes
    that variable to not be included in the netCDF data grids.

See
[here](https://peterson-tim-j.github.io/BOMcatchr/articles/A_Make_data_grids.html)
for an example of how to build your first netCDF data file.

## Extract weather data

Once the netCDF data grids are created, extract data using the
[extract.data](https://peterson-tim-j.github.io/BOMcatchr/reference/extract.data.html)
function. The essential user inputs are:

1.  *ncdfFilename* file name pointing to the netCDF file.
2.  *locations* defining the locations at which the data is to be
    xtracted. it can be a full file name to an ESRI shape file of points
    or polygons or a shape file already imported using
    readShapeSpatial(). Either way the shape file must be in long/lat
    (i.e. not projected), use the ellipsoid GRS 80, and the first column
    must be a unique ID.

Many other user controls are available that control the duration,
time-step, the spatial and temporal aggregation (e.g. *mean*, *sum*,
*min* or *max*), the meteorological variables required and the measure
of evapotranspiration. For examples of time-series extractions see
[here](https://peterson-tim-j.github.io/BOMcatchr/articles/B_Point_rainfall.html)
and
[here](https://peterson-tim-j.github.io/BOMcatchr/articles/C_Catchment_avg_ET_rainfall.html#extract-daily-precipitation-and-pet-data).
For examples of spatial data extractions see
[here](https://peterson-tim-j.github.io/BOMcatchr/articles/C_Catchment_avg_ET_rainfall.html#extract-and-map-monthly-total-precipitation-and-pet)).

## Update netCDF data

Over time, the existing netCDF data can be updated using the function
[build.grids](https://peterson-tim-j.github.io/BOMcatchr/reference/build.grids.html).
Specifically, when an existing netCDF file is input, then the function
updates the data between *updateFrom* and *updateTo*. These dates are
automatically adjusted to ensure there are not date gaps in the data
grids. Also, any data outside this periuod is not modified. See
[here](https://peterson-tim-j.github.io/BOMcatchr/articles/A_Make_data_grids.html#update-existing-data-grids)
for an exanmple.
