# Changelog

## AWAPer 0.2.00

- Final version of AWAPer before function names changed from that
  published in the paper.

- Improvements to robustness of data extraction made, eg. daily and
  monthly data extracted in one call.

- Water year functionality fixed and vignette added.

- Documentation expanded to detail returned data.frames.

## AWAPer 0.1.60

- Major improvements to robustness and the data sources handled.

- All data is now stored with one netCDF files. This is achieved using
  netCDF groups.

- All source data attributes (e.g. url, time step, spatial datum) are
  now no longer hard coded, but rather supplied by get.variableSource().
  This allows updating for source data information without package
  changes.

- Monthly source precipitation grids now handled (see \[here\]
  (<https://www.bom.gov.au/climate/austmaps/about-agcd-maps.shtml>) for
  details of monthly gridded data).

- Vignettes extended to better evaluate results against rain gauge data
  and demonstrate the new features.  

- Stronger error handling of input dates and ET settings.

- Summary of build netcDF file now provided by file.summary()

## AWAPer 0.1.50

- Functional again after being removed from CRAN because of BOM download
  issues! 🎉

- [extractCatchmentData](https://peterson-tim-j.github.io/AWAPer/reference/extractCatchmentData.html)
  function modified to no longer require download of a digital elevation
  model (DEM) when calculating ET. Instead the
  [elevatr](https://cran.r-project.org/web/packages/elevatr/index.html)
  package is used to download elevations from an Amazon Web Service.

- Vignette:

  - [Getting
    Started](https://peterson-tim-j.github.io/AWAPer/articles/AWAPer.html)
  - [Making and update data
    grids](https://peterson-tim-j.github.io/AWAPer/articles/A_Make_data_grids.html)
  - [Extracting time-series point
    data](https://peterson-tim-j.github.io/AWAPer/articles/B_Point_rainfall.html)
  - [Extracting time-series area weighted
    data](https://peterson-tim-j.github.io/AWAPer/articles/C_Catchment_avg_ET_rainfall.html)
  - [Extracting time-series ET area weighted
    data](https://peterson-tim-j.github.io/AWAPer/articles/D_Catchment_avg_ET_types.html)
