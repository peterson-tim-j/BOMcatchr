# Changelog

## AWAPer 0.1.50

- Function again after being removed from CRAN because of BOM download
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
