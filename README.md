<!-- badges: start -->

[![CRANversion](http://www.r-pkg.org/badges/version/BOMcatchr)](https://cran.r-project.org/package=BOMcatchr) [![windows-latest](https://github.com/peterson-tim-j/BOMcatchr/actions/workflows/windows-latest.yml/badge.svg)](https://github.com/peterson-tim-j/BOMcatchr/actions/workflows/windows-latest.yml) [![ubuntu-latest](https://github.com/peterson-tim-j/BOMcatchr/actions/workflows/ubuntu-latest.yml/badge.svg)](https://github.com/peterson-tim-j/BOMcatchr/actions/workflows/ubuntu-latest.yml) [![macos-latest](https://github.com/peterson-tim-j/BOMcatchr/actions/workflows/macos-latest.yml/badge.svg)](https://github.com/peterson-tim-j/BOMcatchr/actions/workflows/macos-latest.yml) [![Codecov](https://img.shields.io/codecov/c/github/peterson-tim-j/BOMcatchr?logo=CODECOV)](https://app.codecov.io/github/peterson-tim-j/BOMcatchr) 
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/BOMcatchr)](https://cran.r-project.org/package=BOMcatchr)  [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/BOMcatchr)](https://cran.r-project.org/package=BOMcatchr)
<!-- badges: end -->

# _BOMcatchr_ - an R-package for catchment-weighted climate data anywhere in Australia
Getting rainfall and evaporation time-series data is an essential first step for many hydrological studies. Too often this requires the tedious task of finding a weather station that is sufficiently close to the study catchment. Rarely is there a weather station inside the catchment, but even if there is then the spatial variability of rainfall and evaporation throughout the catchment must be ignores - and errors introduced (see example [here](https://peterson-tim-j.github.io/BOMcatchr/articles/C_Catchment_avg_ET_rainfall.html#point-versus-area-weighted-rainfall-deficit)). _BOMcatchr_ eliminates these challenges by allowing efficient estimate of area weighted (i.e. catchment weighted) weather time-series data anywhere in Australia and at any time step.   

Some of the features include:

1. Area weighted and point precipitation, minimum and maximum temperature, vapour pressure and solar radiation anywhere in Australia at any time-step. See examples [here](https://peterson-tim-j.github.io/BOMcatchr/articles/B_Point_rainfall.html) and [here](https://peterson-tim-j.github.io/BOMcatchr/articles/C_Catchment_avg_ET_rainfall.html#extract-daily-precipitation-and-pet-data). 
1. Maps of catchment weather variables over time showing the spatial variability in precipitation and potential evapotranspiration (see examples [here](https://peterson-tim-j.github.io/BOMcatchr/articles/C_Catchment_avg_ET_rainfall.html#extract-and-map-monthly-total-precipitation-and-pet))
1. Ten measures of area weighted and point evapotranspiration (see examples [here](https://peterson-tim-j.github.io/BOMcatchr/articles/D_Catchment_avg_ET_types.html)) using the [Evaporation package](https://cran.r-project.org/web/packages/Evapotranspiration/index.html), implementing [McMahon et al, (2013)](https://hess.copernicus.org/articles/17/1331/2013/hess-17-1331-2013.pdf), 
1. Water-year and seasonal area precipitation with error estimates (see examples [here](https://peterson-tim-j.github.io/BOMcatchr/articles/C_Catchment_avg_ET_rainfall.html#Water-year-and-seasonal-area-precipitation-and-error-estimates))

The meteorological data is sourced from Australian Bureau of Meteorology (BOM) ~5x5km gridded products (see [here](https://awo.bom.gov.au/about/overview)). The package functions by building compressed netCDF grids from the BOM data. Users generally build the netCDF grids using all historic data and then update as required (see example [here](https://peterson-tim-j.github.io/BOMcatchr/articles/A_Make_data_grids.html)). 

The package development was funded by the Victorian Government The Department of Environment, Land, Water and Planning [_Climate and Water Initiate_](https://www.water.vic.gov.au/climate-change/research/vicwaci). It was originally named _AWAPer_, but in 2026 was heavily revised and renamed to _BOMcatchr_. 

For details of the approach see the [function references](https://peterson-tim-j.github.io/BOMcatchr/reference/index.html) or the journal paper [_Peterson et al. (2020)_](https://doi.org/10.1002/hyp.13637). 

# Installation
The package will soon be submitted to the R library (i.e. CRAN). In the meantime, you can install it from github using the following R command:

```R
remotes::install_github("peterson-tim-j/BOMcatchr")
```

Once installed, browse the package examples using the R-command:
```R
browseVignettes("BOMcatchr")
```

# Support research
Evidence of research impact helps future research. Please cite the following paper to help us demonstrate impact. 

```
Peterson T. J., Wasko C., Saft M. and Peel M. (2020), AWAPer: An R package for area weighted catchment daily meteorological data anywhere within
  Australia, Hydrological Processes. 34: 1301– 1306. https://doi.org/10.1002/hyp.13637
```

Alternatively, the bibTeX entry for LaTeX users is:
```
  @Article{,
    author = {T. J. Peterson and C. Wasko and M. Saft and M. Peel},
    title = {AWAPer: An R package for area weighted catchment daily meteorological data anywhere within Australia},
    journal = {Hydrological Processes},
    year = {2020},
    volume = {34},
    page = {1301– 1306},
    doi = {10.1002/hyp.13637},
  }
```

Or use the following command to get the citation details.
```R
citation('BOMcatchr')
```
