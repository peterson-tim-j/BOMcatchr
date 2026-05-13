# *BOMcatchr* - an R-package for catchment-weighted climate data anywhere in Australia

Getting rainfall and evaporation time-series data is an essential first
step for many hydrological studies. Too often this requires the tedious
task of finding a weather station that is sufficiently close to the
study catchment. Rarely is there a weather station inside the catchment,
but even if there is then the spatial variability of rainfall and
evaporation throughout the catchment must be ignores - and errors
introduced (see example
[here](https://peterson-tim-j.github.io/BOMcatchr/articles/C_Catchment_avg_ET_rainfall.html#point-versus-area-weighted-rainfall-deficit)).
*BOMcatchr* eliminates these challenges by allowing efficient estimate
of area weighted (i.e. catchment weighted) weather time-series data
anywhere in Australia and at any time step.

Some of the features include:

1.  Area weighted and point precipitation, minimum and maximum
    temperature, vapour pressure and solar radiation anywhere in
    Australia at any time-step. See examples
    [here](https://peterson-tim-j.github.io/BOMcatchr/articles/B_Point_rainfall.html)
    and
    [here](https://peterson-tim-j.github.io/BOMcatchr/articles/C_Catchment_avg_ET_rainfall.html#extract-daily-precipitation-and-pet-data).
2.  Maps of catchment weather variables over time showing the spatial
    variability in precipitation and potential evapotranspiration (see
    examples
    [here](https://peterson-tim-j.github.io/BOMcatchr/articles/C_Catchment_avg_ET_rainfall.html#extract-and-map-monthly-total-precipitation-and-pet))
3.  Ten measures of evapotranspiration (see examples
    [here](https://peterson-tim-j.github.io/BOMcatchr/articles/D_Catchment_avg_ET_types.html))
    using the [Evaporation
    package](https://cran.r-project.org/web/packages/Evapotranspiration/index.html),
    implementing [McMahon et al,
    (2013)](https://hess.copernicus.org/articles/17/1331/2013/hess-17-1331-2013.pdf),
4.  Water-year total rainfall with estimated rain gauge interpolation
    error (see examples
    [here](https://peterson-tim-j.github.io/BOMcatchr/articles/E_Water_year_catchment_rainfall.html))

The meteorological data is sourced from Australian Bureau of Meteorology
(BOM) ~5x5km gridded products (see
[here](https://awo.bom.gov.au/about/overview)). The package functions by
building compressed netCDF grids from the BOM data. Users generally
build the netCDF grids using all historic data and then update as
required (see example
[here](https://peterson-tim-j.github.io/BOMcatchr/articles/A_Make_data_grids.html)).

The package development was funded by the Victorian Government The
Department of Environment, Land, Water and Planning [*Climate and Water
Initiate*](https://www.water.vic.gov.au/climate-change/research/vicwaci).
For details of the approach see the [function
references](https://peterson-tim-j.github.io/BOMcatchr/reference/index.html)
or the journal paper [*Peterson et
al. (2020)*](https://doi.org/10.1002/hyp.13637).

# Installation

The package will soon be submitted to the R library (i.e. CRAN). In the
meantime, you can install it from github using the following R command:

``` r

remotes::install_github("peterson-tim-j/BOMcatchr")
```

Once installed, browse the package examples using the R-command:

``` r

browseVignettes("BOMcatchr")
```

# Windows System Requiements

On Windows OS, the program “7z” is required to uzip the “.Z” compressed
grid files.

Follow the steps below to download and install 7z.

1.  Download and intall 7z from <https://www.7-zip.org/download.html>
2.  Click “Search Windows”, search “Edit environmental variables for
    your account” and click on it.
3.  In the “User variables” window, select the “Path”, and click
    “Edit…”.
4.  In the “Edit environmental variable” window, click “New”.
5.  Paste the path to the 7zip application folder, and click OK.
6.  Restart Windows.
7.  Check the setup by opening the “Command Prompt” and enter the
    command “7z”. If 7z is correctly setup, output details such as the
    version, descriptions of commands, etc should be shown.’)
