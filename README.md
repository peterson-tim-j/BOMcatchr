<!-- badges: start -->

[![CRANversion](http://www.r-pkg.org/badges/version/AWAPer)](https://cran.r-project.org/package=AWAPer) 
![Windows](https://github.com/peterson-tim-j/AWAPer/actions/workflows/windows-latest.yml/badge.svg)
![Ubuntu](https://github.com/peterson-tim-j/AWAPer/actions/workflows/ubuntu-latest.yml/badge.svg)
![Macos](https://github.com/peterson-tim-j/AWAPer/actions/workflows/macos-latest.yml/badge.svg)
[![Codecov](https://img.shields.io/codecov/c/github/peterson-tim-j/AWAPer?logo=CODECOV)](https://app.codecov.io/github/peterson-tim-j/AWAPer) 
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/AWAPer)](https://cran.r-project.org/package=AWAPer)  [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/AWAPer)](https://cran.r-project.org/package=AWAPer)
<!-- badges: end -->

# _AWAPer_ - an R-package for catchment-weighted climate data anywhere in Australia
This R package builds netCDF files of the Bureau of Meteorology Australian Water Availability Project daily national climate grids and allows efficient extraction of daily, weekly, monthly, quarterly or annual catchment average precipitation, Tmin, Tmax, vapour pressure, solar radiation and then estimation of various measures of potential evaporation. 

The package development was funded by the Victorian Government The Department of Environment, Land, Water and Planning _Climate and Water Initiate_ (https://www.water.vic.gov.au/climate-change/research/vicwaci). 

For details of the approach see the paper "Peterson, Tim J, Wasko, C, Saft, M, Peel, MC. AWAPer: An R package for area weighted catchment daily meteorological data anywhere within Australia. _Hydrological Processes_. 2020; 34: 1301– 1306. https://doi.org/10.1002/hyp.13637". 

Using the default compression settings, each meteorological variable requires ~5GB of hard-drive storage for the full record (1900 to 2019). Additionally, the netCDF files should be stored locally, and not over a network, to minimise the time for data extraction. Below are details of the system requirements, how to install AWAPer and the following examples:
1. Building and updating the required netCDF files (![see here](https://github.com/peterson-tim-j/AWAPer#example-1-build-netcdf-files))
1. Extract and check daily point precipitation data against rain gauge observations (![see here](https://github.com/peterson-tim-j/AWAPer#example-2-extract-point-precip-data-and-check-with-osberved-data))
1. Extract daily areal weighted precipitation and calculate two measures of ET (![see here](https://github.com/peterson-tim-j/AWAPer#example-3-calculate-precip-and-evapotranspiration))
1. Calculate all measures of ET possible with AWAPer (![see here](https://github.com/peterson-tim-j/AWAPer#example-4-calculate-evapotranspiration))

# System Requiements
On Windows OS only the program "7z" is required to uzip the ".Z" compressed grid files. Follow the steps below to download and install 7z.

1. Download and intall 7z from https://www.7-zip.org/download.html
1. Click "Search Windows", search "Edit environmental variables for your account" and click on it.
1. In the "User variables" window, select the "Path", and click "Edit...".
1. In the "Edit environmental variable" window, click "New".
1. Paste the path to the 7zip application folder, and click OK.
1. Restart Windows.
1. Check the setup by opening the "Command Prompt" and enter the command "7z". If 7z is correctly setup, output details such as the version, descriptions of commands, etc should be shown.')

# Getting Started
The package will soon be submitted to the R library (i.e. CRAN). In the meantime, you can install it from github using the following R command:

```R
remotes::install_github("peterson-tim-j/AWAPer")
```

Once installed, browse the package examples using the R-command:
```R
browseVignettes("AWAPer")
```
