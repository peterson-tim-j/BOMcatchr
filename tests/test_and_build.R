# This R scrpt details the step required to build the CRAN .tar.gz file for submission to CRAN and how to build the manual PDF.
#------------------------------------------------

setwd('AWAPer')

# Build docs
library(roxygen2)
devtools::document()

# Build PDF. If AWAPer.pdf already exists, then delete before running.
path <- find.package("AWAPer")
file.remove('AWAPer.pdf')
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

# Run unit tests
testthat::test_dir("tests/testthat")

# devtools checks added
setwd('..')
devtools::check('AWAPer', vignettes=F)

# Convert vignette *.Rnw source diles to a Knitr *.Rms
library(knitr)
library(rmarkdown)
setwd('AWAPer')
devtools::clean_vignettes()
knitr::knit("vignettes/AWAPer.Rnw", output = "vignettes/AWAPer.Rmd")
knitr::knit("vignettes/A_Make_data_grids.Rnw", output = "vignettes/A_Make_data_grids.Rmd")
knitr::knit("vignettes/B_Point_rainfall.Rnw", output = "vignettes/B_Point_rainfall.Rmd")
knitr::knit("vignettes/C_Catchment_avg_ET_rainfall.Rnw", output = "vignettes/C_Catchment_avg_ET_rainfall.Rmd")
knitr::knit("vignettes/D_Catchment_avg_ET_types.Rnw", output = "vignettes/D_Catchment_avg_ET_types.Rmd")
knitr::knit("vignettes/E_Water_year_catchment_rainfall.Rnw", output = "vignettes/E_Water_year_catchment_rainfall.Rmd")

# Remove vignettes from .Rbuildignore. This is done to ensure vignettes are built
lines <- readLines(".Rbuildignore")
cleaned_lines <- lines[!grepl("vignettes", lines)]
writeLines(cleaned_lines, ".Rbuildignore")

# Pre-build the vignette *.Rmd files
devtools::build_vignettes()

# Add vignettes back into .Rbuildignore
lines <- readLines(".Rbuildignore")
lines <- c(lines, "^vignettes$", "^vignettes/.*")
writeLines(lines, ".Rbuildignore")

# View vignettes
devtools::load_all()
browseVignettes("AWAPer")

# Move vignettes to inst/doc so that they're prebuilt.
file.rename(from = 'doc/', to = 'inst/doc')

# Update github.io page using pkgdown()
usethis::use_pkgdown_github_pages()
pkgdown::build_site()

# Build the package for CRAN
devtools::build(".")
