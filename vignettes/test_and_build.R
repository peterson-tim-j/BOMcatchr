# This R scrpt details the step required to build the CRAN .tar.gz file for submission to CRAN and how to build the manual PDF.
#------------------------------------------------

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
devtools::check('C:/Users/tpet0008/Documents/AWAPer', vignettes=F)

# Convert vignette *.Rnw source diles to a Knitr *.Rms
library(knitr)
library(rmarkdown)
knitr::knit("vignettes/AWAPer.Rnw", output = "vignettes/AWAPer.Rmd")
knitr::knit("vignettes/A_Make_data_grids.Rnw", output = "vignettes/A_Make_data_grids.Rmd")
knitr::knit("vignettes/B_Point_rainfall.Rnw", output = "vignettes/B_Point_rainfall.Rmd")
knitr::knit("vignettes/C_Catchment_avg_ET_rainfall.Rnw", output = "vignettes/C_Catchment_avg_ET_rainfall.Rmd")
knitr::knit("vignettes/D_Catchment_avg_ET_types.Rnw", output = "vignettes/D_Catchment_avg_ET_types.Rmd")

# Pre-build the vignette *.Rmd files
devtools::build_vignettes()

# View vignettes
devtools::load_all()
browseVignettes("AWAPer")

# Move vignettes to inst/doc so that they're prebuilt.
file.rename(from = 'doc/', to = 'inst/doc')

# Update github.io page using pkgdown()
usethis::use_pkgdown_github_pages()
pkgdown::build_site()

# Build the pavkage for CRAN
devtools::build(".")
