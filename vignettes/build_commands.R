# This R scrpt details the step required to build the CRAN .tar.gz file for submission to CRAN and how to build the manual PDF.
#------------------------------------------------

# Build PDF. If AWAPer.pdf already exists, then delete before running.
path <- find.package("AWAPer")
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

library(knitr)
library(rmarkdown)

# Convert vignette *.Rnw source diles to a Knitr *.Rms
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

# devtools checks added
devtools::check('C:/Users/tpet0008/Documents/AWAPer',args=c("--no-examples"))

# Build the pavkage for CRAN
devtools::build(".")
