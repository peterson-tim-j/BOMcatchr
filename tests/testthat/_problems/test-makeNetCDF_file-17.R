# Extracted from test-makeNetCDF_file.R:17

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "AWAPer", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
Sys.setenv(R_TESTS="")
expect_no_error(
        {
          # Set dates for building netCDFs and extracting data from yesterday to one week ago.
          startDate = Sys.Date()-9
          endDate = Sys.Date()-2

          # Set names for netCDF files (in the system temp. directory).
          ncdfFilename = tempfile(fileext = '.nc')

          # Build netCDF grids for all data but only over the defined time period.
          file.names = makeNetCDF_file(ncdfFilename=ncdfFilename,
                                       updateFrom=startDate, updateTo=endDate)
        },
        message='Testing creaion of netCDF grids.'
      )
