#Test the create of netCDF files
test_that("netCDF grid can be created",
    {

      Sys.setenv(R_TESTS="")
      expect_no_error(
        {
          # Set dates for building netCDFs and extracting data from yesterday to one week ago.
          startDate = Sys.Date()-9
          endDate = Sys.Date()-2

          # define temp direcory for netCDF files
          fdir = tempdir()
          setwd(fdir)

          # Set names for netCDF files (in the system temp. directory).
          ncdfFilename = tempfile(fileext = '.nc')

          # Build netCDF grids for all data but only over the defined time period.
          file.names = makeNetCDF_file(ncdfFilename=ncdfFilename,
                                       updateFrom=startDate, updateTo=endDate)
        },
        message='Testing creaion of netCDF grids.'
      )

      # Test the files were created
      expect_true(file.exists(ncdfFilename),'Testing creation of netCDF file for data from one week ago')

      # test file size is reasonably large
      expect_gt(file.info(ncdfFilename)$size, 1E6,'Testing netCDF file size is > 1MB')

      # Test the netcdf files can be opened.
      expect_no_error(
        {
          summary.df <- AWAPer::file.summary(ncdfFilename)

          summary.df
        },
        message='Testing opening of netCDF grids and reading summary info.'
      )

      # Test the summary.df is a data frame and dimensions
      expect_true(is.data.frame(summary.df))
      expect_shape(summary.df, dim = c(5, 7))

      # Update netcDF grids and expect no errors
      expect_no_error(
        {
          endDate = startDate
          startDate = Sys.Date()-11
          makeNetCDF_file(ncdfFilename=ncdfFilename,
                          updateFrom=startDate, updateTo=endDate)
        },
        message='Testing updating of netCDF grids by two days prior'
      )

      # Test the netcdf files can be opened.
      expect_no_error(
        {
          summary.df <- AWAPer::file.summary(ncdfFilename)
        },
        message='Testing opening of netCDF grids and reading summary info.'
      )

      # Test the summary.df is a data frame and dimensions
      expect_true(is.data.frame(summary.df))
      expect_shape(summary.df, dim = c(5, 7))
    }
)
