#Test the create of netCDF files
test_that("netCDF grid can be created",
    {

      Sys.setenv(R_TESTS="")
      expect_no_error(
        {
          # Set dates for building netCDFs and extracting data for two months
          startDate = as.Date(format( Sys.Date()-62,"%Y-%m-01"),'%Y-%m-%d')
          endDate = as.Date(format( Sys.Date(),"%Y-%m-01"),'%Y-%m-%d')-1

          # Set names for netCDF files (in the system temp. directory).
          ncdfFilename = tempfile(fileext = '.nc')

          # Build netCDF grids for all data but only over the defined time period.
          ncdfFilename= build.grids(ncdfFilename=ncdfFilename,
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
          summary.df <- BOMcatchr::grid.summary(ncdfFilename)

          summary.df
        },
        message='Testing opening of netCDF grids and reading summary info.'
      )

      # Test the summary.df is a data frame and dimensions
      expect_true(is.data.frame(summary.df))
      expect_shape(summary.df, dim = c(7, 8))

      # Update netcDF grids and expect no errors
      expect_no_error(
        {
          endDate = startDate
          startDate = as.Date(format( Sys.Date()-93,"%Y-%m-01"),'%Y-%m-%d')
          ncdfFilename = build.grids(ncdfFilename=ncdfFilename,
                          updateFrom=startDate, updateTo=endDate)
        },
        message='Testing updating of netCDF grids by two days prior'
      )

      # Test the netcdf files can be opened.
      expect_no_error(
        {
          summary.df <- BOMcatchr::grid.summary(ncdfFilename)
        },
        message='Testing opening of netCDF grids and reading summary info.'
      )

      # Test the summary.df is a data frame and dimensions
      expect_true(is.data.frame(summary.df))
      expect_shape(summary.df, dim = c(7, 8))
    }
)
