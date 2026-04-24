#Test the extraction of data
test_that("netCDF grid can be created",
    {

      Sys.setenv(R_TESTS="")

      expect_no_error(
        {
          # Set dates for building netCDFs and extracting data from yesterday to one week ago.
          startDate = as.Date("2010-08-01","%Y-%m-%d")
          endDate = as.Date("2010-09-30","%Y-%m-%d")

          # Set names for netCDF files (in the system temp. directory).
          ncdfFilename = tempfile(fileext = '.nc')

          # Build netCDF grids for all data but only over the defined time period.
          file.names = build.grids(ncdfFilename=ncdfFilename,
                                       updateFrom=startDate, updateTo=endDate)
        },
        message='Testing creaion of two month netCDF grids.'
      )

      expect_no_error(
        {
          # Load example catchment boundaries.
          data("catchments")

          # Extract catchment average monthly data P for Bet Bet Creek.
          climateData.P= extract.data(ncdfFilename=ncdfFilename,
                                              extractFrom=startDate, extractTo=endDate,
                                              locations=catchments,
                                              vars = c('precip'),
                                              temporal.timestep = 'monthly', temporal.function.name = 'sum',
                                              spatial.function.name='var');
        },
        message='Testing extraction of P monthly data.'
      )

      # Test df dimensions
      expect_true(is.data.frame(climateData.P$temporal.sum))
      expect_shape(climateData.P$temporal.sum, dim = c(4, 6))

      expect_no_error(
        {
          # Load the ET constants
          data(constants,package='Evapotranspiration')

          # Extract catchment average data for Bet Bet Creek with
          # the Mortons CRAE estimate of potential ET.
          climateData.P_PET= extract.data(ncdfFilename=ncdfFilename,
                                                  extractFrom=startDate, extractTo=endDate,
                                                  locations=catchments,
                                                  vars = c('tmax', 'tmin', 'precip', 'vprp', 'solarrad', 'et'),
                                                  temporal.timestep = 'monthly', temporal.function.name = 'sum',
                                                  spatial.function.name='var',
                                                  ET.function='ET.MortonCRAE',
                                                  ET.timestep='monthly', ET.constants=constants);
        },
        message='Testing extraction of P monthly and PET data.'
      )

      # Check outputs are data frames
      expect_type(climateData.P, 'list')
      expect_type(climateData.P_PET, 'list')

      # Test df dimensions
      expect_true(is.data.frame(climateData.P_PET$temporal.sum))
      expect_shape(climateData.P_PET$temporal.sum, dim = c(4, 11))

      # check data is finite
      expect_true(all(is.finite(climateData.P$temporal.sum[,5])), 'Test precip results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal.sum[,11])), 'Test PET results are finite')
    }
)
