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
          file.names = grid_build(ncdfFilename=ncdfFilename,
                                   updateFrom=startDate,
                                   updateTo=endDate)
        },
        message='Testing creation of two month netCDF grids.'
      )

      expect_no_error(
        {
          nstations <- BOMcatchr::extract_nstations(ncdfFilename)
        },
        message='Extracting number of weather stations over time.'
      )
      expect_true(is.list(nstations))
      expect_true(is.data.frame(nstations$precip))

      expect_no_error(
        {
          # Load example catchment boundaries.
          catch <- extract_locations('catchment',c('407214','407220'))

          # Extract catchment average monthly data P for Bet Bet Creek.
          climateData.P= extract_data(ncdfFilename=ncdfFilename,
                                              extractFrom=startDate,
                                              extractTo=endDate,
                                              locations=catch,
                                              vars = c('precip'),
                                              temporal.timestep = 'monthly',
                                              temporal.fn.inner = 'sum',
                                              spatial.fn='var')
        },
        message='Testing extraction of P monthly data.'
      )

      # Test df dimensions
      expect_true(is.data.frame(climateData.P$temporal))
      expect_shape(climateData.P$temporal, dim = c(4, 6))

      expect_no_error(
        {
          # Load the ET constants
          data(constants,package='Evapotranspiration')

          # Extract catchment average data for Bet Bet Creek with
          # the Mortons CRAE estimate of potential ET.
          climateData.P_PET= extract_data(ncdfFilename=ncdfFilename,
                                                  extractFrom=startDate,
                                                  extractTo=endDate,
                                                  locations=catch,
                                                  vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad', 'et'),
                                                  temporal.timestep = 'monthly',
                                                  temporal.fn.inner = 'sum',
                                                  spatial.fn='var',
                                                  ET.function='ET.MortonCRAE',
                                                  ET.timestep='monthly',
                                                  ET.constants=constants);
        },
        message='Testing extraction of P monthly and PET data.'
      )

      # Check outputs are data frames
      expect_type(climateData.P, 'list')
      expect_type(climateData.P_PET, 'list')

      # Test df dimensions
      expect_true(is.data.frame(climateData.P_PET$temporal))
      expect_shape(climateData.P_PET$temporal, dim = c(4, 11))

      # check data is finite
      expect_true(all(is.finite(climateData.P$temporal[,5])), 'Test precip results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,11])), 'Test PET results are finite')


      expect_no_error(
        {
          centroid = terra::centroids(catch)

          # Extract point monthly data P for Bet Bet Creek.
          climateData.P= extract_data(ncdfFilename=ncdfFilename,
                                      extractFrom=startDate,
                                      extractTo=endDate,
                                      locations=centroid,
                                      vars = c('precip'),
                                      temporal.timestep = 'weekly',
                                      temporal.fn.inner = 'sum',
                                      spatial.fn='var');
        },
        message='Testing extraction of point P weekly data.'
      )

      expect_no_error(
        {
          # Extract point monthly data P for Bet Bet Creek.
          climateData.P= extract_data(ncdfFilename=ncdfFilename,
                                      extractFrom=startDate,
                                      extractTo=endDate,
                                      locations=catch,
                                      vars = c('precip'),
                                      temporal.timestep = 'monthly',
                                      temporal.fn.inner = 'sum');
        },
        message='Testing extraction of mapped P monthly data.'
      )
    }
)
