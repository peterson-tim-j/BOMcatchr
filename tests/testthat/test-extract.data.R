#Test the extraction of data
test_that("netCDF grid can be created",
    {

      Sys.setenv(R_TESTS="")

      expect_no_error(
        {
          # Set dates for building netCDFs and extracting data from yesterday to one week ago.
          startDate = as.Date("2010-07-01","%Y-%m-%d")
          endDate = as.Date("2010-09-30","%Y-%m-%d")

          # Set names for netCDF files (in the system temp. directory).
          ncdfFilename = tempfile(fileext = '.nc')

          # Build netCDF grids for all data but only over the defined time period.
          build_status = grid_build(ncdfFilename=ncdfFilename,
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
      expect_shape(climateData.P$temporal, dim = c(6, 6))

      expect_no_error(
        {
          # Load the ET constants
          data(constants,package='Evapotranspiration')

          # Define derived ET vars
          derived_var_fns <- list(ET_HS = list( FUN = 'ET_HargreavesSamani',
                                                ET_missing_method = 'neighbouring average',
                                                ET_abnormal_method = 'neighbouring average',
                                                ET_constants = constants,
                                                ET_timestep = 'daily'),
                                  ET_JH = list( FUN = 'ET_JensenHaise',
                                                ET_missing_method = 'neighbouring average',
                                                ET_abnormal_method = 'neighbouring average',
                                                ET_constants = constants,
                                                ET_timestep = 'daily'),
                                  ET_M =  list( FUN = 'ET_Makkink',
                                                ET_missing_method = 'neighbouring average',
                                                ET_abnormal_method = 'neighbouring average',
                                                ET_constants = constants,
                                                ET_timestep = 'daily'),
                                  ET_MB = list( FUN = 'ET_McGuinnessBordne',
                                                ET_missing_method = 'neighbouring average',
                                                ET_abnormal_method = 'neighbouring average',
                                                ET_constants = constants,
                                                ET_timestep = 'daily'),
                                  ET_T =  list( FUN = 'ET_Turc',
                                                ET_missing_method = 'neighbouring average',
                                                ET_abnormal_method = 'neighbouring average',
                                                ET_constants = constants,
                                                ET_timestep = 'daily'),
                                  ET_CRAE_PET = list( FUN = 'ET_MortonCRAE',
                                                      ET_Mortons_est = 'potential ET',
                                                      ET_missing_method = 'neighbouring average',
                                                      ET_abnormal_method = 'neighbouring average',
                                                      ET_constants = constants,
                                                      ET_timestep = 'monthly'),
                                  ET_CRAE_aPET = list(FUN = 'ET_MortonCRAE',
                                                      ET_Mortons_est = 'wet areal ET',
                                                      ET_missing_method = 'neighbouring average',
                                                      ET_abnormal_method = 'neighbouring average',
                                                      ET_constants = constants,
                                                      ET_timestep = 'monthly'),
                                  ET_CRAE_aET = list( FUN = 'ET_MortonCRAE',
                                                      ET_Mortons_est = 'actual areal ET',
                                                      ET_missing_method = 'neighbouring average',
                                                      ET_abnormal_method = 'neighbouring average',
                                                      ET_constants = constants,
                                                      ET_timestep = 'monthly'),
                                  ET_CRWE_lake = list(FUN = 'ET_MortonCRWE',
                                                      ET_Mortons_est = 'shallow lake ET',
                                                      ET_missing_method = 'neighbouring average',
                                                      ET_abnormal_method = 'neighbouring average',
                                                      ET_constants = constants,
                                                      ET_timestep = 'monthly'),
                                  ET_CRWE_PET  = list(FUN = 'ET_MortonCRWE',
                                                      ET_Mortons_est = 'potential ET',
                                                      ET_missing_method = 'neighbouring average',
                                                      ET_abnormal_method = 'neighbouring average',
                                                      ET_constants = constants,
                                                      ET_timestep = 'monthly')
                              )

          # Extract catchment average data for Bet Bet Creek with
          # the Mortons CRAE estimate of potential ET.
          climateData.P_PET= extract_data(ncdfFilename=ncdfFilename,
                                                  extractFrom=startDate,
                                                  extractTo=endDate,
                                                  locations=catch,
                                                  vars = c('tmax', 'tmin', 'precip', 'vprp_3pm', 'solarrad'),
                                                  vars_derived = derived_var_fns,
                                                  temporal.timestep = 'monthly',
                                                  temporal.fn.inner = 'sum',
                                                  spatial.fn='var')
        },
        message='Testing extraction of P monthly and PET data.'
      )

      # Check outputs are data frames
      expect_type(climateData.P, 'list')
      expect_type(climateData.P_PET, 'list')

      # Test df dimensions
      expect_true(is.data.frame(climateData.P_PET$temporal))
      expect_shape(climateData.P_PET$temporal, dim = c(6, 5+5+10))

      # check data is finite
      expect_true(all(is.finite(climateData.P$temporal[,5])), 'Test precip results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,11])), 'Test PET col1 results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,12])), 'Test PET col2 results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,13])), 'Test PET col3 results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,14])), 'Test PET col4 results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,15])), 'Test PET col5 results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,16])), 'Test PET col6 results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,17])), 'Test PET col7 results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,18])), 'Test PET col8 results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,19])), 'Test PET col9 results are finite')
      expect_true(all(is.finite(climateData.P_PET$temporal[,20])), 'Test PET col10 results are finite')


      expect_no_error(
        {
          centroid = terra::centroids(catch)

          # Extract point monthly data P for Bet Bet Creek.
          climateData.P.centroid = extract_data(ncdfFilename=ncdfFilename,
                                      extractFrom=startDate,
                                      extractTo=endDate,
                                      locations=centroid,
                                      vars = c('precip'),
                                      temporal.timestep = 'weekly',
                                      temporal.fn.inner = 'sum',
                                      spatial.fn='var')
        },
        message='Testing extraction of point P weekly data.'
      )

      expect_no_error(
        {
          # Extract area weighted monthly data P for Bet Bet Creek.
          climateData.P.area = extract_data(ncdfFilename=ncdfFilename,
                                      extractFrom=startDate,
                                      extractTo=endDate,
                                      locations=catch,
                                      vars = c('precip'),
                                      temporal.timestep = 'weekly',
                                      temporal.fn.inner = 'sum')

          rmse = mean((climateData.P.area$temporal$precip - climateData.P.centroid$precip)^2)
          bias = mean(climateData.P.area$temporal$precip - climateData.P.centroid$precip)

          message(paste('.. RMSE b/w centroid and weighted area =', rmse, 'mm/week' ))
          message(paste('.. Bias b/w centroid and weighted area =', bias, 'mm/week' ))
        },
        message='Testing extraction of mapped P monthly data.'
      )

      expect_lt(
        {
          mean((climateData.P.area$temporal$precip - climateData.P.centroid$precip)^2)
        },
        1.4
      )

      expect_lt(
        {
          mean(climateData.P.area$temporal$precip - climateData.P.centroid$precip)
        },
        0.6
      )
    }
)
