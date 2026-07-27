#Test the extraction of data
test_that("BOM Geofabric data can be downloaded",
    {

      Sys.setenv(R_TESTS="")

      # States
      expect_no_error(
        {
          v <- extract_locations('state','VIC')
        },
        message='Testing Vic data downloads.'
      )
      expect_no_error(
        {
          v <- extract_locations('state',NA)
        },
        message='Testing all state data downloads.'
      )
      expect_true(inherits(v, "SpatVector"))

      # One basin
      expect_no_error(
        {
          v <- extract_locations('river_region',43637156)
        },
        message='Testing Loddon data download.'
      )
      expect_true(inherits(v, "SpatVector"))


      # Simple rivers
      expect_no_error(
        {
          v <- extract_locations('river_simple','MURRAY RIVER')
        },
        message='Testing one simple river data downloads.'
      )
      expect_no_error(
        {
          v <- extract_locations('state',NA)
        },
        message='Testing all rivers to download.'
      )
      expect_true(inherits(v, "SpatVector"))

      # Stream gauges
      expect_no_error(
        {
          v <- extract_locations('water_gauge',c("616005", "616006"))
        },
        message='Testing two stream gauges to download.'
      )
      expect_no_error(
        {
          v <- extract_locations('water_gauge',NA)
        },
        message='Testing all gauges to download.'
      )
      expect_true(inherits(v, "SpatVector"))

      # Catchment boundaries
      expect_no_error(
        {
          v <- extract_locations('catchment',c('407214','407220'))
        },
        message='Testing two boundaries to download.'
      )
      expect_true(inherits(v, "SpatVector"))

      # Aquifer upper
      expect_no_error(
        {
          v <- extract_locations('aquifer_upper',c(130000046, 130000188))
        },
        message='Testing upper aquifer boundaries.'
      )
      expect_true(inherits(v, "SpatVector"))

      # Aquifer mid
      expect_no_error(
        {
          v <- extract_locations('aquifer_mid',c(130000242, 130000188))
        },
        message='Testing mid aquifer boundaries.'
      )
      expect_true(inherits(v, "SpatVector"))

      # Aquifer aquifer_lower
      expect_no_error(
        {
          v <- extract_locations('aquifer_mid',c(130000052, 130000051))
        },
        message='Testing lower aquifer boundaries.'
      )
      expect_true(inherits(v, "SpatVector"))

    }
)
