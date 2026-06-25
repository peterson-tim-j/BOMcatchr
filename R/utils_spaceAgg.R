# Define spatial averaging function
do.SpatialAggregation = function(data,
                                 location.lookup,
                                 w,
                                 location.ID) {

  cell.index = location.lookup[location.ID,1]:location.lookup[location.ID,2]
  w = w[cell.index]
  return(apply(t(t(data) * w),1,sum,na.rm=TRUE) )
}

# Define spatial averaging function
do.SpatialStatistic = function( data, fn) {
  return( apply(data, 1, fn, na.rm=TRUE) )
}
