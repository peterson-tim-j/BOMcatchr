# Interpolate gaps in observed period of data
.do_interpolation = function(data, time.points, method) {
  for (j in 1:ncol(data)) {
    filt = is.na(data[,j])
    x = 1:length(time.points)
    xpred = x[filt]

    # Interpolate if any NAs
    if (length(xpred)>0) {
      x = x[!filt]
      y = data[!filt,j]

      # Interpolate if at least 2 non-NA obs.
      if (length(y)>1) {
        if (any( method==c('constant','linear'))) {
          ypred=stats::approx(x, y, xpred, method=method, rule=1:2)
        } else
          ypred=stats::spline(x, y, method=method, xout=xpred)

        data[filt,j] = ypred$y
      }
    }
  }

  return(data)
}

# beck fill dates prior to the start of the record
.do_backfilling <- function(data, time.points, fn) {

  # Get number of grid cells to est
  npoints = ncol(data)

  extractYear = as.numeric(format(time.points,"%Y"))
  extractMonth = as.numeric(format(time.points,"%m"))
  extractDay = as.numeric(format(time.points,"%d"))

  # Calculate the average daily solar radiation for each day of the year.
  monthdayUnique = sort(unique(extractMonth*100+extractDay));
  day = as.integer(format(time.points, "%d"));
  month = as.integer(format(time.points, "%m"));
  monthdayAll = month*100+day;
  data_avg = matrix(NA, length(monthdayUnique), npoints);
  for (j in 1:length(monthdayUnique)) {
    ind = monthdayAll==monthdayUnique[j];
    if (sum(ind)==1) {
      data_avg[j,] = data[ind,];
    } else {
      if (npoints==1) {
        data_avg[j,] = mean(data[ind,],na.rm=T)
      } else {
        data_avg[j,] = apply(stats::na.omit(data[ind,]),2,fn)
      }
    }
  }

  # Assign the daily average solar radiation to each day where all obs are NA
  for (j in 1:length(time.points)) {
    if (all(is.na(data[j, ]))) {
      ind = which(monthdayUnique==monthdayAll[j])
      if (length(ind)==1)
        data[j,] = data_avg[ind,]
    }
  }

  return(data)
}
