calcMDD <- function(asset) {
  asset <- rev(asset)
  maxDiffPortVal <- as.numeric(asset[2]) - as.numeric(asset[1])
  minPortVal <- as.numeric(asset[1])
  for (i in 1:(length(asset))) {
    if (as.numeric(asset[i]) - minPortVal > maxDiffPortVal)
    {
      maxDiffPortVal <- as.numeric(asset[i]) - minPortVal
    }
    if (as.numeric(asset[i]) < minPortVal)
    {
      minPortVal <- as.numeric(asset[i])
    }
  }
  mdd <- max(0, (minPortVal + maxDiffPortVal) / minPortVal - 1) * 100
  mdd
}


rollingWindow <- function(asset) {
  indexMonthlyEndpoints <- endpoints(asset, on = "months", k = 1)
  indexMonthlyEndpoints <-
    indexMonthlyEndpoints[2:length(indexMonthlyEndpoints)] #throw away index of 0
  indexMonthlyEndpoints
}

calcMDDRollingWindow <- function(asset) {
  indexMonthlyEndpoints <- rollingWindow(asset)
  
  countRollingWindow <- 12
  helper <- 1
  mdd <- 0
  for (countRollingWindow in length(indexMonthlyEndpoints)) {
    if (countRollingWindow != 12) {
      helper <- (indexMonthlyEndpoints[countRollingWindow - 12] + 1)
    }
    dataRollingWindow <-
      asset[helper:indexMonthlyEndpoints[countRollingWindow]]
    mdd <- mdd + calcMDD(dataRollingWindow)
  }
  mdd <- mdd / (length(indexMonthlyEndpoints) - 13)
  mdd
}

asset <- assets[,2]
calcMDDRollingWindow(asset)
