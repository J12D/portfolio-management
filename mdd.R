calcMDD <- function(asset) {
  asset <- rev(asset)
  if (length(asset) < 2) return(0)
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

assets[,1] %>% apply.monthly(calcMDD) %>% mean