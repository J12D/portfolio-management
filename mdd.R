calcMDD <- function(asset) {
  asset <- rev(asset)
  
  if (as.numeric(asset[2]) - as.numeric(asset[1]) > 0) {
    maxDiffPortVal <- as.numeric(asset[2]) - as.numeric(asset[1])
    minPortVal <- as.numeric(asset[1])
  }
  else{
    maxDiffPortVal <- 0
    minPortVal <- as.numeric(asset[1])
  }
  
  for (i in 1:(length(asset) - 1)) {
    for (j in (i + 1):(length(asset))) {
      if (maxDiffPortVal < as.numeric(asset[j]) - as.numeric(asset[i])) {
        maxDiffPortVal <- as.numeric(asset[j]) - as.numeric(asset[i])
        minPortVal <- as.numeric(asset[i])
      }
    }
  }
  mdd <- max(0, (minPortVal + maxDiffPortVal) / minPortVal - 1) * 100
  mdd
}

getMDD <- function(returns) {
  returns %>% (function(x)
    x[endpoints(x),]) %>%
    rollapply(12, calcMDD) %>% na.omit %>%
    (function(x)
      sum(x) / (length(x) - 13))
}