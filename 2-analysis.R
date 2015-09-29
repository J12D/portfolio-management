library(reshape2)
library(ggplot2)

source("0-helper.R")
source("1-data.R")

returns %>% plotXTS
factors %>% plotXTS

# Mean
returns %>% apply(2,mean)
factors %>% apply(2,mean)

# Variance & Correlation
var(returns)
var(factors)
cor(returns)
cor(factors)

# scatter plot GDAXI vs DJI
plot.zoo(returns$GDAXI %>% zoo,returns$DJI %>% zoo)
