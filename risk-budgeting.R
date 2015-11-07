library(FRAPO)

##Budget constraint
v <- cov(returns)
ERC <- PERC(v)
ERC
w <- Weights(ERC)/100
w * v



