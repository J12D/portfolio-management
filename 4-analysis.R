library(PortfolioAnalytics)

source("0-helper.R")
source("1-data.R")


P <- t(matrix(c(1,0,0,-1, 0,0,1,0, 0,1,0,0, 0,0,0,1), nrow = 4, ncol = 4))
v <- matrix(c(0.1,0.02,0.03,0.3))
Omega <- matrix(c(0.6,0,0,0, 0,0.6,0,0, 0,0,0.6,0, 0,0,0,0.6), nrow = 4, ncol = 4)


#black.litterman(returns, q, Mu=NULL, Sigma =NULL, Views = Pviews)

Mu <- matrix(mean_returns()(returns))
Sigma <- matrix(cov_returns()(returns), nrow = 4, ncol = 4)

black.litterman(returns, P, Mu, Sigma, v)
