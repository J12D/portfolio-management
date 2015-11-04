library(PortfolioAnalytics)

source("0-helper.R")
source("1-data.R")


q <- t(matrix(c(1,0,0,-1, 0,0,1,0, 0,1,0,0, 0,0,0,1), nrow = 4, ncol = 4))
Pviews <- t(c(0.1,0.02,0.03,0.3))


black.litterman(returns, q, Mu=NULL, Sigma =NULL, Views = Pviews)