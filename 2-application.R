source("2-analysis.R")

portfolio_party <- function(model, name) {
  model %>% performance_plot
  model %>% compute_kpis
  model %>% decompose_plot(name)
  model %>% decompose_relw_plot(paste0(name,"_relw"))
}


## ---- Minimum Variance ---------------
minv <- min_variance(cov = cov_returns(shrink = T))
portfolio_party(minv, "minv")


## ---- Maximum Sharpe ---------------
ms <-  max_sharpe(mean = mean_returns(shrink = 0.65),
           cov = cov_returns(shrink = T, lag_adjustment = 3))
ms %>% performance_plot
ms %>% compute_kpis


## ---- Fixed Weights ---------------
fw <- fixed_weights(c(1/3, 1/3, 1/3))
portfolio_party(fw, "equal")


## ---- Black Litterman ---------------
w <- returns %>% (max_sharpe_blacklitterman()) %>% t
bl <- fixed_weights(w)
portfolio_party(bl, "black_littie")

evaluate_fix(w) %>% plotXTS(size = 1)


## ---- Other Optimization ---------------
eff_portfolio(mean = mean_returns(shrink = 0.5),
              cov = cov_returns(shrink = 0.5), T, 3, 0.01, 0.4) %>% performance_plot
eff_portfolio(mean = mean_returns(shrink = 0.5),
              cov = cov_returns(shrink = 0.5), T, 3, 0.01, 0.4) %>% compute_kpis


## ---- Equal Risk Contribution ---------------
library(FRAPO)
erc <- equal_risk_contribution(cov = cov_returns(shrink = 0.5, lag_adjustment = 3))
portfolio_party(erc, "erc")
detach("package:FRAPO", unload = TRUE)


## ---- Fixed allocation, No rebalance ---------------
evaluate_fix(c(0.5,0.5,0.5,-0.5)) %>% plotXTS(size = 1)
evaluate_fix(c(0.5,0.5,0.5,-0.5)) %>% compute_kpis


## ---- Robust Optimization ---------------
max_sharpe_robust() %>% performance_plot
max_sharpe_robust() %>% compute_kpis

min_variance_robust() %>% performance_plot
min_variance_robust() %>% compute_kpis