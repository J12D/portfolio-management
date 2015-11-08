source("2-analysis.R")

portfolio_party <- function(model, name) {
  model %>% performance_plot
  model %>% compute_kpis %>% print
  model %>% decompose_plot(name)
  model %>% decompose_relw_plot(paste0(name,"_relw"))
}


## ---- Minimum Variance ---------------
minv <- min_variance(cov = cov_returns(shrink = T))
portfolio_party(minv, "minv")


## ---- Maximum Sharpe ---------------
ms <-  max_sharpe(mean = mean_returns(shrink = 0.65),
           cov = cov_returns(lag_adjustment = 3, shrink = T))
ms %>% performance_plot
ms %>% compute_kpis
portfolio_party(ms, "ms")


## ---- Fixed Weights ---------------
fw <- fixed_weights(c(1/3, 1/3, 1/3))
portfolio_party(fw, "equal")

## ---- Fixed allocation, No rebalance ---------------
fix_nr <- evaluate_fix(c(1/3, 1/3, 1/3))
evaluate_fix(c(0.5,0.5,0.5,-0.5)) %>% compute_kpis

## ---- Black Litterman ---------------
w <- max_sharpe_blacklitterman()(returns)
bl <- fixed_weights(w)
#portfolio_party(bl, "black_littie")

evaluate_fix(w) %>% plotXTS(size = 1)


## ---- Other Optimization ---------------
eff_portfolio(mean = mean_returns(shrink = 0.65),
              cov = cov_returns(shrink = T), F, 3, 0.01, 0.5) %>% performance_plot

eff_portfolio(mean = mean_returns(shrink = 0.5),
              cov = cov_returns(shrink = T, lag_adjustment = 3), no_shorts = T) %>% performance_plot

eff_portfolio(mean = mean_returns(shrink = 0.5),
              cov = cov_returns(shrink = T, lag_adjustment = 3), no_shorts = T, max.allocation = 0.5) %>% performance_plot

eff_portfolio(mean = mean_returns(shrink = 0.5),
              cov = cov_returns(shrink = T, lag_adjustment = 3), no_shorts = F, max.allocation = 0.5) %>% performance_plot


## ---- Equal Risk Contribution ---------------
library(FRAPO)
erc <- equal_risk_contribution(cov = cov_returns(shrink = 0.5, lag_adjustment = 3))
portfolio_party(erc, "erc")
detach("package:FRAPO", unload = TRUE)


## ---- Robust Optimization ---------------
max_sharpe_robust() %>% performance_plot
max_sharpe_robust() %>% compute_kpis

min_variance_robust() %>% performance_plot
min_variance_robust() %>% compute_kpis

## ---- 
max_sharpe() %>% performance_plot(ass = assets_vxx)
eff_portfolio(max.allocation = 0.6) %>% compute_kpis(ass = assets_vxx)


a <- fw %>% evaluate_model %>% drop_last %>% portfolio_return(rf_allocation = list(weight = 0.5, rate = euribor))
b <- fw %>% evaluate_model %>% drop_last %>% portfolio_return(rf_allocation = list(weight = 0, rate = euribor))
c <- fw %>% evaluate_model %>% drop_last %>% portfolio_return(rf_allocation = list(weight = 1, rate = euribor))
merge.xts(a,b,c) %>% plotXTS
a %>% getMDD
b %>% getMDD
c %>% getMDD
