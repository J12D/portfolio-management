source("2-analysis.R")

portfolio_party <- function(model, name) {
  model %>% performance_plot
  model %>% compute_kpis %>% print
  model %>% decompose_plot(name)
  model %>% decompose_relw_plot(paste0(name,"_relw"))
}


## ---- Minimum Variance ---------------
minv <- min_variance(cov = cov_returns(shrink = T, lag_adjustment = 3))
portfolio_party(minv, "minv")


## ---- Maximum Sharpe ---------------
ms <-  max_sharpe(mean = mean_returns(shrink = 0.5),
           cov = cov_returns(lag_adjustment = 3, shrink = T))
ms %>% performance_plot
ms %>% compute_kpis
portfolio_party(ms, "ms")


## ---- Fixed Weights ---------------
fw <- fixed_weights(c(1/3, 1/3, 1/3))
portfolio_party(fw, "equal")


## ---- Fixed allocation, No rebalance ---------------
fix_nr <- evaluate_fix(c(1/3, 1/3, 1/3))
evaluate_fix(c(0.5,0.5,0.5,-0.5), assets_vxx) %>% compute_kpis_fix


## ---- Black Litterman ---------------
bl_w <- max_sharpe_blacklitterman()(returns)
evaluate_fix(w) %>% compute_kpis_fix
#portfolio_party(bl, "black_littie")


w <- max_sharpe_blacklitterman_vxx()(returns_vxx)
bl <- fixed_weights(w)
evaluate_fix(w, assets_vxx) %>% compute_kpis_fix


## ---- Other Optimization ---------------
mvo <- eff_portfolio(mean = mean_returns(shrink = 0.5),
                     cov = cov_returns(shrink = T), no_shorts = T, max.allocation = 0.5)

mva <- eff_portfolio(mean = mean_returns(shrink = 0.5),
                     cov = cov_returns(shrink = T), no_shorts = F, max.allocation = 0.5)


## ---- Equal Risk Contribution ---------------
library(FRAPO)
erc <- equal_risk_contribution(cov = cov_returns(shrink = T, lag_adjustment = 3))
portfolio_party(erc, "erc")
detach("package:FRAPO", unload = TRUE)


## ---- Robust Optimization ---------------
msr_rob <- max_sharpe_robust()
minv_rob <- min_variance_robust()


## ---- Max Sharpe ----------------
max_sharpe() %>% performance_plot(ass = assets_vxx)
eff_portfolio(max.allocation = 0.6) %>% compute_kpis(ass = assets_vxx)


a <- fw %>% evaluate_model %>% drop_last %>% portfolio_return(rf_allocation = list(weight = 0.5, rate = euribor))
b <- fw %>% evaluate_model %>% drop_last %>% portfolio_return(rf_allocation = list(weight = 0, rate = euribor))
c <- fw %>% evaluate_model %>% drop_last %>% portfolio_return(rf_allocation = list(weight = 1, rate = euribor))
merge.xts(a,b,c) %>% plotXTS


## ---- Produce Tables -----------
library(xtable)
a <- list("Minimum Variance" = minv,
          "Fixed" = fw,
          "Blackie" = bl) %>%
  lapply(compute_kpis) %>% do.call(rbind, .)

xtable(a)


minv %>% evaluate_model %>% drop_last %>% portfolio_return %>% .["2015/"]

fw %>% compute_kpis(subset = "2015-06-30/")

comp <- list("Minimum Variance" = minv,
             "Fixed" = fw,
             "Blackie" = bl) %>%
  lapply(function(x)compute_kpis(x, subset = "2015-06-30/")) %>% do.call(rbind, .)
comp


## PIMMEL PORTFOLIOS
#1
minv %>% compute_kpis(rf_allocation = list(weight = 23/34, rate = euribor))
#2
ms %>% compute_kpis(rf_allocation = list(weight = 81/93, rate = euribor))
#3
evaluate_fix(c(1/3, 1/3, 1/3)) %>% compute_kpis_fix
#4
evaluate_fix(c(1/2, 1/2, 1/2, -1/2), ass = assets_vxx) %>% compute_kpis_fix
#5
evaluate_fix(bl_w) %>% compute_kpis_fix
#6
erc %>% compute_kpis(rf_allocation = list(weight = 1/3, rate = euribor))
#7
minv_rob %>% compute_kpis(rf_allocation = list(weight = 16/26, rate = euribor))
#8
msr_rob %>% compute_kpis(rf_allocation = list(weight = 16/26, rate = euribor))
#9
mvo %>% compute_kpis(ass = assets_vxx, rf_allocation = list(weight = 26/34, rate = euribor))
#10
mva %>% compute_kpis(ass = assets_vxx, rf_allocation = list(weight = 78E-2, rate = euribor))
