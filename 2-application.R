source("2-analysis.R")

list(
  model = minv, #{returns -> weights}
  rf_allocation = list(weight = 16/26, rate = euribor), #{list(weight, rate), NULL}
  assets = assets, # xts with assets in columns
  period = "months", # {"months", "years", "fix"}
  time = "2012-12-30/2015-06-30"
)

run_job <- function(job) {
  model <- job[["model"]]
  assets <- job[["assets"]]
  period <- job[["period"]]
  rf_allocation <- job[["rf_allocation"]]
  subset <- job[["time"]]
  stopifnot(period %in% c("months", "years", "fix"))
  
  if (period == "fix") {
    weights <- model(assets[subset] %>% ROC(type = "discrete"))
    values <- weights %>% evaluate_fix_components(ass = assets, subset = subset)
  }
  else {
    weights <- model %>% evaluate_model(ass = assets, period = period, subset = subset)
    values <- weights %>% drop_last %>% portfolio_return(ass = assets, period = period, subset = subset)
  }
  
  value <- values %>% rowSums.xts
  list(weights = weights, values = values, value = value, job = job)
}

with_model <- function(job, model) {
  job[["model"]] <- model
  job
}

with_allocation <- function(job, euribor_allocation) {
  job[["rf_allocation"]] <- list(weight = euribor_allocation, rate = euribor)
  job
}

on_assets <- function(job, assets) {
  job[["assets"]] <- assets
  job
}

plot_weights <- function(job_result) {
  job_result[["weights"]] %>% plotXTS
}

plot_values <- function(job_result) {
  job_result[["values"]] %>% plotXTS
}

plot_value <- function(job_result) {
  job_result[["value"]] %>% plotXTS
}

base <- list(model = NULL, rf_allocation = NULL, assets = assets, period = "months", time = "2012-12-30/2015-06-30")

annual_base <- as.list(base)
annual_base["period"] <- "years"

fix_base <- as.list(base)
fix_base["period"] <- "fix"

run_job(list(model = minv, rf_allocation = NULL, assets = assets, period = "years", time = "2012-12-30/2015-06-30"))

portfolio_party <- function(job_result, name) {
  job_result[["values"]] %T>%
    plotTable(name) %>%
    base_100 %>%
    plotTable(paste0(name,"_relw"))
}


## ---- 1-Minimum Variance ---------------
minv <- min_variance(cov = cov_returns(shrink = T, lag_adjustment = 3))
base %>% with_model(minv) %>% with_allocation(13/24) %>% run_job %>% portfolio_party("minv")

## ---- 2-Maximum Sharpe ---------------
ms <-  max_sharpe(mean = mean_returns(shrink = 0.5),
           cov = cov_returns(lag_adjustment = 3, shrink = T))
base %>% with_model(ms) %>% run_job %>% portfolio_party("ms")

## ---- 3-Fixed Weights ---------------
fw <- fixed_weights(c(1/3, 1/3, 1/3))
fix_base %>% with_model(fw) %>% run_job %>% portfolio_party("equal")

## ---- 4-Fixed Weights Leveraged ------
fw_lev <- fixed_weights(c(1/2, 1/2, 1/2, -1/2))
fix_base %>% with_model(fw_lev) %>% on_assets(assets_vxx) %>% run_job %>% portfolio_party("equal")

## ---- 5-Black Litterman ---------------
bl_w <- max_sharpe_blacklitterman()(returns)
fix_base %>% with_model(bl_w) %>% run_job %>% portfolio_party("bl")

## ---- 6-Equal Risk Contribution ---------------
library(FRAPO)

erc <- equal_risk_contribution(cov = cov_returns(shrink = T, lag_adjustment = 3))
base %>% with_model(erc) %>% run_job %>% portfolio_party("erc")
detach("package:FRAPO", unload = TRUE)

## ---- 7-Robust MinVP ---------------
minv_rob <- min_variance_robust()
base %>% with_model(minv_rob) %>% run_job %>% portfolio_party("minv_rob")

## ---- 8-Robust MVP ---------------
msr_rob <- max_sharpe_robust()
base %>% with_model(minv_rob) %>% run_job %>% portfolio_party("msr_rob")

## ---- 9-MVP Max Allocation ---------------
mv_max_alloc <- eff_portfolio(mean = mean_returns(shrink = 0.5),
                     cov = cov_returns(shrink = T), no_shorts = T, max.allocation = 0.5)

base %>% with_model(mv_max_alloc) %>% run_job %>% portfolio_party("mvo")

## ---- 10-MVP Max Allocation No Short ---------------
mv_max_alloc_no_short <- eff_portfolio(mean = mean_returns(shrink = 0.5),
                     cov = cov_returns(shrink = T), no_shorts = F, max.allocation = 0.5)

base %>% with_model(mv_max_alloc_no_short) %>% run_job %>% portfolio_party("mva")


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
             "Maximum Sharpe" = ms,
             "Equal Risk" = erc,
             "Min Variance Robust" = minv_rob,
             "Maximum Sharpe Robust" = msr_rob,
             "Mean Variance Maximum Allocation" = mva,
             "Mean Variance Maximum Allocation & Short-Sales" = mvo) %>%
  lapply(function(x)compute_kpis(x, subset = "2013/2015-06-30")) %>% do.call(rbind, .)
comp


# ## PORTFOLIOS
# #1
# c1 <- a <- minv %>% compute_kpis(rf_allocation = list(weight = 23/34, rate = euribor))
# minv %>% portfolio_party("minv", rf_allocation = list(weight = 23/34, rate = euribor))
# #2
# c2 <- ms %>% compute_kpis(rf_allocation = list(weight = 81/93, rate = euribor))
# ms %>% portfolio_party("ms", rf_allocation = list(weight = 81/93, rate = euribor))
# #3
# c3 <- evaluate_fix(c(1/3, 1/3, 1/3)) %>% compute_kpis_fix
# rep(1/3,3) %>% evaluate_fix_components %>% plotTable("fw")
# #4
# c4 <- evaluate_fix(c(1/2, 1/2, 1/2, -1/2), ass = assets_vxx) %>% compute_kpis_fix
# c(1/2, 1/2, 1/2, -1/2) %>% evaluate_fix_components(ass = assets_vxx) %>% plotTable("fw_lev")
# #5
# c5 <- evaluate_fix(bl_w) %>% compute_kpis_fix
# bl_w %>% evaluate_fix_components %>% plotTable("bl")
# #6
# c6 <- erc %>% compute_kpis(rf_allocation = list(weight = 1/3, rate = euribor))
# erc %>% portfolio_party("erc", rf_allocation = list(weight = 1/3, rate = euribor))
# #7
# c7 <- minv_rob %>% compute_kpis(rf_allocation = list(weight = 16/26, rate = euribor))
# minv_rob %>% portfolio_party("minv_rob", rf_allocation = list(weight = 16/26, rate = euribor))
# #8
# c8 <- msr_rob %>% compute_kpis(rf_allocation = list(weight = 16/26, rate = euribor))
# msr_rob %>% portfolio_party("msr_rob", rf_allocation = list(weight = 16/26, rate = euribor))
# #9
# c9 <- mvo %>% compute_kpis(ass = assets_vxx, rf_allocation = list(weight = 26/34, rate = euribor))
# mvo %>% portfolio_party("mvo", rf_allocation = list(weight = 16/26, rate = euribor), ass = assets_vxx)
# #10
# c10 <- mva %>% compute_kpis(ass = assets_vxx, rf_allocation = list(weight = 78E-2, rate = euribor))
# mva %>% portfolio_party("mva", rf_allocation = list(weight = 78E-2, rate = euribor), ass = assets_vxx)