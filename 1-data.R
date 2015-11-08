library(magrittr)
library(quantmod)
library(Quandl)
library(readxl)
library(lubridate)

#source("1-save_data.R")

assets <- readRDS("data/assets")

fx <- assets[["fx"]]
GDAXI <- assets[["GDAXI"]][,"GDAXI.Adjusted"]
DJI <- assets[["DJI"]][,"DJI.Adjusted"]/fx$EUR.USD
N225 <- assets[["N225"]][,"N225.Adjusted"]/fx$EUR.JPY
VXX <- assets[["VXX"]][,"VXX.Adjusted"]
euribor <- assets[["euribor"]]/252
factors <- assets[["factors"]]
us_factors <- assets[["us_factors"]]
eu_factors <- assets[["eu_factors"]]
jp_factors <- assets[["jp_factors"]]

assets <- merge.xts(GDAXI, DJI, N225)["2009-01-30/"] %>% na.locf
colnames(assets) <- c("DAX", "Dow Jones", "Nikkei")

assets_vxx <- merge.xts(assets,VXX) %>% na.omit

asset_returns <- assets %>% ROC(type = "discrete")
returns <- asset_returns["2009-02-02/"] %>% na.omit
colnames(returns) <- c("DAX", "Dow Jones", "Nikkei")

euribor <- euribor["2009-02-02/"] %>% na.omit
#returns <- merge.xts(returns,euribor) %>% na.omit

#assets <- merge.xts(assets, euribor = cumprod(1 + euribor)) %>% na.omit
#assets[1,"euribor"] <- 1

assets %<>% na.omit

factors <- factors["2009-02-02/"] %>% na.omit

fx <- fx["2009-02-02/"]

rm(list = c("GDAXI", "DJI", "N225", "VXX", "asset_returns"))

message("> Imported data ---------------")

##########################################
# apply(assets,2,function(x)x/drop(coredata(x[1]))*100) %>% as.xts %>% plotTable("assets")