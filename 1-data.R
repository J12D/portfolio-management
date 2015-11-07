library(magrittr)
library(quantmod)
library(Quandl)
library(readxl)
library(lubridate)

Sys.setenv(TZ = "UTC")

# -- Indices -------------------
# getSymbols(c("^GDAXI","^DJI","^N225","VXX"),from="1995-01-01")

# -- EURIBOR -------------------
### no  euribor <- Quandl("ECB/RTD_M_S0_N_C_EUR3M_E")
# euribor <- Quandl("BOF/QS_D_IEUTIO3M")
# euribor <- xts(euribor[,-1]/100,euribor[,1])

# -- Factors -------------------
# factors <- read.csv("data/factors.csv")
# factors$X %<>% parse_date_time("%Y%m%d")
# factors <- xts(factors[,-1]/100,factors[,1])["1995/"]

read_factors <- function(filename) {
  f <- read.csv(filename)
  f[f == -99.99] = NA
  f$X %<>% parse_date_time("%Y%m")
  f <- xts(f[,-1]/100, f[,1])
}

us_factors <- read_factors("data/North_America_Factors_1.csv")
eu_factors <- read_factors("data/Europe_Factors_1.csv")
jp_factors <- read_factors("data/Japan_Factors_1.csv")

# -- FX -------------------
# eurusd_0 <- getFX("EUR/USD", from = "2008-01-01", to = "2011-01-01", auto.assign = F)
# eurusd_1 <- getFX("EUR/USD", from = "2011-01-02", auto.assign = F)
# eurusd <- rbind(eurusd_0, eurusd_1)
# 
# eurjpy_0 <- getFX("EUR/JPY", from = "2008-01-01", to = "2011-01-01", auto.assign = F)
# eurjpy_1 <- getFX("EUR/JPY", from = "2011-01-02", auto.assign = F)
# eurjpy <- rbind(eurjpy_0, eurjpy_1)
# 
# fx <- merge.xts(eurusd, eurjpy)

# saveRDS(list(GDAXI=GDAXI, DJI=DJI, N225=N225, VXX=VXX, euribor=euribor, factors=factors, fx=fx, us_factors=us_factors, eu_factors=eu_factors, jp_factors=jp_factors),"data/assets")

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

asset_returns <- assets %>% ROC(type = "discrete")
returns <- asset_returns["2009-02-02/"] %>% na.omit
colnames(returns) <- c("DAX", "Dow Jones", "Nikkei")

euribor <- euribor["2009-02-02/"] %>% na.omit
returns <- merge.xts(returns,euribor) %>% na.omit

assets <- merge.xts(assets, euribor = cumprod(1 + euribor)) %>% na.omit
assets[1,"euribor"] <- 1

factors <- factors["2009-02-02/"] %>% na.omit

fx <- fx["2009-02-02/"]

rm(list = c("GDAXI", "DJI", "N225", "VXX", "asset_returns"))

message("> Imported data ---------------")

#apply(assets,2,function(x)x/drop(coredata(x[1]))*100) %>% as.xts %>% plotTable("assets")
