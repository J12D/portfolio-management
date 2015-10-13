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

# saveRDS(list(GDAXI=GDAXI, DJI=DJI, N225=N225, VXX=VXX, euribor=euribor, factors=factors, fx=fx),"data/assets")

assets <- readRDS("data/assets")

GDAXI <- assets[["GDAXI"]][,"GDAXI.Adjusted"]
DJI <- assets[["DJI"]][,"DJI.Adjusted"]
N225 <- assets[["N225"]][,"N225.Adjusted"]
VXX <- assets[["VXX"]][,"VXX.Adjusted"]
euribor <- assets[["euribor"]]/252
factors <- assets[["factors"]]
fx <- assets[["fx"]]

assets <- merge.xts(GDAXI, DJI, N225, VXX)["2009-01-30/"] %>% na.locf
colnames(assets) <- c("DAX", "Dow Jones", "Nikkei", "VIX")

asset_returns <- assets %>% ROC(type = "discrete")
returns <- asset_returns["2009-02-02/"] %>% na.omit
colnames(returns) <- c("DAX", "Dow Jones", "Nikkei", "VIX")

euribor <- euribor["2009-02-02/"] %>% na.omit

factors <- factors["2009-02-02/"] %>% na.omit

fx <- fx["2009-02-02/"]

rm(list = c("GDAXI", "DJI", "N225", "VXX", "asset_returns"))

message("> Imported data ---------------")

