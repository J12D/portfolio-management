library(magrittr)
library(quantmod)
library(Quandl)
library(lubridate)

Sys.setenv(TZ = "UTC")

# Indices
# getSymbols(c("^GDAXI","^DJI","^N225","VXX"),from="1995-01-01")

# EURIBOR
### no  euribor <- Quandl("ECB/RTD_M_S0_N_C_EUR3M_E")
# euribor <- Quandl("BOF/QS_D_IEUTIO3M")
# euribor <- xts(euribor[,-1]/100,euribor[,1])

# Factors
# factors <- read.csv("data/factors.csv")
# factors$X %<>% parse_date_time("%Y%m%d")
# factors <- xts(factors[,-1]/100,factors[,1])["1995/"]

# saveRDS(list(GDAXI=GDAXI, DJI=DJI, N225=N225, VXX=VXX, euribor=euribor, factors=factors),"data/assets")

assets <- readRDS("data/assets")

GDAXI <- assets[["GDAXI"]][,"GDAXI.Adjusted"]
DJI <- assets[["DJI"]][,"DJI.Adjusted"]
N225 <- assets[["N225"]][,"N225.Adjusted"]
VXX <- assets[["VXX"]][,"VXX.Adjusted"]
euribor <- assets[["euribor"]]/252
factors <- assets[["factors"]]

assets <- merge.xts(GDAXI, DJI, N225, VXX)["2009-01-30/"] %>% na.locf
colnames(assets) <- c("DAX", "Dow Jones", "Nikkei", "VIX")
asset_returns <- assets %>% ROC(type = "discrete")


returns <- merge.xts(asset_returns, Euribor = euribor)["2009-02-02/"] %>% na.omit
factors <- factors["2009-02-02/"] %>% na.omit

rm(list = c("GDAXI", "DJI", "N225", "VXX", "asset_returns", "euribor"))
cat("> Imported data ---------------")