library(magrittr)
library(quantmod)
library(Quandl)
library(lubridate)

Sys.setenv(TZ = "UTC")

# Indices
# getSymbols(c("^GDAXI","^DJI","^N225","VXX"))

# EURIBOR
### no  euribor <- Quandl("ECB/RTD_M_S0_N_C_EUR3M_E")
# euribor <- Quandl("BOF/QS_D_IEUTIO3M")
# euribor <- xts(euribor[,-1]/100,euribor[,1])

# Factors
# factors <- read.csv("data/factors.csv")
# factors$X %<>% parse_date_time("%Y%m%d")
# factors <- xts(factors[,-1],factors[,1])["1995/"]

# saveRDS(list(GDAXI=GDAXI, DJI=DJI, N225=N225, VXX=VXX, euribor=euribor, factors=factors),"data/assets")

assets <- readRDS("data/assets")

GDAXI <- assets[["GDAXI"]][,"GDAXI.Adjusted"]
DJI <- assets[["DJI"]][,"DJI.Adjusted"]
N225 <- assets[["N225"]][,"N225.Adjusted"]
VXX <- assets[["VXX"]][,"VXX.Adjusted"]
euribor <- assets[["euribor"]]
factors <- assets[["factors"]]/100

assets <- merge.xts(GDAXI, DJI, N225, VXX)["2009-01-30/"] %>% na.locf
colnames(assets) <- c("GDAXI", "DJI", "N225", "VXX")
asset_returns <- assets %>% ROC(type = "discrete")

returns <- merge.xts(asset_returns,euribor)["2009-02-02/"] %>% na.omit
factors <- factors["2009-02-02/"] %>% na.omit

rm(list = c("GDAXI", "DJI", "N225", "VXX", "asset_returns", "euribor"))
cat("> Imported data ---------------")