library(binhf)
library(parallel)
library(foreach)
library(doMC)
library(pbapply)
library(caret)
library(Metrics)
library(methods)
library(plyr)

load("~/Dev/ORF 474/hw2/combined/amzn/20150301_AMZN.RData")
Trades <- Trades[,-c(9:13)] # Removes any junk info we don't need
allDays <- unique(Trades$Date)

day = 9
# glmnetFitBuy <- readRDS("glmnetFitBuy")
# 
# for(day in c(21, 24, 31, 35, 39, 40, 42))
# {
#   first_buy_val_day <- readRDS(paste0("allTrain_Day_", day, "_Buy_fefs"))
#   
#   preds <- predict(glmnetFitBuy, newdata = first_buy_val_day[1:(ncol(first_buy_val_day) - 1)])
#   
#   miniTrades_1 <- Trades[which(Trades$Date == allDays[day]),]
#   preds_for_trade_data <- rep(0, nrow(miniTrades_1))
#   preds_for_trade_data[15:(length(preds) - 1 + 15)] <- preds
#   
#   saveRDS(preds_for_trade_data, file = paste0("preds_buy_", day))
# }

glmnetFitSell <- readRDS("glmnetFitSell")

for(day in c(16, 17, 18, 19, 22, 23, 25))
{
  first_sell_val_day <- readRDS(paste0("allTrain_Day_", day, "_Sell_fefs"))
  
  preds <- predict(glmnetFitSell, newdata = first_sell_val_day[1:(ncol(first_sell_val_day) - 1)])
  
  miniTrades_1 <- Trades[which(Trades$Date == allDays[day]),]
  preds_for_trade_data <- rep(0, nrow(miniTrades_1))
  preds_for_trade_data[15:(length(preds) - 1 + 15)] <- preds
  
  saveRDS(preds_for_trade_data, file = paste0("preds_sell_", day))
}

