library(binhf)
library(parallel)
library(foreach)
library(doMC)
library(pbapply)

load("~/Dev/ORF 474/hw2/combined/amzn/20150301_AMZN.RData")
Trades <- Trades[,-c(9:13)] # Removes any junk info we don't need
allDays <- unique(Trades$Date)

# Estimate # shares traded per trade
lots_per_trade_per_day <- unlist(lapply(c(1:length(allDays)), 
       function(x) sum(Trades[which(Trades$Date == allDays[x]),]$Shares)/(nrow(Trades[which(Trades$Date == allDays[x]),])*100)
))

mean_lots_per_trade <- mean(lots_per_trade_per_day)
sd_lots_per_trade_per_day <- sd(lots_per_trade_per_day)

for(k in c(1:42))
{
print(paste0("On day = ",k))
miniTrades_1 <- Trades[which(Trades$Date == allDays[k]),]
miniQuotes_1 <- Quotes[which(Quotes$Date == allDays[k]),]

rownames(miniTrades_1) <- c(1:nrow(miniTrades_1))
rownames(miniQuotes_1) <- c(1:nrow(miniQuotes_1))

# SharesTraded <- miniTrades_1$Shares
# PriceTraded <- miniTrades_1$Price
# 
# PriceXV <- cumsum(SharesTraded*PriceTraded)
# V_Amount <- cumsum(SharesTraded)
# VWAP_M <- PriceXV/V_Amount
# 
# VWAP_ObservedSoFar <- rep(0, length(VWAP_M))
# VWAP_ObservedSoFar[2:length(VWAP_ObservedSoFar)] <- VWAP_M[1:(length(VWAP_M) - 1)]
# VWAP_Final <- VWAP_M[length(VWAP_M)]
# 
# OFI_ObservedSoFar <- rep(0, length(VWAP_M))

#miniTrades_1$VWAP_ObservedSoFar <- VWAP_ObservedSoFar

# Create Training Set
miniTrades_1$Date <- NULL
miniTrades_1$Exchange <- NULL
miniTrades_1$Sym <- NULL
miniTrades_1$SYM_SUFFIX <- NULL
miniTrades_1$Cond <- NULL
miniTrades_1$QU_COND <- NULL
time <- miniTrades_1$Time
miniTrades_1$Time <- NULL
miniTrades_1$Shares <- miniTrades_1$Shares/100
miniTrades_1$Spread <- miniTrades_1$Ask - miniTrades_1$Bid


df_toMakeTrain <- miniTrades_1
df_toMakeTrain_ask_prices <- miniTrades_1$Ask
df_toMakeTrain_bid_prices <- miniTrades_1$Bid

lookBack <- 15
lookAhead <- 15

getNewRow <- function(x, lookBack, lookAhead, df, ask_prices, bid_prices)
{
  sub_df <- df[x:(x+lookBack-1),]
  
  sub_df[,9] <- sub_df[,2] - sub_df[,3] # Price - Bid
  sub_df[,10] <- sub_df[,2] - sub_df[,5] # Price - Ask
  sub_df[,11] <- sub_df[,6] - sub_df[,4] # AskSize - BidSize

  priceVol <- sd(sub_df[,2])
  bidVol <- sd(sub_df[,3])
  askVol <- sd(sub_df[,5])
  signVol <- sd(sub_df[,7])
  shareVol <- sd(sub_df[,1])
  
  numTimesGEqAsk <- length(which(sub_df[,2] >= sub_df[,5])) # Count of number times trade price >= ask
  numTimesLEqBid <- length(which(sub_df[,2] <= sub_df[,3])) # Count of number times trade price <= bid
  numTimesInside <- length(which(sub_df[,2] > sub_df[,3] & sub_df[,2] < sub_df[,5])) # Count of number times trade price between ask and bid
  
  imm_price_change <- diff(sub_df[,2])
  imm_ask_change <- diff(sub_df[,5])
  imm_bid_change <- diff(sub_df[,3])
  
  crude_price_impact <- imm_price_change*sub_df[1:(nrow(sub_df) - 1),1]*sub_df[1:(nrow(sub_df) - 1),7]
  
  sub_df[,2] <- sub_df[,2] - sub_df[1,2] # Change In Price since first sample
  sub_df[,3] <- sub_df[,3] - sub_df[1,3] # Change In Bid since first sample
  sub_df[,5] <- sub_df[,5] - sub_df[1,5] # Change In Ask since first sample
  
  sub_df[,12] <- c(0, imm_price_change)
  sub_df[,13] <- c(0, imm_ask_change)
  sub_df[,14] <- c(0, imm_bid_change)
  sub_df[,15] <- c(0, crude_price_impact)
  
  m_mat <- as.matrix(sub_df)
  curr_row <- c(m_mat)
  target_ask <- ask_prices[x+lookBack+lookAhead-1] - ask_prices[x+lookBack-1]
  target_bid <- bid_prices[x+lookBack+lookAhead-1] - bid_prices[x+lookBack-1]
  curr_row <- c(curr_row, priceVol, bidVol, askVol, signVol, shareVol, 
                numTimesGEqAsk, numTimesLEqBid, numTimesInside, target_ask, target_bid)
  
  return(curr_row)
}

df_names <- names(df_toMakeTrain)
res <- mclapply(c(1:(nrow(df_toMakeTrain) - (lookBack + lookAhead) + 1)),
              function(x) getNewRow(x, lookBack, lookAhead, df_toMakeTrain, df_toMakeTrain_ask_prices, df_toMakeTrain_bid_prices), mc.cores = 6)

allTrain <- do.call(rbind.data.frame, res)

longNamesOfallTrain <- c("Shares", "DeltaPrice", "DeltaBid", "BidSize", "DeltaAsk", "AskSize", "Sign", "Spread",
                     "Price_Bid_Diff", "Price_Ask_Diff", "AskSize_BidSize_Diff", 
                     "Immed_Price_Change", "Immed_Bid_Change", "Immed_Ask_Change","Crude_Price_Impact")

shortNamesOfallTrain <- c("priceVol", "bidVol", "askVol", "signVol", "shareVol", 
                          "numTimesGEqAsk", "numTimesLEqBid", "numTimesInside")
                     
new_name_vec <- c()
for(i in longNamesOfallTrain)
{
  new_name_vec <- c(new_name_vec, paste0(i, c(lookBack:1)))
}

new_name_vec <- c(new_name_vec, shortNamesOfallTrain, "target_ask", "target_bid")
colnames(allTrain) <- new_name_vec

# load(file = "results.RData")
# featuresCols <- which(names(allTrain) %in% results$optVariables)
# target <- allTrain$target
# allTrain <- allTrain[featuresCols]
# allTrain$target <- target
fname <- c()

if(sum(miniTrades_1$Shares*miniTrades_1$Sign) > 0)
{
  fname <- paste0("allTrain", "_Day_",k, "_Buy")
} else {
  fname <- paste0("allTrain", "_Day_",k, "_Sell")
}

saveRDS(allTrain, file = fname)
}