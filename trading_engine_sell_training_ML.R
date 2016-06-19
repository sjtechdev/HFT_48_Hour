# We will run throughout Days 16, 17, 18, 19
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
list_of_preds <- list()
list_of_preds[[16]] = readRDS(paste0("preds_sell_", 16))
list_of_preds[[17]] = readRDS(paste0("preds_sell_", 17))
list_of_preds[[18]] = readRDS(paste0("preds_sell_", 18))
list_of_preds[[19]] = readRDS(paste0("preds_sell_", 19))

# INITIATE
# ML Algo trades if pred is negative enough
MLAlgo <- function(thresh, predVal, numSharesDesired)
{
  if(predVal <= thresh)
  {
    return(numSharesDesired)
  } else {
    return(-1)
  }
}

objToOptimize <- function(params)
{
  allFinalVWAP_ME <- c()
  allFinalVWAP_Market <- c()
  allFinalObjectives <- c()
  allFinalPovRates <- c()
  
  runSimulation <- function(tradingDay, params)
  {
    miniTrades_1 <- Trades[which(Trades$Date == allDays[tradingDay]),]
    miniTrades_1$Exchange <- NULL
    miniTrades_1$Sym <- NULL
    miniTrades_1$SYM_SUFFIX <- NULL
    miniTrades_1$Cond <- NULL
    miniTrades_1$QU_COND <- NULL
    preds <- list_of_preds[[tradingDay]]
    
    currMarketVWAP <- 0
    currMarketVWAP_top <- 0
    currMarketVWAP_bottom <- 0
    
    currMyVWAP <- 0
    currMyVWAP_top <- 0
    currMyVWAP_bottom <- 0
    
    currPOV <- 0
    totalSellVolObs <- 0
    currObjective <- 0
    currentState <- 0
    numSharesToTrade <- 0
    
    dfOfMyTransactions <- data.frame(matrix(ncol = 2))
    colnames(dfOfMyTransactions) <- c("Price", "Volume")
    dfOfMyTransactions_counter = 1;
    max_drift_over_participating = params[1];
    max_drift_under_participating = params[2];
    ML_param = params[3];
    nSharesD = params[4];
    
    for (i in c(1:nrow(miniTrades_1))) {
      currentRow <- miniTrades_1[i,]
      currentTime <- miniTrades_1[i,2]
      currentShares <- miniTrades_1[i,3]
      currentPrice <- miniTrades_1[i,4]
      currentBid <- miniTrades_1[i,5]
      currentSign <- miniTrades_1[i,9]
      currPred <- preds[i]
      
      if(currentState == 1)
      {
        dfOfMyTransactions[dfOfMyTransactions_counter,1] = currentBid
        dfOfMyTransactions[dfOfMyTransactions_counter,2] = numSharesToTrade
        dfOfMyTransactions_counter = dfOfMyTransactions_counter + 1
        currMyVWAP_top = currMyVWAP_top + currentBid*numSharesToTrade
        currMyVWAP_bottom = currMyVWAP_bottom + numSharesToTrade
        currMyVWAP = currMyVWAP_top/currMyVWAP_bottom
        currPOV = currPOV + numSharesToTrade
        currentState = 0;
      }
      
      currMarketVWAP_top = currMarketVWAP_top + currentPrice*currentShares
      currMarketVWAP_bottom = currMarketVWAP_bottom + currentShares
      currMarketVWAP = currMarketVWAP_top/currMarketVWAP_bottom
      currObjective = min(currMyVWAP - currMarketVWAP, 0)
      
      if (currentSign == -1)
      {
        totalSellVolObs = totalSellVolObs + currentShares
      }
      
      # Check if over-participating
      # If over-participating, disallow trades
      if (currPOV - .1*totalSellVolObs < 100*max_drift_over_participating)
      {
        result = MLAlgo(ML_param, currPred, nSharesD)
        if (result > 0)
        {
          currentState = 1
          numSharesToTrade = result
        } else {
          currentState = 0
        }
      }
      
      if (.1*totalSellVolObs - currPOV > 100*max_drift_under_participating){
        currentState = 1
        # print("BREAKER EXECUTED")
        numSharesToTrade = 500
      }
    }
    
    # Final Reconcilliation if overbought or oversold
    if(currPOV > .1*totalSellVolObs)
    {
      dfOfMyTransactions[dfOfMyTransactions_counter,1] = miniTrades_1[i,7]
      dfOfMyTransactions[dfOfMyTransactions_counter,2] = .1*totalSellVolObs - currPOV
      currMyVWAP_top = currMyVWAP_top + miniTrades_1[i,7]*(currPOV - .1*totalSellVolObs)
      currMyVWAP_bottom = currMyVWAP_bottom + (currPOV - .1*totalSellVolObs)
      currMyVWAP = currMyVWAP_top/currMyVWAP_bottom
      currPOV = currPOV - (currPOV - .1*totalSellVolObs)
      currObjective = min(currMyVWAP - currMarketVWAP, 0)
    } else if (currPOV < .1*totalSellVolObs) {
      dfOfMyTransactions[dfOfMyTransactions_counter,1] = currentBid
      dfOfMyTransactions[dfOfMyTransactions_counter,2] = .1*totalSellVolObs - currPOV
      currMyVWAP_top = currMyVWAP_top + currentBid*(.1*totalSellVolObs - currPOV)
      currMyVWAP_bottom = currMyVWAP_bottom + (.1*totalSellVolObs - currPOV)
      currMyVWAP = currMyVWAP_top/currMyVWAP_bottom
      currPOV = currPOV + (.1*totalSellVolObs - currPOV)
      currObjective = min(currMyVWAP - currMarketVWAP, 0)
    }
    
    # print(paste0("Our Final VWAP: ", currMyVWAP))
    # print(paste0("Market's Final VWAP: ", currMarketVWAP))
    # print(paste0("Objective Function: ", currObjective))
    # print(paste0("Our Final POV rate: ", currPOV/totalSellVolObs))
    
    return(c(currMyVWAP, currMarketVWAP, currObjective, currPOV/totalSellVolObs))
  }
  
  listOfDays <- c(16, 17, 18, 19)
  for(d in listOfDays)
  {
    # print(paste0("On day ", d))
    res <- runSimulation(d, params)
    allFinalVWAP_ME <- c(allFinalVWAP_ME, res[[1]])
    allFinalVWAP_Market <- c(allFinalVWAP_Market, res[[2]])
    allFinalObjectives <- c(allFinalObjectives, res[[3]])
    allFinalPovRates <- c(allFinalPovRates, res[[4]])
  }
  
  # print("Results of Simulation: ")
  # print(paste0("Our Final Simulation Average VWAP: ", mean(allFinalVWAP_ME)))
  # print(paste0("Market's Final Simulation Average VWAP: ", mean(allFinalVWAP_Market)))
  print(paste0("Simulation Average Objective Function: ", mean(allFinalObjectives)))
  return(mean(allFinalObjectives))
}

paramGrid <- expand.grid(c(10), c(10), seq(0,-0.2,-0.04), c(200, 300, 400, 500))

registerDoMC(8)
optimizerResults <- foreach(listPos=c(1:nrow(paramGrid))) %dopar%
{
  return(c(objToOptimize(paramGrid[listPos,]), unlist(paramGrid[listPos,])))
}

saveRDS(optimizerResults, file = "optimizerResults_Sell_ML_Algo")

###################################################################
optimizerResults <- readRDS(file = "optimizerResults_Sell_ML_Algo")
optimizerResults_df <- do.call(rbind.data.frame, optimizerResults)
colnames(optimizerResults_df) <- c("Objective", "max_drift_over_participating", "max_drift_under_participating", "pred_thresh", "numShares")
optimizerResults_df <- optimizerResults_df[order(-optimizerResults_df$Objective),]
