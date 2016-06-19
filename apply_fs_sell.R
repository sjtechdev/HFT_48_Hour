library(caret)
library(Metrics)
library(methods)
library(doMC)

sellFiles <- list.files("/Users/Joshi/Dev/ORF 474/Final", pattern="*_Sell", full.names=TRUE)
results_sell_optVar <- readRDS("results_sell_optVar")

for(i in c(1:length(sellFiles)))
{
  currTrain <- readRDS(sellFiles[i])
  featuresCols <- which(names(currTrain) %in% results_sell_optVar)
  target <- currTrain$target_bid
  currTrain <- currTrain[featuresCols]
  currTrain$target <- target
  saveRDS(currTrain, file = paste0(sellFiles[i], "_fefs"))
}