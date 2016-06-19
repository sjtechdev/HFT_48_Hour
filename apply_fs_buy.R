library(caret)
library(Metrics)
library(methods)
library(doMC)

buyFiles <- list.files("/Users/Joshi/Dev/ORF 474/Final", pattern="*_Buy", full.names=TRUE)
results_buy_optVar <- readRDS("results_buy_optVar")

for(i in c(1:length(buyFiles)))
{
  currTrain <- readRDS(buyFiles[i])
  featuresCols <- which(names(currTrain) %in% results_buy_optVar)
  target <- currTrain$target_ask
  currTrain <- currTrain[featuresCols]
  currTrain$target <- target
  saveRDS(currTrain, file = paste0(buyFiles[i], "_fefs"))
}


