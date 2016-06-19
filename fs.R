library(caret)
library(Metrics)
library(methods)
library(doMC)
library(pROC)

# Get days for training market buy algo
first_buy_day <- readRDS("allTrain_Day_1_Buy")
second_buy_day <- readRDS("allTrain_Day_2_Buy")
third_buy_day <- readRDS("allTrain_Day_4_Buy")

# Get days for training market sell algo
first_sell_day <- readRDS("allTrain_Day_3_Sell")
second_sell_day <- readRDS("allTrain_Day_5_Sell")
third_sell_day <- readRDS("allTrain_Day_6_Sell")

allTrainBuy <- rbind(first_buy_day, second_buy_day, third_buy_day)
allTrainSell <- rbind(first_sell_day, second_sell_day, third_sell_day)

# We are looking at ask when buying w/ market orders
allTrainBuy$target_bid <- NULL

# We are looking at bid when selling w/ market orders
allTrainSell$target_ask <- NULL

# Feature Selection on buy dataset
trainMat <- data.matrix(allTrainBuy)
trainResponse <- trainMat[, ncol(allTrainBuy)]
trainMat <- trainMat[, -ncol(allTrainBuy)]

registerDoMC(4)
control <- rfeControl(functions=treebagFuncs, method="cv", number=4, verbose = TRUE, returnResamp = FALSE, saveDetails = FALSE)
results <- rfe(trainMat, trainResponse, sizes=seq(1,50,1), rfeControl=control)
saveRDS(results, file = "fe_results_buy")
saveRDS(allTrainBuy, file = "allTrainBuy")

# Go easy on the memory
remove(allTrainBuy)
remove(results)
remove(trainMat)
remove(trainResponse)

# Feature Selection on ask dataset
trainMat <- data.matrix(allTrainSell)
trainResponse <- trainMat[, ncol(allTrainSell)]
trainMat <- trainMat[, -ncol(allTrainSell)]

control <- rfeControl(functions=treebagFuncs, method="cv", number=4, verbose = TRUE, returnResamp = FALSE, saveDetails = FALSE)
results <- rfe(trainMat, trainResponse, sizes=seq(1,50,1), rfeControl=control)
saveRDS(results, file = "fe_results_sell")
saveRDS(allTrainSell, file = "allTrainSell")

# featuresCols <- which(names(allTrainBuy) %in% results_buy$optVariables)
# target <- allTrainBuy$target_ask
# allTrainBuy <- allTrainBuy[featuresCols]
# allTrainBuy$target <- target

featuresCols <- which(names(allTrainSell) %in% results_sell$optVariables)
target <- allTrainSell$target_bid
allTrainSell <- allTrainSell[featuresCols]
allTrainSell$target <- target

saveRDS(allTrainSell, "allTrainSell")
saveRDS(allTrainBuy, "allTrainBuy")
