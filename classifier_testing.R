library(caret)
library(Metrics)
library(methods)
library(doMC)
library(pROC)

# Get days for training market buy algo
first_buy_val_day <- readRDS("allTrain_Day_9_Buy_fefs")
second_buy_val_day <- readRDS("allTrain_Day_11_Buy_fefs")
third_buy_val_day <- readRDS("allTrain_Day_12_Buy_fefs")
fourth_buy_val_day <- readRDS("allTrain_Day_13_Buy_fefs")
fifth_buy_val_day <- readRDS("allTrain_Day_20_Buy_fefs")

# Get days for training market sell algo
first_sell_val_day <- readRDS("allTrain_Day_7_Sell_fefs")
second_sell_val_day <- readRDS("allTrain_Day_8_Sell_fefs")
third_sell_val_day <- readRDS("allTrain_Day_10_Sell_fefs")
fourth_sell_val_day <- readRDS("allTrain_Day_14_Sell_fefs")
fifth_sell_val_day <- readRDS("allTrain_Day_15_Sell_fefs")

gbmFitBuy <- readRDS("gbmFitBuy")
glmnetFitBuy <- readRDS("glmnetFitBuy")
knnFitBuy <- readRDS("knnFitBuy")
mlpFitBuy <- readRDS("mlpFitBuy")

gbmFitSell <- readRDS("gbmFitSell")
glmnetFitSell <- readRDS("glmnetFitSell")
knnFitSell <- readRDS("knnFitSell")
mlpFitSell <- readRDS("mlpFitSell")


par(mfrow=c(2,2))

allpreds <- c()
allactual <- c()
all_rmse <- c()

preds <- predict(glmnetFitSell, newdata = first_sell_val_day[1:(ncol(first_sell_val_day)-1)])
# plot(preds, first_sell_val_day$target, main = "GBM")
cor(preds, first_sell_val_day$target)
rmse(preds, first_sell_val_day$target)
all_rmse <- c(all_rmse, rmse(preds, first_sell_val_day$target))
allpreds <- c(allpreds, preds)
allactual <- c(allactual, first_sell_val_day$target)

preds <- predict(glmnetFitSell, newdata = second_sell_val_day[1:(ncol(second_sell_val_day)-1)])
# plot(preds, second_sell_val_day$target, main = "GBM")
cor(preds, second_sell_val_day$target)
rmse(preds, second_sell_val_day$target)
all_rmse <- c(all_rmse, rmse(preds, second_sell_val_day$target))
allpreds <- c(allpreds, preds)
allactual <- c(allactual, second_sell_val_day$target)

preds <- predict(glmnetFitSell, newdata = third_sell_val_day[1:(ncol(third_sell_val_day)-1)])
plot(preds, third_sell_val_day$target, main = "GLMNET", xlim = c(-1,1), ylim = c(-1,1))
cor(preds, third_sell_val_day$target)
rmse(preds, third_sell_val_day$target)
all_rmse <- c(all_rmse, rmse(preds, third_sell_val_day$target))
allpreds <- c(allpreds, preds)
allactual <- c(allactual, third_sell_val_day$target)

preds <- predict(glmnetFitSell, newdata = fourth_sell_val_day[1:(ncol(fourth_sell_val_day)-1)])
# plot(preds, fourth_sell_val_day$target, main = "GBM")
cor(preds, fourth_sell_val_day$target)
rmse(preds, fourth_sell_val_day$target)
all_rmse <- c(all_rmse, rmse(preds, fourth_sell_val_day$target))
allpreds <- c(allpreds, preds)
allactual <- c(allactual, fourth_sell_val_day$target)

preds <- predict(glmnetFitSell, newdata = fifth_sell_val_day[1:(ncol(fifth_sell_val_day)-1)])
# plot(preds, fifth_sell_val_day$target, main = "GBM")
cor(preds, fifth_sell_val_day$target)
rmse(preds, fifth_sell_val_day$target)
all_rmse <- c(all_rmse, rmse(preds, fifth_sell_val_day$target))
allpreds <- c(allpreds, preds)
allactual <- c(allactual, fifth_sell_val_day$target)

mean(all_rmse)



















preds <- predict(knnFitBuy, newdata = second_buy_val_day[1:(ncol(second_buy_val_day)-1)])
# plot(preds, second_buy_val_day$target, main = "KNN")
cor(preds, second_buy_val_day$target)
rmse(preds, second_buy_val_day$target)

preds <- predict(glmnetFitBuy, newdata = second_buy_val_day[1:(ncol(second_buy_val_day)-1)])
# plot(preds, second_buy_val_day$target, main = "GLMNET")
cor(preds, second_buy_val_day$target)
rmse(preds, second_buy_val_day$target)

preds <- predict(mlpFitBuy, newdata = second_buy_val_day[1:(ncol(second_buy_val_day)-1)])
# plot(preds, second_buy_val_day$target, main = "MLP")
cor(preds, second_buy_val_day$target)
rmse(preds, second_buy_val_day$target)
