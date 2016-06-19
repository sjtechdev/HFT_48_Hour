library(caret)
library(Metrics)
library(methods)
library(doMC)
library(pROC)

allTrain <- readRDS("allTrainSell")

trainMat <- data.matrix(allTrain)
trainResponse <- trainMat[, ncol(allTrain)]
trainMat <- trainMat[, -ncol(allTrain)]

set.seed(777)
tr <- trainControl(method='cv',
                   number = 8,
                   verboseIter=TRUE,
                   returnData = FALSE,
                   returnResamp = "final")
gbmGrid <- expand.grid(n.trees = seq(5,500,5), interaction.depth = c(6:9), shrinkage = 0.1, n.minobsinnode = c(10,20))
mlpGrid <- expand.grid(size = c(5,10,15,20,25))
knnGrid <- expand.grid(k = c(5,15,25,50,65, 80, 100, 120, 160))
glmnetGrid <- expand.grid(alpha = seq(0,1,0.05), lambda = c(.000001, .00001, .0001, .0012, 0.0024, 0.0048, .01, .02, .04, .08, .16))
registerDoMC(8)
knnFit <- train(x = trainMat, y = trainResponse,
              method = "knn",
              tuneGrid = knnGrid,
              trControl = tr)

saveRDS(knnFit, file = "knnFitSell")

# fit <- readRDS("glmnetFitSell")

# preds <- predict(gbmFit, newdata = allTrain2[1:30])
# actual <- allTrain2$target
# 
# preds <- ifelse(preds > 0, 1, 0)
# actual <- ifelse(actual > 0, 1, 0)
# 
# roc(preds)
