
install.packages("xgboost")
library(xgboost)
XGBoostTrain <- function(data4execution, idName, targetName, numCores, num_params_optimization, KPI, seed){
  
  varsTrain <- !names(data4execution$TRAIN) %in% c(idName, targetName)
  
  Y_train <- data4execution$TRAIN[, targetName]
  X_train <- data4execution$TRAIN[, varsTrain]
  Y_test <- data4execution$TEST[, targetName]
  X_test <- data4execution$TEST[, varsTrain]
  
  bestModelXGB <- NULL
  bestPredictionsTest <- NULL
  metrics <- Inf
  
  print("XGBOOST")
  modelXG <- list()
  predictionsTrain <- list()
  predictionsTest <- list()
  predictionsValidation <- list()
  xgtrain <- xgb.DMatrix(data = as.matrix(X_train), label = Y_train)
  xgtest <- xgb.DMatrix(data = as.matrix(X_test), label = Y_test)
  watchlist <- list(train = xgtrain, val = xgtest)
  
  # hyperparameter tunning tune eta and nrounds
  parametersCV <- list(
    objective = "reg:linear",
    eval_metric = "rmse",
    gamma = 1,
    max_depth = 7,
    min_child_weight = 1,
    subsample = 1,
    colsample_bytree = 0.8
  )
  
  bestTestRMSE <- Inf
  bestEta <- NULL
  bestNrounds <- NULL
  etaArray <- c(0.3, 0.1, 0.05, 0.03, 0.02, 0.015, 0.01, 0.005, 0.003, 0.001)
  
  for (etaValue in etaArray){
    
    modelCV <- xgboost::xgb.cv(
      data = xgtrain,
      nfold = 5,
      watchlist = watchlist,
      early_stopping_rounds = 100,
      eta = etaValue,
      params = parametersCV,
      nthread = 4,
      maximize = FALSE
    )
    
    minTestRMSE <- min(modelCV$evaluation_log$test_rmse_mean)
    
    if(minTestRMSE < bestTestRMSE){
      
      
      bestTestRMSE <- minTestRMSE
      bestEta <- modelCV$params$eta
      bestNrounds <- modelCV$best_iteration
      
    }
    
  }
  
  # create grid
  if (num_params_optimization == 0){
    gridS <- data.frame(max_depth = 10,
                        eta = 0.01,
                        gamma = 1,
                        nrounds = 1000,
                        colsample_bytree = 0.8,
                        min_child_weight = 1,
                        stringsAsFactors = F)
  }else{
    
    gridS <- data.frame(iterations = integer(), learning_rate = numeric(),
                        depth = integer(), stringsAsFactors = F)
    for (i in 1:num_params_optimization){
      
      gridS <- rbind(gridS, data.frame(list(max_depth = sample(6:16, 1),
                                            eta = bestEta,
                                            nrounds = bestNrounds,
                                            gamma = runif(1, 0, 0.2),
                                            subsample = sample(c(0.6, 0.7, 0.75,
                                                                 0.85, 0.9, 0.95, 1), 1),
                                            colsample_bytree = sample(c(0.6, 0.7, 0.75, 0.85, 0.9, 0.95, 1), 1),
                                            min_child_weight = sample(1:40, 1)
      )))
      
      
    }
    
    defaultParams = data.frame(
      max_depth = 10,
      eta = 0.01,
      gamma = 1,
      nrounds = 1000,
      colsample_bytree = 0.8,
      subsample = 1,
      min_child_weight = 1,
      stringsAsFactors = F
    )
    
    gridS <- rbind(gridS, defaultParams)
  }
  
  gridS <- dplyr::distinct(gridS)
  
  #. tune tree based hyperparams
  bestTestRMSE <- Inf
  for(i in 1:nrow(gridS)){
    
    parametersCV <- list(
      objective = "reg:linear",
      eval_metric = "rmse",
      max_depth = gridS[i, "max_depth"],
      eta = bestEta,
      gamma = gridS[i, "gamma"],
      subsample = gridS[i, "subsample"],
      colsample_bytree = gridS[i, "colsample_bytree"]
    )
    
    
    
  }
  
  
}

