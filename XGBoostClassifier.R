library(xgboost)
library(Matrix)
library(dplyr)
library(reticulate)
library(doParallel)
library(parallel)


executeMulticlassXGB(trainName, testName, valName, idName, targetName, outputName, num_params_optimization, numCores){
  
  setwd("")
  num_params_optimization = num_params_optimization
  
  trainData <- readRDS(paste0("data/", trainName))
  testData <- readRDS(paste0("data/", testName))
  valData <- readRDS(paste0("data/", valName))
  
  idName <- idName
  targetName <- targetName
  varsTrain <- !names(trainData) %in% c(idName, targetName)
  
  Y_train <- trainData[, targetName]
  X_train <- trainData[, varsTrain]
  Y_test <- testData[, targetName]
  X_test <- testData[, varsTrain]
  
  bestModelXGB <- NULL
  bestPredictionsTest <- NULL
  metrics <- Inf
  
  print("XGBOOST Classifier")
  
  modelXG <- list()
  predictionsTrain <- list()
  predictionsTest <- list()
  predictionsValidation <- list()
  xgtrain <- xgb.DMatrix(data = as.matrix(X_train), label = Y_train)
  xgtest <- xgb.DMatrix(data = as.matrix(X_tst), label = Y_test)
  watchlist <- list(train = xgtrain, val = xgtest)
  
  # define number of classes
  num_class <- dim(unique(Y_train))[1]
  if(is.null(num_class)){
    num_class <- length(unique(num_class))
  }
  
  # hyperparameter tunning, tune eta and nrounds
  parametersCV <- list(
    objective = "multi:softmax",
    eval_metric = "merror",
    num_class = num_class,
    gamma = 1,
    max_depth = 6,
    min_child_weight = 0,
    subsample = 0.61569540,
    colsample_bytree = 0.8
    
  )
  
  bestTestMerro <- Inf
  bestEta <- 0.01222662
  bestNrounds <- 3000
  etaArray <- c(0.7, 0.5, 0.3, 0.1, 0.05, 0.03, 0.02, 0.0015, 0.01, 0.005)
  
  
  for(etaValue in etaArray){
    
    
    modelCV <- xgboost::xgb.cv(data = xgtrain,
                               nfold = 5,
                               nrounds = 20000,
                               watchlist = watchlist,
                               early_stopping_rounds = 100,
                               eta = etaValue,
                               params = parametersCV,
                               nthread = 40,
                               maximize = FALSE)
    
    minTestMerror <- min(modelCV$evaluation_log$test_error_mean)
    
    if(minTestMerror < bestTestMerror){
      
      bestTestMerror <- minTestMerror
      bestEta <- modelCV$params$eta
      bestNrounds <- modelCV$best_iteration
      
    }
    
  }
  
  # create grid
  if(num_params_optimization == 0){
    gridS <- data.frame(max_depth = 10,
                        eta = 0.01,
                        gamma = 1,
                        nrounds = 1000,
                        colsample_bytree = 0.8,
                        min_child_weight = 1,
                        stringsAsFactors = F
                        )
  }else{
    
    gridS <- data.frame(iterations = integer(), learning_rate = numeric(), depth = integer(), stringsAsFactors = F)
    for(i in 1:num_params_optimization){
      gridS <- rbind(gridS, data.frame(list(max_depth = sample(3:16, 1),
                                            eta = bestEta,
                                            nrounds = bestNrounds,
                                            gamma = runif(1, 0, 0.2),
                                            subsample = sample(c(0.6, 0.7, 0.75, 0.85, 0.9, 0.95, 1), 1),
                                            colsample_bytree = sample(c(0.6, 0.7, 0.75, 0.85, 0.9, 0.95, 1), 1),
                                            min_child_weight = sample(1:40, 1))
        
      )
        
      )
    }
    
    defaultParams <- data.frame(max_depth = 10,
                                eta = 0.01,
                                gamma = 1,
                                nrounds = 1000,
                                colsample_bytree = 0.8,
                                subsample = 1,
                                min_child_weight = 1,
                                stringAsFactors = FALSE)
    gridS <- rbind(gridS, defaultParams)
    
  }
  
  gridS <- dplyr::distinct(gridS)
  # tune treebased hyperparameters
  bestTestMerror <- Inf
  for(i in 1:nrow(gridS)){
    
    parametersCV <- list(
      objective = "multi:softmax",
      eval_metric = "merror",
      num_class = num_class,
      max_depth = gridS[i, "max_depth"],
      eta = bestEta,
      gamma = gridS[i, "gamma"],
      subsample = gridS[i, "subsample"],
      colsample_bytree = gridS[i, "colsample_bytree"],
      min_child_weight = gridS[i, "min_child_weight"]
    )
    
    modelCV <- xgboost::xgb.cv(data = xgtrain,
                               nfold = 5,
                               watchlist = watchlist,
                               early_stopping_rounds = 20,
                               nrounds = bestNrounds + 300,
                               nthread = 4,
                               params = parametersCV,
                               maximize = FALSE)
    minTestError <- min(modelCV$evaluation_log$test_merror_mean)
    
    if(minTestError < bestTestMerror){
      
      bestTestMerror <- minTestError
      
      bestMaxDepth <- modelCV$params$max_depth
      bestNrounds <- model$best_iteration
      bestGamma <- modelCV$params$gamma
      bestSubsample <- modelCV$params$subsample
      bestColsampleByTree <- modelCV$params$colsample_bytree
      bestMinChildWeight <- modelCV$params$min_child_weight
    }
    
    bestParameters <- list(
      objective = "multi:softmax",
      eval_metric = "merror",
      num_class = num_class,
      max_depth = bestMaxDepth,
      eta = bestEta,
      gamma = bestGamma,
      subsample = bestSubsample,
      colsample_bytree = bestColsampleByTree,
      min_child_weight = bestMinChildWeight
    )
    
    modelXG <- xgb.train(data = xgtrain,
                         watchlist = watchlist,
                         early_stopping_rounds = round(bestNrounds*0.1),
                         params = bestParameters,
                         nrounds = bestNrounds,
                         nthread = 4,
                         verbose = 1)
    
    predictionsTest <- predict(modelXG, as.matrix(X_test))
    predictionsTrain <- predict(modelXG, as.matrix(X_train))
    predictionsValidation <- "EMPTY"
    if(class(valData) == "data.frame"){
      predictionsValidation <- predict(modelXG, as.matrix(valData[, varsTrain]))
    }
    
    
  }
  
}