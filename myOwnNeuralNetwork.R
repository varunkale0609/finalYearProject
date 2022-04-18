#load the H20 package
library(h2o)
h2o.init()

h2o.removeAll()

# ************************************************
# runNeuralNetworkModels() :
# function to run or load the NN models and evaualte metrics
#
# INPUT       :   dataframe - training - the training data
#                 dataframe - test - the testing data
#                 character - outputField - output field to use - FTR
#                 double    - i - the K fold currently on
#                 boolean   - loadModel - TRUE if model is already saved
#                 boolean   - withTransfer - TRUE if model is to use transfer data
# 
#
# OUTPUT      :   list - list of model metrics
# ************************************************
runNeuralNetworkModels <- function(training, test, outputField, i, loadModel = FALSE, withTransfer = TRUE){
 
  if(loadModel == FALSE){
    trainHex = as.h2o(training) 
    
    predictors <-colnames(training)[!(colnames(training) %in% c("FTR"))]
    response = "FTR"
    
    activation_opt <- c("Rectifier","RectifierWithDropout", "Maxout","MaxoutWithDropout")
    hidden_layers <- list(5, c(5,5))
    epochs <- c(10,50,100)
    l1_opt <- c(0,1e-3,1e-4)
    l2_opt <- c(0,1e-3,1e-4)
    
    hyper_params <- list(activation = activation_opt,
                         hidden = hidden_layers,
                         epochs = epochs,
                         l1 = l1_opt,
                         l2 = l2_opt)
    
    #set search criteria
    search_criteria <- list(strategy = "RandomDiscrete",stopping_metric = "misclassification", max_models=300)
    
    # Perform grid search on training data
    nn_grid <- h2o.grid(x = predictors,
                        y = response,
                        algorithm = "deeplearning",
                        grid_id = paste0("grid_search_", i),
                        hyper_params = hyper_params,
                        search_criteria = search_criteria,
                        training_frame = trainHex)
    
    #Select best model based on mae
    d_grid <- h2o.getGrid(paste0("grid_search_", i),sort_by = "err", decreasing = F)
    d_grid
    best_nn_model <- h2o.getModel(d_grid@model_ids[[1]])
    best_nn_model
    
    if(withTransfer == TRUE){
      
      h2o.download_mojo(best_nn_model,
                                path=paste0("withTransfer2/grids_", i, "/model"),get_genmodel_jar = TRUE)
      
    } else if(withTransfer == FALSE) {
      
      h2o.download_mojo(best_nn_model,
                        path=paste0("withoutTransfer2/grids_", i, "/model"),get_genmodel_jar = TRUE)
      
    }
    
  } else{
    
    if(withTransfer == TRUE){
      
      pathName <- list.files(paste0("withTransfer1/grids_", i, "/model"))
      #load the model
      path <- paste0('withTransfer1/grids_', i, "/model/", pathName)
      best_nn_model <- h2o.loadModel(path = path)
      best_nn_model
      
    } else if(withTransfer == FALSE) {
      
      pathName <- list.files(paste0("withoutTransfer1/grids_", i, "/model"))
      #load the model
      path <- paste0('withoutTransfer1/grids_', i, "/model/", pathName)
      best_nn_model <- h2o.loadModel(path = path)
      best_nn_model
    }
    
    
  }

   metrics <- evaluate_neural_network(test_dataset = test, outputField = OUTPUT_FIELD, model = best_nn_model, i = i)
  
  return(metrics)
  
}

# ************************************************
# evaluate_neural_network() :
# function to evaluate the neural network
#
# INPUT       :   dataframe - test_dataset - the testing data
#                 character - outputField - the output field - FTR
#                 h2o model - model - the NN model 
#                 double - i - the K fold value
#
# OUTPUT      :   list - list of model metrics
# ************************************************
evaluate_neural_network <- function(test_dataset, outputField, model, i){
  
  toReturn <- list()
  
  testHex <- h2o::as.h2o(test_dataset)
    
  pred <- h2o::h2o.predict(model, testHex)
  
  predicted <- as.numeric(as.vector(pred[[1]]))
  actual <- unfactorNumeric(test_dataset$FTR)
  
  conf <- h2o.confusionMatrix(model, newdata = testHex)

  toReturn$Accuracy <-  1 - tail(conf$Error, 1)
  
  confMetrics <- getConfusionMatrixMetrics(conf = conf)
  
  
  metrics <- h2o.performance(model, newdata = testHex)@metrics
  
  toReturn$RMSE                 <- metrics$RMSE
  toReturn$MAE                  <- caret::MAE(predicted, actual)
  #toReturn$R2                   <- metrics$r2
  toReturn$R2                   <- caret::R2(predicted, actual)
  toReturn$MSE                  <- metrics$MSE
  #toReturn$mean_per_class_error <- metrics$mean_per_class_error
 # toReturn$logloss              <- metrics$logloss
  toReturn$TP                   <- confMetrics$TP
  toReturn$FP                   <- confMetrics$FP
  toReturn$TN                   <- confMetrics$TN
  toReturn$FN                   <- confMetrics$FN
  toReturn$Precision            <- confMetrics$Precision
  toReturn$Recall               <- confMetrics$Recall
  toReturn$WeightedF1           <- confMetrics$WeightedF1
  toReturn$RMSE_train        <- model@model$training_metrics@metrics$RMSE    

  return(toReturn)
  
}

getConfusionMatrixMetrics <- function(conf){
  
  awayValues <- list()
  
  awayValues$TP <- conf[1,1]
  awayValues$FP <- conf[1,2] + conf[1,3]
  awayValues$TN <- conf[2,2] + conf[2,3] + conf[3,2] + conf[3,3]
  awayValues$FN <- conf[2,1] + conf[3,1]
  awayValues$Precision <- awayValues$TP/(awayValues$TP + awayValues$FP)
  awayValues$Recall <- awayValues$TP/(awayValues$TP + awayValues$FN)
  awayValues$F1 <- 2*(((awayValues$Precision * awayValues$Recall)/(awayValues$Precision + awayValues$Recall)))
  
  drawValues <- list()
  
  drawValues$TP <- conf[2,2]
  drawValues$FP <- conf[2,1] + conf[2,3]
  drawValues$TN <- conf[1,1] + conf[1,3] + conf[3,1] + conf[3,3]
  drawValues$FN <- conf[1,2] + conf[3,2]
  drawValues$Precision <- drawValues$TP/(drawValues$TP + drawValues$FP)
  drawValues$Recall <- drawValues$TP/(drawValues$TP + drawValues$FN)
  drawValues$F1 <- 2*(((drawValues$Precision * drawValues$Recall)/(drawValues$Precision + drawValues$Recall)))
  
  
  homeValues <- list()
  
  homeValues$TP <- conf[3,3]
  homeValues$FP <- conf[3,1] + conf[3,2]
  homeValues$TN <- conf[1,1] + conf[1,2] + conf[2,1] + conf[2,2]
  homeValues$FN <- conf[1,3] + conf[2,3]
  homeValues$Precision <- homeValues$TP/(homeValues$TP + homeValues$FP)
  homeValues$Recall <- homeValues$TP/(homeValues$TP + homeValues$FN)
  homeValues$F1 <- 2*(((homeValues$Precision * homeValues$Recall)/(homeValues$Precision + homeValues$Recall)))
  
  averages <- list()
  averages$TP <- (homeValues$TP + awayValues$TP + drawValues$TP)
  averages$FP <- (homeValues$FP + awayValues$FP + drawValues$FP)
  averages$TN <- (homeValues$TN + awayValues$TN + drawValues$TN)
  averages$FN <- (homeValues$TN + awayValues$TN + drawValues$TN)
  averages$Precision <- (homeValues$Precision + awayValues$Precision + drawValues$Precision)/3
  averages$Recall <- (homeValues$Recall + awayValues$Recall + drawValues$Recall)/3
  
  
  away <- sum(conf[1:3, 1])
  draw <- sum(conf[1:3, 2])
  home <- sum(conf[1:3, 3])
  total <- home + away + draw
  averages$WeightedF1 <- (homeValues$F1*home + awayValues$F1*away + drawValues$F1*draw)/total
  
  
  return(averages)
  
}

# ************************************************
# getMetricsOfNN() :
# function to retireve the metrics of the NN model
#
# INPUT       :   dataframe - results_data - the historical results
#                 dataframe - transfer_data_df - the transfer data
#                 dataframe - transfer_inflation - the transfer inflation rates data
#                 boolean   - loadModel - TRUE if want to load the saved models
#                 boolean  - withTransfer - TRUE if want to use a witht transfer model
#
# OUTPUT      :   list - list of model metrics
# ************************************************
getMetricsOfNN <- function(results_data, transfer_data_df, transfer_inflation, loadModel, withTransfer){
  
  if(withTransfer == TRUE){
    normalisedDataset <- pre_processed_dataset(results = results_data, transfer_data_df = transfer_data_df, transfer_inflation = transfer_inflation, with_transfer = T, randomForest = FALSE)
  } else if (withTransfer == FALSE){
    normalisedDataset <- pre_processed_dataset(results = results_data, transfer_data_df = transfer_data_df, transfer_inflation = transfer_inflation, with_transfer = F, randomForest = FALSE)
  }
  normalisedDataset$FTR <- as.factor(normalisedDataset$FTR)

  stratifiedData <- stratifyDataset(dataset = normalisedDataset, output = OUTPUT_FIELD, folds = 8)
  
  saveModels <- kFoldModel(runNeuralNetworkModels,stratifiedData,OUTPUT_FIELD,loadModel, withTransfer)
  
  
  return(saveModels)
  
}


# NN_metrics <- getMetricsOfNN(results_data = results_data, transfer_data_df = transfer_data_df, transfer_inflation = transfer_inflation, loadModel = FALSE, withTransfer = TRUE)


