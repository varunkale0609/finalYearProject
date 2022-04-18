# 
# # method for finding the params for the RF model
# find_best_rf_metrics <- function(results_data, transfer_data_df, transfer_inflation){
#   
#   print("Warning: This will take a few hours to run")
#   
#   toReturn <- list()
#   # pre proccesed data
#   pre_processed_data <- preprocessing_for_RF(results_data = results_data, 
#                                              transfer_data_df = transfer_data_df,
#                                              transfer_inflation = transfer_inflation)
#   pre_processed_data$FTR <- as.factor(pre_processed_data$FTR) 
#   
#   # split data
#   holdoutDataset <- createHoldoutDataset(dataset = pre_processed_data, holdout = 70 )
#   training <- holdoutDataset$training
#   test     <- holdoutDataset$test
#   
#   set.seed(123)
#   
#   # Step 1 Define the control
#   trControl <- trainControl(method = "cv",
#                             number = 8,
#                             search = "grid")
#   
#   tuneGrid <- expand.grid(.mtry = c(40: 50))
#   forest_mtry <- train(FTR~.,
#                    data = training,
#                    method = "rf",
#                    metric = "Accuracy",
#                    tuneGrid = tuneGrid,
#                    trControl = trControl,
#                    importance = TRUE,
#                    nodesize = 14,
#                    ntree = 300)
# 
#   mtry <- forest_mtry$bestTune$mtry 
#   toReturn$mtry <- mtry
#   
#   store_maxnode <- list()
#   tuneGrid <- expand.grid(.mtry = optimal_mtry)
#   for (maxnodes in c(5: 15)) {
#     set.seed(1234)
#     forest_maxnodes <- train(FTR~.,
#                         data = training,
#                         method = "rf",
#                         metric = "Accuracy",
#                         tuneGrid = tuneGrid,
#                         trControl = trControl,
#                         importance = TRUE,
#                         nodesize = 14,
#                         maxnodes = maxnodes,
#                         ntree = 300)
#     current_iteration <- toString(maxnodes)
#     store_maxnode[[current_iteration]] <- forest_maxnodes
#   }
#   results_mtry <- resamples(store_maxnode)
#   summary_maxNodes <- summary(results_mtry)
#   
# }
# 
# 
# store_maxnode <- list()
# tuneGrid <- expand.grid(.mtry = optimal_mtry)
# for (maxnodes in c(5: 15)) {
#   set.seed(1234)
#   forest_maxnodes <- train(FTR~.,
#                       data = training,
#                       method = "rf",
#                       metric = "Accuracy",
#                       tuneGrid = tuneGrid,
#                       trControl = trControl,
#                       importance = TRUE,
#                       nodesize = 14,
#                       maxnodes = maxnodes,
#                       ntree = 300)
#   current_iteration <- toString(maxnodes)
#   store_maxnode[[current_iteration]] <- forest_maxnodes
# }
# results_mtry <- resamples(store_maxnode)
# summary(results_mtry)
# 
# 
# store_maxnode <- list()
# tuneGrid <- expand.grid(.mtry = optimal_mtry)
# for (maxnodes in c(20: 30)) {
#   set.seed(1234)
#   forest_maxnodes <- train(FTR~.,
#                       data = training,
#                       method = "rf",
#                       metric = "Accuracy",
#                       tuneGrid = tuneGrid,
#                       trControl = trControl,
#                       importance = TRUE,
#                       nodesize = 14,
#                       maxnodes = maxnodes,
#                       ntree = 300)
#   key <- toString(maxnodes)
#   store_maxnode[[key]] <- forest_maxnodes
# }
# results_node <- resamples(store_maxnode)
# summary(results_node)
# #25 is the best 
# 
# save_maximumTrees <- list()
# for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
#   set.seed(5678)
#   forest_trees <- train(FTR~.,
#                        data = training,
#                        method = "rf",
#                        metric = "Accuracy",
#                        tuneGrid = tuneGrid,
#                        trControl = trControl,
#                        importance = TRUE,
#                        nodesize = 14,
#                        maxnodes = 24,
#                        ntree = ntree)
#   key <- toString(ntree)
#   save_maximumTrees[[key]] <- forest_trees
# }
# results_tree <- resamples(save_maximumTrees)
# summary(results_tree)
# #450 is best

# ************************************************
# createForest() :
#
# Creates a Random Forest on a dataset
#
# INPUT   :
#         :   Data Frame     - train              - train dataset
#             Data Frame     - test               - test dataset
#             character      - predictorField     - the field we are predicting 
#             integer        - forestSize         - the size of the forest to create
#             character      - title              - title for the forest
#             boolean        - plot               - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
createForest<-function(train,test,predictorField,forestSize,title = "Importance for Random Forest",plot=TRUE) {
  
  # Need to produce a data frame from the predictor fields and a vector for the output
  outputClassIndex <- which(names(train) == predictorField)
  inputs <- train[-outputClassIndex]
  output <- train[, outputClassIndex]
  
  rf<-randomForest::randomForest(inputs,
                                 factor(output),
                                 ntree=1000,
                                 importance=TRUE,
                                 mtry=10,
                                 maxnodes = 9)
  
  return(rf)
}

# ************************************************
# createAndEvaluateForest() :
#
# Creates Random Forest on a dataset and evaluates it
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
createAndEvaluateForest<-function(train,test,predictorField,i,load,forestSize,title = "Importance for Random Forest",showInViewer=F,plot=TRUE, withTransfer = TRUE){
  
  if (!load) {
    
    rf <- createForest(train, test, predictorField, forestSize, title, plot=plot)
    
    
    if(withTransfer == TRUE){
      
      if (!dir.exists("With_Transfer_Tree_Models")) {
        dir.create("With_Transfer_Tree_Models")
      }
      
      saveRDS(rf, paste0("With_Transfer_Tree_Models/rf_", i, ".rds"))
      
    } else if(withTransfer == FALSE){
      
      if (!dir.exists("Without_Transfer_Tree_Models")) {
        dir.create("Without_Transfer_Tree_Models")
      }
      
      saveRDS(rf, paste0("Without_Transfer_Tree_Models/rf_", i, ".rds"))
      
    }
    
  } else {
    
    if(withTransfer == TRUE){
      
      rf <- readRDS(paste0("With_Transfer_Tree_Models/rf_", i, ".rds"))
      
    } else if(withTransfer == FALSE){
      
      rf <- readRDS(paste0("Without_Transfer_Tree_Models/rf_", i, ".rds"))
    }
    
  }
  
  treeClassifications <- getTreeClassifications(rf, test, predictorField)
  confusionMat <- confusionMatrix(treeClassifications, as.factor(test$FTR))
  
  treeMetrics <- getRandomForestMetrics(confusionMat, treeClassifications, test)
  
  inputs <- train[-(which(names(train) == predictorField))]
  
  # We use type=prob here so that we can later find the ideal threshold for the classifier
  predClass <- predict(rf, inputs, type = "class")
  
  treeMetrics$RMSE_train <- caret::RMSE(as.numeric(as.vector(predClass)), train$FTR)

  
  return(treeMetrics)
} #endof createAndEvaluateForest()



# ************************************************
# getTreeClassifications() :
#
# Run the predict function on a decision tree to generate the predicted classes
#
# INPUT
#         :   object         - tree                          - the trained decision tree
#         :   Data Frame     - testDataset                   - the test dataset used to make predictions on
#         :   string         - predictorField                - the name of the predictor field in the dataset
#
# OUTPUT  
#         :   double vector  - predictedClassProbabilities   - a vector consisting of all of the classifications generated for the given dataset
#
# ************************************************
getTreeClassifications <- function(tree, testDataset, predictorField){
  # Use the input fields to generate outputs from the tree
  inputs <- testDataset[-(which(names(testDataset) == predictorField))]
  
  # We use type=prob here so that we can later find the ideal threshold for the classifier
  predictedClassProbabilities <- predict(tree, inputs, type = "class")
  
  return(predictedClassProbabilities)
} #endof getTreeClassifications()

# ************************************************
# getRandomForestMetrics() :
# gets random forest metrics
#
# INPUT       :   matrix - confusionMat - confusion matrix for the forest model
#                 vector - treeClassifcations - the correct classifications
#                 dataframe - test - the test dataset
#
# OUTPUT      :   list - list of the random forest model metrics
# ************************************************
getRandomForestMetrics <- function(confusionMat, treeClassifcations, test){
  
  toReturn <- list()
  
  toReturn$Accuracy <- confusionMat$overall["Accuracy"]
  #toReturn$Kappa    <- confusionMat$overall["Kappa"]
  
  treee <<- as.numeric(as.vector(treeClassifcations))
  toReturn$RMSE <-   caret::RMSE(as.numeric(as.vector(treeClassifcations)), (test$FTR))
  toReturn$MAE <- caret::MAE(as.numeric(as.vector(treeClassifcations)), (test$FTR))
  toReturn$R2 <- caret::R2(as.numeric(as.vector(treeClassifcations)), (test$FTR))
  toReturn$MSE <- MLmetrics::MSE(as.numeric(as.vector(treeClassifcations)), (test$FTR))
  
  confMetrics <-  getConfusionMatrixMetrics(confusionMat$table)
  
  toReturn$TP                   <- confMetrics$TP
  toReturn$FP                   <- confMetrics$FP
  toReturn$TN                   <- confMetrics$TN
  toReturn$FN                   <- confMetrics$FN
  toReturn$Precision            <- confMetrics$Precision
  toReturn$Recall               <- confMetrics$Recall
  toReturn$WeightedF1          <- confMetrics$WeightedF1
  
  # caret::RMSE()
  #caret::log
  return(toReturn)
  
}

pre_processed_data <- pre_processed_dataset(results = results_data, 
                                            transfer_data_df = transfer_data_df,
                                            transfer_inflation = transfer_inflation,
                                            with_transfer = TRUE,
                                            randomForest = TRUE)

stratifiedData <- stratifyDataset(pre_processed_data,OUTPUT_FIELD,K_FOLDS)

# This code trains random forest models using stratified cross validation.
#*****************************************************************************************************************  

# Forest_Stratified_Cross_Val <- kFoldModel(createAndEvaluateForest, stratifiedData, OUTPUT_FIELD, load = F, forestSize = 1000, plot=F, withTransfer = TRUE)
