# ************************************************
# stratifyDataset() :
#
# splits the dataset into its differnet class
#
#
# INPUT   : dataframe - dataset - dataset to be stratified
#         : character - output - FTR outputt field
#         : double - folds - the number of the fold
#
#
# OUTPUT  : dataframe - the stratified dataset ready for Cross Validation.
# ************************************************
stratifyDataset <- function(dataset, output, folds){
  
  uniqueClasses <- unique(dataset[,output])
  rowPositions <- which(dataset[,output]==uniqueClasses[1])
  
  win<- dataset[rowPositions,]
  noWin<- dataset[-rowPositions,]
  foldSequenceYes <- rep(seq(1:folds),ceiling(nrow(win)/folds))
  foldSequenceNo  <- rep(seq(1:folds),ceiling(nrow(noWin)/folds))
  
  win$foldIds <- foldSequenceYes[1:nrow(win)]
  noWin$foldIds  <- foldSequenceNo[1:nrow(noWin)]
  
  stratifiedData<-rbind(win, noWin)
  
  stratifiedData <- stratifiedData[sample(nrow(stratifiedData)),]
  
  return(stratifiedData)
  
} 

# ************************************************
# kFoldTrainingSplit() :
#
# Separates the dataset from the stratifed datset
#
# INPUT   : dataframe - dataset - Stratified dataset.
#         : double - fold - FoldIds number for the test set
#
# OUTPUT  : splitData - List - containing the test and train datasets
#
#*************************************************

kFoldTrainingSplit <- function(dataset, fold){
  
  #Create a data.frame containing all of the rows with FoldIds == fold.
  testSet <- subset(dataset, subset = foldIds==fold)
  
  #Create a data.frame contraining the rest of the rows, with FoldIds != fold.
  trainingSet <- subset(dataset, subset = foldIds!=fold)
  
  #Merge the two data.frames  into a single list, ready to be returned.
  #Remove the foldIds field from each of them to not skew the models. 
  splitData <- list(test=subset(testSet, select = -foldIds), train = subset(trainingSet, select = -foldIds))
  
  #Return the separated data in list form ready for modelling. 
  return(splitData)
  
}

# ************************************************
# kFoldModel() :
# running the models on k folds
#
# INPUT       :   function - FUN - the function to be used
#                 dataset - dataset - the pre processed datasets
#                 character - outputField - the output field - FTR
#
# OUTPUT      :   list - the metrics for the models over k folds
# ************************************************
kFoldModel <- function(FUN,dataset,outputField,...){
  
  results <- data.frame()
  
  #Iterate from 1 to number of folds and train / test a model for each.
 
  for (i in 1:K_FOLDS) {
  
    
    #Create the training set consisting of K-1 of the folds,
    #and the testing set of 1 of the folds.
    separatedData<-kFoldTrainingSplit(dataset,i)
    
    #Call the model function passed in the argument with the testing and training data
    #as well as the value of i so the model can be saved and loaded later.
    modelMeasures<-FUN(train=separatedData$train,
                       test=separatedData$test,outputField,i,...)
    
    #Bind the results list to the result data frame
    results <- rbind(results, modelMeasures)
    
  }
  
  #Average the results from all K models.
  resultMeans<-colMeans(results)
  #Return the average results as a list.
  return(as.list(resultMeans))
}