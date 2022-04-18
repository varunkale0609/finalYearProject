
# ************************************************
# pre_processed_dataset() :
# gets the pre-processed dataset
#
# INPUT       :   dataframe - results - the historical results
#                 dataframe - transfer_data_df - the transfer data
#                 dataframe - transfer_inflation - the transfer inflation rates data
#                 boolean   - with_transfer - TRUE if with transfer model data is wanted
#                 boolean   - randomForest - TRUE if data is required for randoom forest model
#
# OUTPUT      :   list - list of the home and away averages to be used in the model prediction
# ************************************************

pre_processed_dataset <- function(results, transfer_data_df, transfer_inflation, with_transfer = TRUE, randomForest = FALSE){
  
  if(randomForest == FALSE){
    
    pre_processed_data <- preprocessing(results_data  = results_data, transfer_data_df = transfer_data_df, transfer_inflation = transfer_inflation)
    pre_processed_data <- as.data.frame(lapply(pre_processed_data,normalise))
    
  } else if(randomForest == TRUE){
    
    pre_processed_data <- preprocessing_for_RF(results_data = results_data, 
                                               transfer_data_df = transfer_data_df,
                                               transfer_inflation = transfer_inflation)
  }

  if(with_transfer == T){
    to_return <- pre_processed_data
  } else {
    to_return <- pre_processed_data %>% select(-c(HomeNetSpend, AwayNetSpend))
  }
  
  return(to_return)
}

