# ************************************************
# getHomeAndAwayAverages() :
# gets the averages for the home and away teams 
#
# INPUT       :   dataframe - results_data - the historical results
#                 dataframe - transfer_data_df - the transfer data
#                 dataframe - transfer_inflation - the transfer inflation rates data
#                 boolean   - neural_net - TRUE if averages are for NN model
#
# OUTPUT      :   list - list of the home and away averages to be used in the model prediction
# ************************************************
getHomeAndAwayAverages <- function(dataset, homeTeamName, awayTeamName, neural_net = TRUE){
  
  return_list <- list()
  
  if(neural_net == TRUE) {
    to_use_dataset <-  dataset[dataset[homeTeamName] == 1, ]
    to_use_dataset_01 <-  dataset[dataset[awayTeamName] == 1, ]
  } else {
    to_use_dataset    <-  dataset %>% dplyr::filter(HomeTeam == homeTeamName)
    to_use_dataset_01 <-  dataset %>% dplyr::filter(AwayTeam == awayTeamName)
  }
 
  
  return_list$HS <- mean(to_use_dataset$HS)
  return_list$HST <- mean(to_use_dataset$HST)
  return_list$HY <- mean(to_use_dataset$HY)
  return_list$HR <- mean(to_use_dataset$HR)
  return_list$Pos <- mean(to_use_dataset$Pos)
  return_list$HC <- mean(to_use_dataset$HC)
  return_list$AC <- mean(to_use_dataset$AC)
  return_list$HF <- mean(to_use_dataset$HF)
  return_list$AF <- mean(to_use_dataset$AF)
  
  return_list$AS <- mean(to_use_dataset_01$AS)
  return_list$AST <- mean(to_use_dataset_01$AST)
  return_list$AY <- mean(to_use_dataset_01$AY)
  return_list$AR <- mean(to_use_dataset_01$AR)
  
  return(return_list)
}