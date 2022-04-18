# ************************************************
# loadModels() :
# load the saved models to be used in the application
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# ************************************************

loadModels <- function(){
  returnList <- list()
  
  # returnList$withTransfer1 <- h2o.loadModel(path = "withTransfer/grids_1/model/grid_search_1_model_1")
  # returnList$withTransfer2 <- h2o.loadModel(path = "withTransfer/grids_2/model/grid_search_2_model_2")
  # returnList$withTransfer3 <- h2o.loadModel(path = "withTransfer/grids_3/model/grid_search_3_model_195")
  # returnList$withTransfer4 <- h2o.loadModel(path = "withTransfer/grids_4/model/grid_search_4_model_191")
  # returnList$withTransfer5 <- h2o.loadModel(path = "withTransfer/grids_5/model/grid_search_5_model_56")
  # returnList$withTransfer6 <- h2o.loadModel(path = "withTransfer/grids_6/model/grid_search_6_model_89")
  # returnList$withTransfer7 <- h2o.loadModel(path = "withTransfer/grids_7/model/grid_search_7_model_167")
  # returnList$withTransfer8 <- h2o.loadModel(path = "withTransfer/grids_8/model/grid_search_8_model_151")
  # 
  # returnList$withoutTransfer1 <- h2o.loadModel(path = "withoutTransfer/grids_1/model/grid_search_1_model_8")
  # returnList$withoutTransfer2 <- h2o.loadModel(path = "withoutTransfer/grids_2/model/grid_search_2_model_12")
  # returnList$withoutTransfer3 <- h2o.loadModel(path = "withoutTransfer/grids_3/model/grid_search_3_model_155")
  # returnList$withoutTransfer4 <- h2o.loadModel(path = "withoutTransfer/grids_4/model/grid_search_4_model_29")
  # returnList$withoutTransfer5 <- h2o.loadModel(path = "withoutTransfer/grids_5/model/grid_search_5_model_29")
  # returnList$withoutTransfer6 <- h2o.loadModel(path = "withoutTransfer/grids_6/model/grid_search_6_model_133")
  # returnList$withoutTransfer7 <- h2o.loadModel(path = "withoutTransfer/grids_7/model/grid_search_7_model_15")
  # returnList$withoutTransfer8 <- h2o.loadModel(path = "withoutTransfer/grids_8/model/grid_search_8_model_116")
  
  returnList$withTransfer1 <- h2o.loadModel(path = "./withTransfer1/grids_1/model/grid_search_1_model_196")
  returnList$withTransfer2 <- h2o.loadModel(path = "./withTransfer1/grids_2/model/grid_search_2_model_87")
  returnList$withTransfer3 <- h2o.loadModel(path = "./withTransfer1/grids_3/model/grid_search_3_model_163")
  returnList$withTransfer4 <- h2o.loadModel(path = "./withTransfer1/grids_4/model/grid_search_4_model_169")
  returnList$withTransfer5 <- h2o.loadModel(path = "./withTransfer1/grids_5/model/grid_search_5_model_214")
  returnList$withTransfer6 <- h2o.loadModel(path = "./withTransfer1/grids_6/model/grid_search_6_model_28")
  returnList$withTransfer7 <- h2o.loadModel(path = "./withTransfer1/grids_7/model/grid_search_7_model_119")
  returnList$withTransfer8 <- h2o.loadModel(path = "./withTransfer1/grids_8/model/grid_search_8_model_107")

  returnList$withoutTransfer1 <- h2o.loadModel(path = "./withoutTransfer1/grids_1/model/grid_search_1_model_124")
  returnList$withoutTransfer2 <- h2o.loadModel(path = "./withoutTransfer1/grids_2/model/grid_search_2_model_56")
  returnList$withoutTransfer3 <- h2o.loadModel(path = "./withoutTransfer1/grids_3/model/grid_search_3_model_9")
  returnList$withoutTransfer4 <- h2o.loadModel(path = "./withoutTransfer1/grids_4/model/grid_search_4_model_128")
  returnList$withoutTransfer5 <- h2o.loadModel(path = "./withoutTransfer1/grids_5/model/grid_search_5_model_207")
  returnList$withoutTransfer6 <- h2o.loadModel(path = "./withoutTransfer1/grids_6/model/grid_search_6_model_83")
  returnList$withoutTransfer7 <- h2o.loadModel(path = "./withoutTransfer1/grids_7/model/grid_search_7_model_121")
  returnList$withoutTransfer8 <- h2o.loadModel(path = "./withoutTransfer1/grids_8/model/grid_search_8_model_128")
  
  return(returnList)
}

# ************************************************
# predictWithTransfer() :
# function to predict the result of the football matches using transfer data
#
# INPUT       :   dataframe - dataset - the dataset to use 
#                 double    - homeExp - the home team expenditure
#                 double    - homeInc - the home team income
#                 double    - awayExp - the away team expenditure 
#                 double    - awayInc - the away team income
#                 character - homeTeam - the home team 
#                 character - awayTeam - the away team
#                 list      - models - the laoded models
# 
#
# OUTPUT      :   vector  - containing the output result from the prediction wit
#                           probabilities of each class
# ************************************************

predictWithTransfer <- function(dataset, homeExp, homeInc, awayExp, awayInc, homeTeam, awayTeam, models){
  test_pred <- dataset
  
  df_forPrediction <- data.frame(matrix(ncol = ncol(test_pred), nrow = 0))
  colnames(df_forPrediction) <- colnames(test_pred)

  emptyRow <- rep(0, ncol(test_pred))
  df_forPrediction[1,] <- emptyRow
  
  df_forPrediction$Season <- 2021
  df_forPrediction$HomeNetSpend <- (homeExp - homeInc) *1e6
  df_forPrediction$AwayNetSpend <- (awayExp - awayInc) *1e6
  
  awayName <- awayTeam
  awayTeamName <- gsub(" ", ".", awayName)
  awayTeamName <- paste0("AwayTeam", awayTeamName)
  
  homeName <- homeTeam
  homeTeamName <- gsub(" ", ".", homeName)
  homeTeamName <- paste0("HomeTeam", homeTeamName)
  
  df_forPrediction[,awayTeamName] <- 1
  df_forPrediction[,homeTeamName] <- 1
  
  averages <- getHomeAndAwayAverages(dataset = dataset, homeTeamName = homeTeamName, awayTeamName = awayTeamName, neural_net = TRUE)
  df_forPrediction$HS  <- averages$HS
  df_forPrediction$AS  <- averages$AS
  df_forPrediction$HST <- averages$HST
  df_forPrediction$AST <- averages$AST
  df_forPrediction$HY  <- averages$HY
  df_forPrediction$AY  <- averages$AY
  df_forPrediction$HR  <- averages$HR
  df_forPrediction$AR  <- averages$AR
  df_forPrediction$Pos <- averages$Pos
  df_forPrediction$HC <- averages$HC
  df_forPrediction$AC <- averages$AC
  df_forPrediction$HF <- averages$HF
  df_forPrediction$AF <- averages$AF
  
  test_pred <- rbind(test_pred,df_forPrediction)
  test_pred <- test_pred %>%
    select(-FTR)
  test_pred_01 <- as.data.frame(lapply(test_pred[,1:16],normalise))
  test_pred_02 <- cbind(test_pred_01, test_pred[,17:102])
  
  last_row <- tail(test_pred_02, 1)
  
  showModal(modalDialog(
    title = "Please Wait...",
    "Prediction May Take Up To 10 Seconds to load!",fade = T,
    size = "l"
  ))
  
  # result_1 <-   round(as.numeric(as.vector(predict(models$withTransfer1,
  #                                                  as.h2o(last_row)))),2)
  # result_2 <-   round(as.numeric(as.vector(predict(models$withTransfer2,
  #                                                  as.h2o(last_row)))),2)
  # result_3 <-   round(as.numeric(as.vector(predict(models$withTransfer3,
  #                                                  as.h2o(last_row)))),2)
  # result_4 <-   round(as.numeric(as.vector(predict(models$withTransfer4,
  #                                                  as.h2o(last_row)))),2)
  # result_5 <-   round(as.numeric(as.vector(predict(models$withTransfer5,
  #                                                  as.h2o(last_row)))),2)
  # result_6 <-   round(as.numeric(as.vector(predict(models$withTransfer6,
  #                                                  as.h2o(last_row)))),2)
  # result_7 <-   round(as.numeric(as.vector(predict(models$withTransfer7,
  #                                                  as.h2o(last_row)))),2)
  # result_8 <-   round(as.numeric(as.vector(predict(models$withTransfer8,
  #                                                  as.h2o(last_row)))),2)
  result_1 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withTransfer2/grids_1/model/grid_search_1_model_18.zip"))
                                 ),2)
  result_2 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withTransfer2/grids_2/model/grid_search_2_model_76.zip"))
                                 ),2)
  result_3 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withTransfer2/grids_3/model/grid_search_3_model_88.zip"))
                                 ),2)
  result_4 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withTransfer2/grids_4/model/grid_search_4_model_84.zip"))
                                 ),2)
  result_5 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withTransfer2/grids_5/model/grid_search_5_model_211.zip"))
                                 ), 2)
  result_6 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withTransfer2/grids_6/model/grid_search_6_model_11.zip"))
                                 ),2)
  result_7 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withTransfer2/grids_7/model/grid_search_7_model_4.zip"))
                                 ),2)
  result_8 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withTransfer2/grids_8/model/grid_search_8_model_55.zip"))
                                 ),2)
  
  
  mean_result <- mean(result_1[1], result_2[1], result_3[1], result_4[1],
                      result_5[1], result_6[1], result_7[1], result_8[1])
  mean_away <- mean(result_1[2], result_2[2], result_3[2], result_4[2],
                    result_5[2], result_6[2], result_7[2], result_8[2])
  mean_draw <- mean(result_1[3], result_2[3], result_3[3], result_4[3],
                    result_5[3], result_6[3], result_7[3], result_8[3])
  mean_home <- mean(result_1[4], result_2[4], result_3[4], result_4[4],
                    result_5[4], result_6[4], result_7[4], result_8[4])
  
  removeModal()
  return(c(mean_result, mean_away, mean_draw, mean_home))
}

# ************************************************
# predictWithoutTransfer() :
# function to predict the result of the football matches excluding transfer data
#
# INPUT       :   dataframe - dataset - the dataset to use
#                 character - homeTeam - the home team 
#                 character - awayTeam - the away team
#                 list      - models - the laoded models
# 
#
# OUTPUT      :   vector  - containing the output result from the prediction wit
#                           probabilities of each class
# ************************************************

predictWithoutTransfer <- function(dataset, homeTeam, awayTeam, models){
  test_pred <- dataset %>%
    select(-c(HomeNetSpend, AwayNetSpend))
  
  df_forPrediction <- data.frame(matrix(ncol = ncol(test_pred), nrow = 0))
  colnames(df_forPrediction) <- colnames(test_pred)
  emptyRow <- rep(0, ncol(test_pred))
  df_forPrediction[1,] <- emptyRow
  
  df_forPrediction$Season <- 2021
  
  awayName <- awayTeam
  awayTeamName <- gsub(" ", ".", awayName)
  awayTeamName <- paste0("AwayTeam", awayTeamName)
  
  homeName <- homeTeam
  homeTeamName <- gsub(" ", ".", homeName)
  homeTeamName <- paste0("HomeTeam", homeTeamName)
  
  df_forPrediction[,awayTeamName] <- 1
  df_forPrediction[,homeTeamName] <- 1
  
  averages <- getHomeAndAwayAverages(dataset = dataset, homeTeamName = homeTeamName, awayTeamName = awayTeamName, neural_net = TRUE)
  df_forPrediction$HS  <- averages$HS
  df_forPrediction$AS  <- averages$AS
  df_forPrediction$HST <- averages$HST
  df_forPrediction$AST <- averages$AST
  df_forPrediction$HY  <- averages$HY
  df_forPrediction$AY  <- averages$AY
  df_forPrediction$HR  <- averages$HR
  df_forPrediction$AR  <- averages$AR
  df_forPrediction$Pos <- averages$Pos
  df_forPrediction$HC <- averages$HC
  df_forPrediction$AC <- averages$AC
  df_forPrediction$HF <- averages$HF
  df_forPrediction$AF <- averages$AF
  
  test_pred <- rbind(test_pred,df_forPrediction)
  test_pred <- test_pred %>%
    select(-FTR)
  test_pred_01 <- as.data.frame(lapply(test_pred[,1:14],normalise))
  test_pred_02 <- cbind(test_pred_01, test_pred[,15:100])
  
  last_row <- tail(test_pred_02, 1)
  
  showModal(modalDialog(
    title = "Please Wait...",
    "Prediction May Take Up To 10 Seconds to load!",fade = T,
    size = "l"
  ))
  
  # result_1 <-   round(as.numeric(as.vector(predict(models$withoutTransfer1,
  #                                                  as.h2o(last_row)))),2)
  # result_2 <-   round(as.numeric(as.vector(predict(models$withoutTransfer2,
  #                                                  as.h2o(last_row)))),2)
  # result_3 <-   round(as.numeric(as.vector(predict(models$withoutTransfer3,
  #                                                  as.h2o(last_row)))),2)
  # result_4 <-   round(as.numeric(as.vector(predict(models$withoutTransfer4,
  #                                                  as.h2o(last_row)))),2)
  # result_5 <-   round(as.numeric(as.vector(predict(models$withoutTransfer5,
  #                                                  as.h2o(last_row)))),2)
  # result_6 <-   round(as.numeric(as.vector(predict(models$withoutTransfer6,
  #                                                  as.h2o(last_row)))),2)
  # result_7 <-   round(as.numeric(as.vector(predict(models$withoutTransfer7,
  #                                                  as.h2o(last_row)))),2)
  # result_8 <-   round(as.numeric(as.vector(predict(models$withoutTransfer8,
  #                                                  as.h2o(last_row)))),2)
  
  result_1 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withoutTransfer2/grids_1/model/grid_search_1_model_214.zip")))
                      ,2)
  result_2 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withoutTransfer2/grids_2/model/grid_search_2_model_189.zip")))
                      ,2)
  result_3 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withoutTransfer2/grids_3/model/grid_search_3_model_125.zip")))
                      ,2)
  result_4 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withoutTransfer2/grids_4/model/grid_search_4_model_173.zip")))
                      ,2)
  result_5 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withoutTransfer2/grids_5/model/grid_search_5_model_176.zip")))
                      , 2)
  result_6 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withoutTransfer2/grids_6/model/grid_search_6_model_202.zip")))
                      ,2)
  result_7 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withoutTransfer2/grids_7/model/grid_search_7_model_201.zip")))
                      ,2)
  result_8 <-   round(as.numeric(as.vector(h2o.mojo_predict_df(frame = last_row, 
                                                               mojo_zip_path = "withoutTransfer2/grids_8/model/grid_search_8_model_196.zip")))
                      ,2)
  
  mean_result <- mean(result_1[1], result_2[1], result_3[1], result_4[1],
                      result_5[1], result_6[1], result_7[1], result_8[1])
  mean_away <- mean(result_1[2], result_2[2], result_3[2], result_4[2],
                    result_5[2], result_6[2], result_7[2], result_8[2])
  mean_draw <- mean(result_1[3], result_2[3], result_3[3], result_4[3],
                    result_5[3], result_6[3], result_7[3], result_8[3])
  mean_home <- mean(result_1[4], result_2[4], result_3[4], result_4[4],
                    result_5[4], result_6[4], result_7[4], result_8[4])
  
  removeModal()
  return(c(mean_result, mean_away, mean_draw, mean_home))
}
