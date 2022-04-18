
# ************************************************
# preprocessing_for_RF() :
# preprocessing for the Random Forest  model
#
# INPUT       :   dataframe - results_data - the historical results
#                 dataframe - transfer_data_df - the transfer data
#                 dataframe - transfer_inflation - the transfer inflation rates data
#
# OUTPUT      :   dataframe - pre-processed data frame
# ************************************************

preprocessing_for_RF <- function(results_data, transfer_data_df, transfer_inflation, score_prediction = FALSE){
  
  dataSetToUse       <- results_and_transfer_combined(results_data = results_data, transfer_data = transfer_data_df, transfer_inflation = transfer_inflation)
  dataSetToUse$DateTime <- as.Date(dataSetToUse$DateTime, format = "%d/%m/%Y")
  
  totalPointsDataSet <- get_results_table(results_data = results_data)$total_points %>% rename("HomeTeam" = Team) %>% select(HomeTeam, Season, Pos)
  
  dataSetToUse <- merge(dataSetToUse, totalPointsDataSet, by = c("Season", "HomeTeam")) %>% arrange(DateTime)
  
  dataSetToUse <- dataSetToUse %>% select(-DateTime)
  

  colsToRemove <- c("FTHG", "FTAG", "HTHG", "HTAG", "HTR", "Referee")
  
  dataSetToUse <- dataSetToUse %>% select(-all_of(colsToRemove))
  
  full_time_results <-  dplyr::case_when(dataSetToUse$FTR == "H" ~ 1,
                                         dataSetToUse$FTR == "D" ~ 0.5,
                                         dataSetToUse$FTR == "A" ~ 0)
  
  dataSetToUse <- dataSetToUse %>%
    mutate(FTR = full_time_results)
  
  field_types<<-FieldTypes(dataSetToUse)
  
  # Determine if NUMERIC fields are DISCREET or ORDINAL
  field_Types_Discreet_Ordinal<- discreetNumeric(dataSetToUse,field_types,DISCREET_BINS)
  discreet_fields <- names(dataSetToUse)[field_Types_Discreet_Ordinal=="DISCREET"]
  
  results<-data.frame(field=names(dataSetToUse),initial=field_types,types1=field_Types_Discreet_Ordinal)
  print(formattable::formattable(results))
  
  # Discreet subset 
  discreetDataset<-dataSetToUse[,which(field_Types_Discreet_Ordinal==TYPE_DISCREET)]
  
  # Ordinals subset
  ordinals<-dataSetToUse[,which(field_Types_Discreet_Ordinal==TYPE_ORDINAL)]
  
  # Test if any ordinals are outliers and replace with mean values
  ordinalsDataset <- outlier(ordinals = ordinals, OUTLIER_CONFIDENCE)
  
  to_be_tested <- cbind(discreetDataset, ordinalsDataset)
  
  variances<-apply(to_be_tested, 2, var)
  namesToREmove <- names(variances[which(variances<=0)])
  dataBeforeNormalisation <<- to_be_tested %>% select(-all_of(namesToREmove))
  
  # Calculate correlation matrix
  corMatrix <- cor(dataBeforeNormalisation)
  
  # find attributes that are highly corrected
  highlyCorrelated <- findCorrelation(corMatrix, CUTOFF)
  
  #names of highly correlated fields
  highlyCorCol <- colnames(dataBeforeNormalisation)[highlyCorrelated]
  print("Removing the following columns due to high correlation")
  print(highlyCorCol)
  
  # remove highly correlated fields
  dataForNormalisation <- dataBeforeNormalisation[, !(names(dataBeforeNormalisation) %in% highlyCorCol)]
  dim(dataForNormalisation)
  
  
  # Symbolic subset
  symbolicDatasetForOHE<-dataSetToUse[,which(field_types==TYPE_SYMBOLIC)]
  
  # # One hot encode the following fields
  # fieldsForEncoding <- c("HomeTeam", "AwayTeam")
  # oneHotDataset <- oneHotEncoding(dataset = symbolicDatasetForOHE, fieldsForEncoding =fieldsForEncoding)
  
  # Combine symbolic and numeric datasets
  dataBeforeNormalisation<-cbind(dataForNormalisation,symbolicDatasetForOHE)
  
  normalisedDataset <- dataBeforeNormalisation
  
  # normalisedDataset$FTR <- as.factor(normalisedDataset$FTR)
  
  
  
  return(normalisedDataset)
}
