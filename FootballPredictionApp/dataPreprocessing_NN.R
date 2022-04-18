# ************************************************
# preprocessing() :
# preprocessing for the NN model
#
# INPUT       :   dataframe - results_data - the historical results
#                 dataframe - transfer_data_df - the transfer data
#                 dataframe - transfer_inflation - the transfer inflation rates data
#
# OUTPUT      :   dataframe - pre-processed data frame
# ************************************************
preprocessing <- function(results_data, transfer_data_df, transfer_inflation){
  
  dataSetToUse       <- results_and_transfer_combined(results_data = results_data, transfer_data = transfer_data_df, transfer_inflation = transfer_inflation)
  dataSetToUse$DateTime <- as.Date(dataSetToUse$DateTime, format = "%d/%m/%Y")
    
  totalPointsDataSet <- get_results_table(results_data = results_data)$total_points %>% rename("HomeTeam" = Team) %>% select(HomeTeam, Season, Pos)
  
  dataSetToUse_01 <- merge(dataSetToUse, totalPointsDataSet, by = c("Season", "HomeTeam")) %>% arrange(DateTime)
  
  dataSetToUse_02 <<- dataSetToUse_01 %>% select(-DateTime)
  
  colsToRemove <- c("FTHG", "FTAG", "HTHG", "HTAG", "HTR", "Referee")
  
  dataSetToUse_03 <- dataSetToUse_02 %>% select(-all_of(colsToRemove))
  
  full_time_results <-  dplyr::case_when(dataSetToUse_03$FTR == "H" ~ 2,
                                         dataSetToUse_03$FTR == "D" ~ 1,
                                         dataSetToUse_03$FTR == "A" ~ 0)
  
  dataSetToUse_04 <<- dataSetToUse_03 %>%
    mutate(FTR = full_time_results)
  
  field_types<<-FieldTypes(dataSetToUse_04)
  
  # Determine if NUMERIC fields are DISCREET or ORDINAL
  field_Types_Discreet_Ordinal<- discreetNumeric(dataSetToUse_04,field_types,DISCREET_BINS)
  discreet_fields <- names(dataSetToUse_04)[field_Types_Discreet_Ordinal=="DISCREET"]
  
  results<-data.frame(field=names(dataSetToUse_04),initial=field_types,types1=field_Types_Discreet_Ordinal)
  print(formattable::formattable(results))
  
  # Discreet subset 
  discreetDataset<-dataSetToUse_04[,which(field_Types_Discreet_Ordinal==TYPE_DISCREET)]
  
  # Ordinals subset
  ordinals<-dataSetToUse_04[,which(field_Types_Discreet_Ordinal==TYPE_ORDINAL)]
  
  # Test if any ordinals are outliers and replace with mean values
  ordinalsDataset <- outlier(ordinals = ordinals, OUTLIER_CONFIDENCE)
  
  # Symbolic subset
  symbolicDatasetForOHE<<-dataSetToUse_04[,which(field_types==TYPE_SYMBOLIC)]
  
  # One hot encode the following fields
  fieldsForEncoding <- c("HomeTeam", "AwayTeam")
  oneHotDataset <- oneHotEncoding(dataset = symbolicDatasetForOHE, fieldsForEncoding =fieldsForEncoding)
  
  # Combine symbolic and numeric datasets
  dataBeforeNormalisation<-cbind(ordinalsDataset,discreetDataset,oneHotDataset)
  
  # Make sure there are no NA's
  any(is.na(dataBeforeNormalisation))
  if(all(sapply(dataBeforeNormalisation, is.numeric ))){
    print("All Fields Are Numeric")
  }
  
  variances<-apply(dataBeforeNormalisation, 2, var)
  namesToREmove <- names(variances[which(variances<=0)])
  dataBeforeNormalisation <<- dataBeforeNormalisation %>% select(-all_of(namesToREmove))
  
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
  
  # normalise dataset using function above
  #normalisedDataset <- as.data.frame(lapply(dataForNormalisation,normalise))
  
  normalisedDataset <- dataForNormalisation
 
 
  
  return(normalisedDataset)
}
