
# ************************************************
# adjust_transfers_money() :
# adjust money to todays money
#
# INPUT       :   dataset - transfer_inflation - inflation dataset
#                 dataset - transfer_data_df - transfer dataset 
#
# OUTPUT      :   list - list containing dataframes of transfer data
# ************************************************
adjust_transfers_money <- function(transfer_inflation, transfer_data_df){
  
  return_list <- list()
  
  #changing season names 
  transfer_data_df$Season <- rep(2001:2020, each = 20)
  transfer_inflation$Season <- seq(from = 2020, to = 2001, by = -1)
  
  remove_percentage <- function(x){
    x <- substr(x,1, nchar(x)-1)
    x <- as.numeric(x)
    return(x)
  }
  
  transfer_data <- create_transfer_data(transfers_data = transfer_data_df)
  
  return_list$transfer_data_no_inflation <- transfer_data
  
  convert_to_number <- function(x){
    x <- substr(x, 2, nchar(x)-1)
    x <- as.numeric(x)
    x <- x * 1000000
    return(x)
  }
  
  transfer_data$Expenditure <- convert_to_number(as.character(transfer_data$Expenditure))
  transfer_data$Income <- convert_to_number(as.character(transfer_data$Income))
  transfer_data$Balance <- convert_to_number(as.character(transfer_data$Balance))
  
  return_list$transfers_without_pound <- transfer_data

  transfer_inflation_data <- merge(transfer_data, transfer_inflation, by = "Season")
  
  return_list$updated_transfer_data <- transfer_inflation_data %>%
    mutate("Expenditure" = Expenditure * Increase_Factor,
           "Income"      = Income * Increase_Factor,
           "Balance"     = Balance * Increase_Factor) %>%
    select("Season", "Team", "Expenditure", "Arrivals", "Income", "Departures", "Balance")
  
  return_list$updated_transfers_for_display <- return_list$updated_transfer_data
  
  return_list$updated_transfers_for_display$Expenditure <- dollar(return_list$updated_transfers_for_display$Expenditure)
  return_list$updated_transfers_for_display$Balance <- dollar(return_list$updated_transfers_for_display$Balance)
  return_list$updated_transfers_for_display$Income <- dollar(return_list$updated_transfers_for_display$Income)
  
  
  return(return_list)
  
}
