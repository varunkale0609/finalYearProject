
# ************************************************
# create_transfer_data() :
# change the names of the teams to match the results data
#
# INPUT       :   dataframe - transfers_data - the transfer data
#
# OUTPUT      :   dataframe - containing the transfer data with the new names
# ************************************************
create_transfer_data <- function(transfers_data){
  
  #renaming the teams, wish there was an easier way
  transfers_data$Team <- gsub("Arsenal FC", "Arsenal", transfers_data$Team)
  transfers_data$Team <- gsub("Liverpool FC", "Liverpool", transfers_data$Team)
  transfers_data$Team <- gsub("West Ham United", "West Ham", transfers_data$Team)
  transfers_data$Team <- gsub("Leicester City", "Leicester", transfers_data$Team)
  transfers_data$Team <- gsub("Fulham FC", "Fulham", transfers_data$Team)
  transfers_data$Team <- gsub("Birmingham City", "Birmingham", transfers_data$Team)
  transfers_data$Team <- gsub("Reading FC", "Reading", transfers_data$Team)
  transfers_data$Team <- gsub("Blackpool FC", "Blackpool", transfers_data$Team)
  transfers_data$Team <- gsub("AFC Bournemouth", "Bournemouth", transfers_data$Team)
  transfers_data$Team <- gsub("Leeds United", "Leeds", transfers_data$Team)
  transfers_data$Team <- gsub("Charlton Athletic", "Charlton", transfers_data$Team)
  transfers_data$Team <- gsub("Sunderland AFC", "Sunderland", transfers_data$Team)
  transfers_data$Team <- gsub("Ipswich Town", "Ipswich", transfers_data$Team)
  transfers_data$Team <- gsub("Blackburn Rovers", "Blackburn", transfers_data$Team)
  transfers_data$Team <- gsub("Portsmouth FC", "Portsmouth", transfers_data$Team)
  transfers_data$Team <- gsub("Wigan Athletic", "Wigan", transfers_data$Team)
  transfers_data$Team <- gsub("Stoke City", "Stoke", transfers_data$Team)
  transfers_data$Team <- gsub("Queens Park Rangers", "QPR", transfers_data$Team)
  transfers_data$Team <- gsub("Brighton & Hove Albion", "Brighton", transfers_data$Team)
  transfers_data$Team <- gsub("Chelsea FC", "Chelsea", transfers_data$Team)
  transfers_data$Team <- gsub("Newcastle United", "Newcastle", transfers_data$Team)
  transfers_data$Team <- gsub("Manchester City", "Man City", transfers_data$Team)
  transfers_data$Team <- gsub("Derby County", "Derby", transfers_data$Team)
  transfers_data$Team <- gsub("Bradford City", "Bradford", transfers_data$Team)
  transfers_data$Team <- gsub("Bolton Wanderers", "Bolton", transfers_data$Team)
  transfers_data$Team <- gsub("Wolverhampton Wanderers", "Wolves", transfers_data$Team)
  transfers_data$Team <- gsub("Hull City", "Hull", transfers_data$Team)
  transfers_data$Team <- gsub("Swansea City", "Swansea", transfers_data$Team)
  transfers_data$Team <- gsub("Huddersfield Town", "Huddersfield", transfers_data$Team)
  transfers_data$Team <- gsub("Everton FC", "Everton", transfers_data$Team)
  transfers_data$Team <- gsub("Tottenham Hotspur", "Tottenham", transfers_data$Team)
  transfers_data$Team <- gsub("Coventry City", "Coventry", transfers_data$Team)
  transfers_data$Team <- gsub("Manchester United", "Man United", transfers_data$Team)
  transfers_data$Team <- gsub("Southampton FC", "Southampton", transfers_data$Team)
  transfers_data$Team <- gsub("West Bromwich Albion", "West Brom", transfers_data$Team)
  transfers_data$Team <- gsub("Norwich City", "Norwich", transfers_data$Team)
  transfers_data$Team <- gsub("Watford FC", "Watford", transfers_data$Team)
  transfers_data$Team <- gsub("Burnley FC", "Burnley", transfers_data$Team)
  transfers_data$Team <- gsub("Cardiff City", "Cardiff", transfers_data$Team)
  transfers_data$Team <- gsub("Middlesbrough FC", "Middlesbrough", transfers_data$Team)
  
  return(transfers_data)
  
}

# ************************************************
# results_and_transfer_combined() :
# combine the transfers data and results table
#
# INPUT       :   dataframe - results_data - the results data
#                 dataframe - transfer_data - the transfer data
#                 dataframe - transfer_inflation - the transfer inflation data
#
# OUTPUT      :   dataframe - containing both the results and transfer dataset
# ************************************************
results_and_transfer_combined <- function(results_data, transfer_data, transfer_inflation){
  #putting together transfer data and results data
  
  results_data$Season <- rep(2001:2020, each = 380)
  
  # calling adjust function for inflation and getting table with numeric values
  transfer_data <- adjust_transfers_money(transfer_inflation, transfer_data)
  transfer_data_01 <- transfer_data$updated_transfer_data
  
  # renaming column so I can add to final table
  transfer_data_02 <- transfer_data_01 %>%
    dplyr::rename("HomeTeam" = "Team")
  
  # converting to factor - doesnt work without this for some reason
  transfer_data_02$HomeTeam <- as.factor(transfer_data_02$HomeTeam)
  
  #add in the home teams transfers
  combined <- results_data %>%
    left_join(transfer_data_02, by = c("Season", "HomeTeam")) %>%
    select(-c(Expenditure, Arrivals, Income, Departures)) %>%
    dplyr::rename("HomeNetSpend" = "Balance")
  
  # do the same steps for the away team
  transfer_data_03 <- transfer_data_02 %>%
    dplyr::rename("AwayTeam" = "HomeTeam")
  
  # the final table with only the net spend column.
  final_table <- combined %>%
    left_join(transfer_data_03, by = c("Season", "AwayTeam")) %>%
    select(-c(Expenditure, Arrivals, Income, Departures)) %>%
    dplyr::rename("AwayNetSpend" = "Balance")
  
  return(final_table)
}