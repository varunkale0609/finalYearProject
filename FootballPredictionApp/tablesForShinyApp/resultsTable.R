# ************************************************
# shinyResultsTable() :
# table of results for the UI 
#
# INPUT       :   dataframe - results_data - results information for the team.
#
# OUTPUT      :   dataframe - table of results for UI
# ************************************************
shinyResultsTable <- function(results_data){
  
  resultsForShiny <- results_data %>%
    mutate(Result = paste(FTHG, " - ", FTAG)) %>%
    rename("Date" = "DateTime") %>%
    select(Date, HomeTeam, AwayTeam, Result, Referee)
  
  resultsForShiny$Date <- as.Date(resultsForShiny$Date, format = "%d/%m/%Y")
  
  return(resultsForShiny)
}

# ************************************************
# yearlyLeaderBoard() :
# yearly table of results for the UI 
#
# INPUT       :   dataframe - results_data - results information for the team.
#                 character - year - year for the transfer data
#
# OUTPUT      :   dataframe - yearly table of results for UI
# ************************************************
yearlyLeaderBoard <- function(results_data, year){
  
  get_overall_table <- get_results_table(results_data)$total_points
  
  return_table <- get_overall_table %>%
    filter(Season == year)%>%
    select(Team, Season, Total_Points, Wins, Draws, Loses)%>%
    rename("Points" = "Total_Points",
           "W" = "Wins",
           "L" = "Loses",
           "D" = "Draws")
  
  return(return_table)
}

# ************************************************
# historicalTable() :
# historical table of results for the UI 
#
# INPUT       :   dataframe - results_data - results information
#                 character - seasons - seasons for the results data
#                 character - wanted - the table display wanted
#
# OUTPUT      :   dataframe - results table
# ************************************************
historicalTable <- function(results_data, seasons, wanted){
  
  get_overall_table <- get_results_table(results_data)$total_points
  
  if(wanted == 'Years in Premier League'){
    return_table <- get_overall_table %>%
      filter(between(Season, seasons[1], seasons[2])) %>%
      group_by(Team) %>%
      summarise(Tournaments      = n(),
                Matches          = Tournaments * 38,
                First            = min(Season),
                Latest           = max(Season),
                Highest          = min(Pos),
                Lowest           = max(Pos)) %>%
      arrange(desc(Tournaments))
  } else if(wanted == "Titles"){
    return_table <- get_overall_table %>%
      filter(between(Season, seasons[1], seasons[2]),
             Pos == 1) %>%
      group_by(Team) %>%
      summarise(Titles  = n(),
                Seasons = list(Season)) %>%
      mutate(Seasons = map_chr(Seasons, ~ str_c(sort(unlist(.x)), collapse = ", "))) %>%
      arrange(desc(Titles))
  } else if (wanted == "Top 4"){
    return_table <- get_overall_table %>%
      filter(between(Season, seasons[1], seasons[2]),
             Pos <= 4) %>%
      group_by(Team) %>%
      summarise(Top_4   = n(),
                Seasons = list(Season)) %>%
      mutate(Seasons = map_chr(Seasons, ~ str_c(sort(unlist(.x)), collapse = ", "))) %>%
      arrange(desc(Top_4))
  } else if (wanted == "Relegated"){
    return_table <- get_overall_table %>%
      filter(between(Season, seasons[1], seasons[2]),
             Pos >= 18) %>%
      group_by(Team) %>%
      summarise(Relegated   = n(),
                Seasons     = list(Season)) %>%
      mutate(Seasons = map_chr(Seasons, ~ str_c(sort(unlist(.x)), collapse = ", "))) %>%
      arrange(desc(Relegated))
  } else if (wanted == "Team Overall"){
    return_table <- get_overall_table %>%
      filter(between(Season, seasons[1], seasons[2])) %>%
      group_by(Team) %>%
      summarise(Points = sum(Total_Points),
                Wins   = sum(Wins),
                Draws  = sum(Draws),
                Losses = sum(Loses)) %>%
      arrange(desc(Points))
  }
}

# ************************************************
# specific_team_table() :
# the table for the specific team
#
# INPUT       :   dataframe - results_data - results information
#                 character - team - the team selected
#
# OUTPUT      :   dataframe - the specific teams table
# ************************************************
specific_team_table <- function(results_data, team = "Arsenal"){
  
  list_of_tables <- get_results_table(results_data)$total_points
  
  team_table <- list_of_tables %>%
    filter(Team == team) %>%
    rename("Points" = "Total_Points",
           "W" = "Wins",
           "D" = "Draws",
           "L" = "Loses",
           "GS" = "Goals_Scored",
           "GC" = "Goals_Conceded",
           "GD" = "Goal_Difference") %>%
    select(Pos, Team, Season, Points, W, D, L, GS, GC, GD)
  
  return(team_table)
}

specific_transfer_data_information <- function(transfer_data_df, transfer_inflation, team, seasons, inflation_yes){
  
  transfer_information <- adjust_transfers_money(transfer_inflation = transfer_inflation,
                                                 transfer_data_df   = transfer_data_df)
  
  if(inflation_yes == "With Inflation"){
    transfer_information_01 <- transfer_information$updated_transfer_data
  }else{
    transfer_information_01 <- transfer_information$transfers_without_pound
  }
  
  return_transfer_data <- transfer_information_01 %>%
    filter(between(Season, seasons[1], seasons[2]),
           Team   == team)
  
  return(return_transfer_data)

}

# ************************************************
# specific_transfer_stats() :
# the table for the specific team transfer stats
#
# INPUT       :   dataframe - transfer_info - transfer dataset
#
# OUTPUT      :   list - list of required transfer information
# ************************************************
specific_transfer_stats <- function(transfer_info){
  returnList <- list()
  
  returnList$Seasons <- nrow(transfer_info)
  returnList$Team    <- unique(transfer_info$Team)
  returnList$Expenditure <- round(sum(transfer_info$Expenditure)/1e6, 2)
  returnList$Arrivals <- sum(transfer_info$Arrivals)
  returnList$Income <- round(sum(transfer_info$Income)/1e6, 2)
  returnList$Departures <- sum(transfer_info$Departures)
  returnList$Balance <- paste("£", round(sum(transfer_info$Balance)/1e6, 2), "m")
  
  return(returnList)
  
}
