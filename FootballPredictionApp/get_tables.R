
# ************************************************
# specific_team_table() :
# the table for the specific team
#
# INPUT       :   dataframe - results_data - the historical results
#                 team - the team for the table
#
# OUTPUT      :   dataframe - the results table filtered for the team
# ************************************************
specific_team_table <- function(results_data, team = "Arsenal"){
  
  list_of_tables <- get_results_table(results_data)$total_points
  
  team_table <- list_of_tables %>%
    filter(Team == team)
  
  return(team_table)
}

# ************************************************
# get_results_table() :
# the results tables for the UI
#
# INPUT       :   dataframe - results_data - the historical results
#
# OUTPUT      :   list - list of dataframes containing different tables
# ************************************************
get_results_table <- function(results_data){

    #results_data <- read.csv("tests/inputData/results_data.csv")
    
    return_list <- list()
    
    #changing season names 
    results_data$Season <- rep(2001:2020, each = 380)
    return_list$results_data <- results_data
    
    #away results table
    return_list$away_points <- results_data %>%
      mutate(Away_Points = case_when(FTR == "A" ~ 3,
                                     FTR == "H" ~ 0,
                                     FTR == "D" ~ 1)) %>% 
      dplyr::select(AwayTeam, Away_Points, Season, FTHG, FTAG) %>%
      arrange(AwayTeam, Season) %>%
      group_by(AwayTeam, Season) %>%
      mutate(total_away_points    = sum(Away_Points),
             away_goals_conceded  = sum(FTHG),
             away_goals_scored    = sum(FTAG),
             away_goal_difference = away_goals_scored - away_goals_conceded,
             away_wins            = sum(Away_Points == 3),
             away_draws           = sum(Away_Points == 1),
             away_lose            = sum(Away_Points == 0)) %>%
      dplyr::select(-c(Away_Points, FTAG, FTHG)) %>%
      distinct() %>%
      arrange(Season, desc(total_away_points))
    
    #home results table
    return_list$home_points <- results_data %>%
      mutate(Home_Points = case_when(FTR == "A" ~ 0,
                                     FTR == "H" ~ 3,
                                     FTR == "D" ~ 1)) %>%
      dplyr::select(HomeTeam, Home_Points, Season, FTHG, FTAG) %>%
      arrange(HomeTeam, Season) %>%
      group_by(HomeTeam, Season) %>%
      mutate(total_home_points    = sum(Home_Points),
             home_goals_scored    = sum(FTHG),
             home_goals_conceded  = sum(FTAG),
             home_goal_difference = home_goals_scored - home_goals_conceded, 
             home_wins            = sum(Home_Points == 3),
             home_draws           = sum(Home_Points == 1),
             home_lose            = sum(Home_Points == 0)) %>%
      dplyr::select(-c(Home_Points, FTAG, FTHG)) %>%
      distinct() %>%
      arrange(Season, desc(total_home_points))
    
    #arrange in alphabetical order
    away_points_alphabetical <- return_list$away_points %>%
      arrange(Season, AwayTeam)
    
    away_point_to_add <- subset(away_points_alphabetical, select = -c(Season))
    
    home_points_to_add <- return_list$home_points %>%
      arrange(Season, HomeTeam)
    
    #total points table
    total_points <- cbind(home_points_to_add, away_point_to_add)
    return_list$total_points <- total_points %>%
      dplyr::arrange(AwayTeam, HomeTeam, Season) %>%
      mutate(Total_Points    = sum(total_home_points, total_away_points),
             Goals_Scored    = away_goals_scored + home_goals_scored,
             Goals_Conceded  = away_goals_conceded + home_goals_conceded,
             Goal_Difference = home_goal_difference + away_goal_difference,
             Wins            = home_wins + away_wins,
             Draws           = home_draws + away_draws,
             Loses           = home_lose + away_lose) %>%
      dplyr::select(HomeTeam, Season, Total_Points, Goals_Scored, Goals_Conceded, Goal_Difference, Wins, Draws, Loses) %>%
      rename(Team = HomeTeam) %>%
      arrange(Season, desc(Total_Points))
    
    #adding league positions
    return_list$away_points <- return_list$away_points %>%
      ungroup() %>%
      mutate(Pos = rep(seq(from = 1,to  = 20, by = 1), 20)) %>%
      dplyr::select(Pos, everything())
    
    return_list$home_points <- return_list$home_points %>%
      ungroup() %>%
      mutate(Pos = rep(seq(from = 1,to  = 20, by = 1), 20)) %>%
      dplyr::select(Pos, everything())
    
    return_list$total_points <- return_list$total_points %>%
      ungroup() %>%
      mutate(Pos = rep(seq(from = 1,to  = 20, by = 1), 20)) %>%
      dplyr::select(Pos, everything())
    
    #overall table for each season
    return_list$years <- split(return_list$total_points,rep(1:20,each=20))
    columnnames <- paste0("Season", seq(from = 2001, to = 2020,  by = 1))
    names(return_list$years) <- columnnames
    return(return_list)
    
}