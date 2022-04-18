library(ggplot2)
library(leaflet)


# ************************************************
# plot_positions() :
# plot the positions the teams have finished each season
# for the UI
#
# INPUT       :   dataframe - results_data - the results data
#                 character - team - team name
#
# OUTPUT      :   ggplot - plot of the team positions.
# ************************************************
plot_positions <- function(results_data, team){
  
  get_overall_table <- get_results_table(results_data)$total_points %>%
    filter(Team == team)
  
  ggplot(get_overall_table, aes(x = Season, y = Pos)) +
    #geom_line(colour = "#69b3a2") + 
    geom_point(shape = 21, color = factor(get_overall_table$Pos), size = 3, fill = factor(get_overall_table$Pos))+ 
    geom_text(aes(label = Pos),hjust =2, vjust = 0 )+
    geom_hline(yintercept = 4, colour = "blue")+
    geom_hline(yintercept = 18, colour = "red")+
    xlim(2001, 2020)+
    ylim(1,20)+
    theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous("Season", labels = as.character(get_overall_table$Season), breaks = get_overall_table$Season)
  
}

# ************************************************
# plot_ground() :
# plot the positions the teams have finished each season
# for the UI
#
# INPUT       :   dataframe - teams_stadium - the stadiums of all the teams.
#                 character - team - team name
#
# OUTPUT      :   leaflet - map of the teams stadiums.
# ************************************************
# teams_stadiums <- read_csv("newData/teams_stadiums.csv")
plot_ground <- function(teams_stadiums, team){
  teams_stadiums <- teams_stadiums %>% filter(Team == team)
  
  m <- leaflet(teams_stadiums) %>%
    addTiles() %>%
    addMarkers(lat = ~Lat, lng = ~Lon,  
               label = paste("Stadium: ", teams_stadiums$Ground, "| Capacity: ", teams_stadiums$Capacity ),
               options = popupOptions(closeButton = FALSE) )
  m
}

teams_information <- function(teams_stadiums, team){
  
  teams_stadiums <- teams_stadiums %>% filter(Team == team)
  
  return(teams_stadiums$Description)
}

# ************************************************
# transfer_plots() :
# plots the transfer information for the UI 
#
# INPUT       :   dataframe - transfer_information_return - transfer information for the team.
#                 character - graph - the graph to display
#
# OUTPUT      :   ggplot - plot of either the expenditure, balance or income for the team
# ************************************************
transfer_plots <- function(transfer_information_return, graph = c("Balance", "Expenditure", "Income")){
  
  if(graph == "Balance"){
    
    transfer_information_return %>% mutate(Color = ifelse(Balance > 0, "green", "red")) %>%
    ggplot2::ggplot(aes(Season, Balance/1e6, fill = Color)) + 
      geom_col() +
      ggtitle(paste(unique(transfer_information_return$Team), "Net Balance")) +
      theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
      xlab("Season") +
      ylab("Balance (£m)") +
      scale_y_continuous(labels = scales::comma) +
      geom_text(aes(label = paste0("£", round(Balance/1e6, 2), "m")), size = 3,  position = position_stack(vjust = 0.5), color = "black") +
      scale_fill_identity(guide = FALSE) +
      scale_x_continuous("Season", labels = as.character(transfer_information_return$Season), breaks = transfer_information_return$Season)
  
  }else if (graph == "Expenditure"){
      
    ggplot2::ggplot(data = transfer_information_return, aes(Season, Expenditure/1e6)) + 
      geom_col(fill = c("red")) +
      ggtitle(paste(unique(transfer_information_return$Team), "Expenditure")) +
      theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
      xlab("Season") +
      ylab("Expenditure (£m)") +
      scale_y_continuous(labels = scales::comma) +
      geom_text(aes(label = paste0("£", round(Expenditure/1e6, 2), "m")), size = 3,  position = position_stack(vjust = 0.5), color = "black") +
      scale_x_continuous("Season", labels = as.character(transfer_information_return$Season), breaks = transfer_information_return$Season)
    
  } else if(graph == "Income"){
    
    ggplot2::ggplot(data = transfer_information_return, aes(Season, Income/1e6)) + 
      geom_col(fill = c("green")) +
      ggtitle(paste(unique(transfer_information_return$Team), "Income")) +
      theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
      xlab("Season") +
      ylab("Income (£m)") +
      scale_y_continuous(labels = scales::comma) +
      geom_text(aes(label = paste0("£", round(Income/1e6, 2), "m")), size = 3,  position = position_stack(vjust = 0.5), color = "black") +
      scale_x_continuous("Season", labels = as.character(transfer_information_return$Season), breaks = transfer_information_return$Season)
    
    }
  
}
