#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#load the H20 package
library(h2o)

h2o.init()
# h2o.shutdown()
# h2o.init(nthreads = -1)

# h2o.removeAll()

library(shiny)
library(shinyjs)
library(shinyBS)
source("loadLibrariesAndConstants.R")

resultsForShiny <- shinyResultsTable(results_data = results_data)
finalTables <- get_results_table(results_data = results_data)$total_points
# models <- loadModels()
beforeNormalisedData <- preprocessing(results = results_data, transfer_data_df = transfer_data_df, transfer_inflation = transfer_inflation)

# Define server logic required to draw a histogram


shinyServer(function(input, output) {


#Historical Tab ---------------------------------------------------------------------------------------------------------
    
    tbl_yearly <- reactive({
        yearlyLeaderBoard(results_data = results_data,
                          year = as.numeric(input$year_choice)) })

    output$resultsData <- renderDT({
        datatable(tbl_yearly(),
                  options = list(info = F,
                                 paging = F,
                                 scrollY = '500px',
                                 searching = F))
    })
    
    tbl_historical <- reactive({
        historicalTable(results_data = results_data,
                        seasons = c(as.numeric(input$year_start), as.numeric(input$year_finish)),
                        wanted = input$tbl_choice)
    })
    
    output$historicalTable <- renderDT({
        datatable(tbl_historical(),
                  options = list(info = F,
                                 paging = F,
                                 scrollY = '500px',
                                 searching = F))
    })

#--------------------------------------------------------------------------------------------------  
# Team Page -------------------------------------------------------------------------------------------
    
    tbl_teamtable <- reactive({
        specific_team_table(results_data = results_data,
                            team = input$team_selector )
    })
    
    output$teamTable <- renderDT({
        datatable(tbl_teamtable(),
                  options = list(info = F,
                                 paging = F,
                                 #scrollY = '500px',
                                 searching = F),rownames = F)
    })
    
    output$team_career_L <- renderPrint({ 
        
                
        teamData <- finalTables %>% filter(Team == input$team_selector)
        #teamData$Pos <- as.integer(teamData$Pos)
        
        p(tags$b("Tournaments: ", style = "font-size: 101%; font-family:Helvetica; "),tags$em(nrow(teamData)), br(),
          tags$b("Titles: ", style = "font-size: 101%; font-family:Helvetica; "), tags$em(nrow(teamData %>% filter(Pos == 1))), br(),
          tags$b("Top 4's: ", style = "font-size: 101%; font-family:Helvetica; "),tags$em(nrow(teamData %>% filter(Pos <= 4))),  br(),
          tags$b("Times Relegated: ", style = "font-size: 101%; font-family:Helvetica; "),tags$em(nrow(teamData %>% filter(Pos >= 18))), br())
    })
    
    output$team_career_R <- renderPrint({
        teamData <- finalTables %>% filter(Team == input$team_selector)
        p(tags$b("Wins: ", style = "font-size: 101%; font-family:Helvetica; "),tags$em(sum(teamData$Wins)), br(),
          tags$b("Draws: ", style = "font-size: 101%; font-family:Helvetica;"),tags$em(sum(teamData$Draws)), br(),
          tags$b("Losses: ", style = "font-size: 101%; font-family:Helvetica;"),tags$em(sum(teamData$Loses)), br(),
          tags$b("Goals: ", style = "font-size: 101%; font-family:Helvetica;"), tags$em(sum(teamData$Goals_Scored)),br()) 
        
    })
    
    plt_pos <- reactive({
        plot_positions(results_data = results_data,
                       team         = input$team_selector )
    })
    
    output$positions <- renderPlot({
        plt_pos()
    })
    
    map_of_team <- reactive({
        plot_ground(teams_stadiums = teams_stadiums,
                    team = input$team_selector)
    })
    
    output$teamMap <- renderLeaflet({
        map_of_team()
    })
    
    teams_descriptions <- reactive({
        teams_information(teams_stadiums = teams_stadiums,
                          team = input$team_selector)
    })
    output$teamDescription <- renderText({
        teams_descriptions()
    })
    
    output$wikiLink <- renderUI({
        url <- a(paste0(input$team_selector, " Wikipedia link"), 
                 href = paste0("https://en.wikipedia.org/w/index.php?search=", input$team_selector, "+football&title=Special:Search&profile=advanced&fulltext=1&advancedSearch-current=%7B%7D&ns0=1"),target = "_blank")
        tagList("The team information above was sourced from:", url)
    })
#-------------------------------------------------------------------------------------------------------------------------------------
#Transfers Page
    transfer_plots_balance <- reactive({
        
         transfer_plots(transfer_information_return = specific_transfer_data_information(transfer_data_df = transfer_data_df,
                                                                          transfer_inflation = transfer_inflation,
                                                                          team = input$team_chosen,
                                                                          seasons = c(input$transfer_start, input$transfer_end),
                                                                          inflation_yes = input$inflation_select),
                        graph = "Balance" )
    })
    output$transfer_plots_balance<- renderPlot({
         transfer_plots_balance()


        })

    transfer_plots_expenditure <- reactive({
        transfer_plots(transfer_information_return = specific_transfer_data_information(transfer_data_df = transfer_data_df,
                                                                                        transfer_inflation = transfer_inflation,
                                                                                        team = input$team_chosen,
                                                                                        seasons = c(input$transfer_start, input$transfer_end),
                                                                                        inflation_yes = input$inflation_select),
                       graph = "Expenditure" )
    })
    output$transfer_plots_expenditure<- renderPlot({
        transfer_plots_expenditure()

    })

    transfer_plots_income <- reactive({
        transfer_plots(transfer_information_return = specific_transfer_data_information(transfer_data_df = transfer_data_df,
                                                                                        transfer_inflation = transfer_inflation,
                                                                                        team = input$team_chosen,
                                                                                        seasons = c(input$transfer_start, input$transfer_end),
                                                                                        inflation_yes = input$inflation_select),
                       graph = "Income" )
    })
    output$transfer_plots_income<- renderPlot({
        transfer_plots_income()

    })
    
    get_transfer_stats <- reactive({
        specific_transfer_stats(transfer_info = specific_transfer_data_information(transfer_data_df = transfer_data_df,transfer_inflation = transfer_inflation,
                                                                                   team = input$team_chosen,
                                                                                   seasons = c(input$transfer_start, input$transfer_end),
                                                                                   inflation_yes = input$inflation_select))
        
        
    })
    output$transfer_stats_L <- renderPrint({
        
        p(tags$b("Team: ", style = "font-size: 101%; font-family:Helvetica; "),tags$em(get_transfer_stats()$Team), br(),
          tags$b("Expenditure: ", style = "font-size: 101%; font-family:Helvetica;"),
          tags$em(paste("£",get_transfer_stats()$Expenditure, "m")), br(),
          tags$b("Income: ", style = "font-size: 101%; font-family:Helvetica;"),
          tags$em(paste("£",get_transfer_stats()$Income, "m")), br(),
          tags$b("Balance: ", style = "font-size: 101%; font-family:Helvetica;"), tags$em(get_transfer_stats()$Balance),br())
        
    })
    
    output$transfer_stats_R <- renderPrint({
        
        p(tags$b("Seasons: ", style = "font-size: 101%; font-family:Helvetica; "),tags$em(get_transfer_stats()$Season), br(),
          tags$b("Arrivals: ", style = "font-size: 101%; font-family:Helvetica;"),tags$em(get_transfer_stats()$Arrivals), br(),
          tags$b("Departures: ", style = "font-size: 101%; font-family:Helvetica;"),tags$em(get_transfer_stats()$Departures), br(),
          tags$b("Data: ", style = "font-size: 101%; font-family:Helvetica;"), tags$em(input$inflation_select),br()) 
        
    })
    
    output$stats1 <- renderValueBox({
        valueBox("Avg Expenditure Per Season",
                 value = paste("£",round(get_transfer_stats()$Expenditure/get_transfer_stats()$Season,2), "m"), 
                 icon = icon("glyphicon glyphicon-gbp", lib = "glyphicon"), color = "blue"
                )
    })
    
    output$stats2 <- renderValueBox({
        valueBox("Avg Team Personel Change Per Season",
                 value = round(get_transfer_stats()$Arrivals/get_transfer_stats()$Season) - 
                     round(get_transfer_stats()$Departures/get_transfer_stats()$Season), 
                 icon = icon("glyphicon glyphicon-plus-sign", lib = "glyphicon"), color = "red"
        )
    })
    
    output$transfer1 <- renderText({
        "This is calculated by the (Total Signings - Total Departures)/Number of Seasons"
    })
    output$stats3 <- renderValueBox({
        valueBox("Avg Income Per Season",
                 value = paste("£",round(get_transfer_stats()$Income/get_transfer_stats()$Season,2), "m"), 
                 icon = icon("glyphicon glyphicon-gbp", lib = "glyphicon"), color = "green"
        )
    })
 
    #})
    
#-------------------------------------------------------------------------------------------------------------------------------------
    #Prediction Page

    get_metrics <- reactive({
        if(input$tabs == "With Transfer Data"){
            readRDS(file = "withTransferMetrics.RDS")
        } else{
            readRDS(file = "withoutTransferMetrics.RDS")
        }
    })
    
    output$stats11 <- renderValueBox({
       
        valueBox("Model Accuracy",
                 value = paste0(round(get_metrics()$Accuracy * 100, 2), "%"), 
                 icon = icon("glyphicon glyphicon-ok-circle", lib = "glyphicon"),
                 color = "blue"
        )
    })
    output$accuracy <- renderText({
        "Accuracy: How often the number of correctly predicted classes (win, draw and loss) in the dataset is predicted correctly"
    })
    
    output$stats22 <- renderValueBox({
        valueBox("Model Precision",
                 value = paste0(round(get_metrics()$Precision, 3) ), 
                 icon = icon("glyphicon glyphicon-fire", lib = "glyphicon"), color = "red"
        )
    })
    
    output$precision <- renderText({
        "Precsion: Calculates what proportion of the predicted positive class data was actually positive. "
    })
    
    output$stats33 <- renderValueBox({
        valueBox("Model Recall",
                 value = paste0(round(get_metrics()$Recall,3)), 
                 icon = icon("glyphicon glyphicon-registration-mark", lib = "glyphicon"), color = "green"
        )
    })
    
    output$recall <- renderText({
        "Recall: Calculates what proportion of the positive data was actually predicted as positive by the classifier."
    })
    
    output$stats44 <- renderValueBox({
        valueBox("Model F1-Score",
                 value = paste0(round(get_metrics()$WeightedF1, 2 )), 
                 icon = icon("glyphicon glyphicon-apple", lib = "glyphicon"), color = "maroon"
        )
    })
    
    output$f1 <- renderText({
        "F1-Score: Calculates what proportion of the positive data was actually predicted as positive by the classifier.
        2*(Precision * Recall)/(Precision + Recall)"
    })
    
    output$stats55 <- renderValueBox({
        valueBox("Model R-Squared",
                 value = paste0(round(get_metrics()$R2, 2)), 
                 icon = icon("glyphicon glyphicon-piggy-bank", lib = "glyphicon"), color = "blue"
        )
    })
    
    output$r2 <- renderText({
        "R-Squared: This can be explained as the percentage of variation in the data. It can also be interpreted as a measure of how close the data points are fitted to the regression line."
    })
    
    
    output$stats66 <- renderValueBox({
        valueBox("Model RMSE",
                 value = paste0(round(get_metrics()$RMSE, 2)), 
                 icon = icon("glyphicon glyphicon-ban-circle", lib = "glyphicon"), color = "orange"
        )
    })
    
    output$rmse <- renderText({
        "RMSE: The Root Means Square Error is a way of measuring the error of a machine learning model. "
    })
    
    predictions <- eventReactive(input$calculateWith,{
        
        predictWithTransfer(dataset = beforeNormalisedData, homeExp = input$homeExp, homeInc = input$homeInc,
                            awayExp = input$awayExp, awayInc = input$awayInc, homeTeam = input$team_chosen_h,
                            awayTeam =input$team_chosen_a, models = models )
        
        
    })
    
    output$winnerWithTransfer <- renderText({
        
       if(predictions()[1] == 0){
           print(predictions())
           paste0("The winner is predicted to be ", input$team_chosen_a, " with ", predictions()[2] *100, "% Confidence")
        }
        else if(predictions()[1] == 0.5){
            print(predictions())
            paste0("The result is predicted to be a draw with ", predictions()[3]*100, "% Confidence")
        }
        else if(predictions()[1] == 1){
            print(predictions())
            paste0("The winner is predicted to be ", input$team_chosen_h, " with ", predictions()[4] *100, "% Confidence")
        }
    
        
    })
    
    predictionsWithout <- eventReactive(input$calculateWithout, {
        predictWithoutTransfer(dataset = beforeNormalisedData, homeTeam = input$team_chosen_home,
                               awayTeam = input$team_chosen_away, models = models )
        
    })
    
    output$winnerWithoutTransfer <- renderText({
        
        if(predictionsWithout()[1] == 0){
            print(predictionsWithout())
            paste0("The winner is predicted to be ", input$team_chosen_away, " with ", predictionsWithout()[2] *100, "% Confidence")
        }
        else if(predictionsWithout()[1] == 0.5){
            print(predictionsWithout())
            paste("The result is predicted to be a draw with ", predictionsWithout()[3]* 100, "% Confidence")
        }
        else if(predictionsWithout()[1] == 1){
            print(predictionsWithout())
            paste0("The winner is predicted to be ", input$team_chosen_home, " with ", predictionsWithout()[4] *100, "% Confidence")
        }
        
    })
    
    })
