#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# require(devtools)
# install_version("h2o", version = "3.32.0.1", repos = "http://cran.us.r-project.org")

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(DT)
library(leaflet)
library(shinyBS)
#library(h2o)

source("get_tables.R")
source("tablesForShinyApp/resultsTable.R")
results_data       <- read.csv("newData/results_new.csv")
transfer_data_df   <- read.csv("newData/transfer_data_new.csv")
transfer_inflation <- read.csv("newData/transfer_inflation_new.csv")
teams_stadiums <- read.csv("newData/teams_stadiums.csv")

resultsForShiny <- shinyResultsTable(results_data = results_data)
finalTables <- get_results_table(results_data = results_data)$total_points
# beforeNormalisedData <- preprocessing(results = results_data, transfer_data_df = transfer_data_df, transfer_inflation = transfer_inflation)


shinyUI(
    dashboardPage(title = "Football Prediction App", skin  = "red",
                  dashboardHeader(title = "Football Prediction App"),
                  dashboardSidebar(
                      sidebarMenu(
                          menuItem("Team Page", tabName = "teampage", icon = icon("users")),
                          menuItem("Historical Results", tabName = "historical", icon = icon("bar-chart-o")),
                          menuItem("Transfers Page", tabName = "transfers", icon = icon("glyphicon glyphicon-gbp", lib = "glyphicon")),
                          menuItem("Prediction Page", tabName = "predictions",icon = icon("futbol"))
                      )),
                  dashboardBody(
                      tabItems(
                          #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                          tabItem(tabName = "teampage",
                                  fluidRow(
                                      column(6,style = "background-colour: #fff;",
                                             fluidRow(column(12,style = "background-colour: #fff;",
                                                             box(width = 12, title = "Select a team", status = "primary", solidHeader = T,
                                                                 selectizeInput(inputId = "team_selector",
                                                                                label = NULL,
                                                                                choices = (unique(results_data$HomeTeam)),
                                                                                selected = "Arsenal")),
                                                             # fluidRow(
                                                             #   column(12,
                                                             #          p(tags$b('PL Record', style = "font-size: 150%; font-family:Helvetica;")))),
                                                             box(width = 12, title = "Premier League Record", status = "primary", solidHeader = T,
                                                                 column(5, align = "center",
                                                                        htmlOutput('team_career_L')),
                                                                 column(7, align = "center",
                                                                        htmlOutput("team_career_R")),
                                                                 box(width = 12,status = "primary",
                                                                     column(12,
                                                                            
                                                                            textOutput("teamDescription"))),
                                                                 box(width = 12, status = "primary",
                                                                     column(12,
                                                                            uiOutput("wikiLink")))))),
                                      ),
                                      column(6,
                                             
                                             box(title = "Premier League Position Over the Years", status = "info", solidHeader = T, width = 12,
                                                 hr(),
                                                 column(12,
                                                        plotOutput("positions")  )))),
                                  fluidRow(
                                      column(7,
                                             box(width = 12, title = "Premier League Record Overall Record", solidHeader = T, status = "warning",
                                                 DTOutput("teamTable"))),
                                      column(5,
                                             box(width = 12, title = "Map of Teams Stadium", status = "success", solidHeader = T,
                                                 column(12,
                                                        leafletOutput("teamMap")))))) ,
                          #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                          #Historical Page 
                          tabItem("historical",
                                  fluidRow(
                                      column(5,
                                             # fluidRow(
                                             #     column(12, 
                                             #            p(tags$b('Yearly Tournament Leaderboard', style = "font-size: 150%; font-family:Helvetica; ")))),
                                             # hr(),
                                             box(width = 12, title = "Yearly Tournament Leaderboard", status = "primary", solidHeader = T,
                                                 hr(),
                                                 column(4,
                                                        selectInput(inputId = 'year_choice',
                                                                    label = NULL,
                                                                    choices = sort(unique(finalTables$Season), decreasing = T), 
                                                                    selected = 2001)),
                                                 DTOutput("resultsData")
                                             )
                                      ),
                                      column(7,
                                             
                                             box(width = 12,solidHeader = T, title = "Historical Records", status = "danger",
                                                 hr(),
                                                 # column(5, 
                                                 #        p(tags$b('Historical Records', style = "font-size: 150%; font-family:Helvetica;"))),
                                                 column(2, align = 'left',
                                                        selectInput(inputId = 'year_start',
                                                                    label = NULL,
                                                                    choices = seq(2001, 2020, 1), 
                                                                    selected = 2001)),
                                                 column(1, style = 'margin-top: 7px;', align = 'center', p("to")),
                                                 column(2, align = 'left',
                                                        selectInput(inputId = 'year_finish',
                                                                    label = NULL,
                                                                    choices = seq(2001, 2020, 1), 
                                                                    selected = 2020)),
                                                 fluidRow(column(5,
                                                                 selectizeInput(inputId = "tbl_choice", 
                                                                                label = NULL,
                                                                                choices = c("Years in Premier League", "Titles", "Top 4",
                                                                                            "Relegated", "Team Overall"), 
                                                                                selected = "Years In Premier League"))),
                                                 br(),
                                                 DTOutput("historicalTable"))))),
                          #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                          tabItem(tabName = "transfers",
                                  fluidRow(
                                      box(width = 3,title = "Adjust Graphs", status = "primary",solidHeader = T,align = "center",
                                          box(width = 12, status = "primary",
                                              p(tags$b('Select a Team', style = "font-size: 80%; font-family:Helvetica;")),
                                              selectInput(inputId = 'team_chosen',
                                                          label = NULL,
                                                          choices = unique(results_data$HomeTeam), 
                                                          selected = "Arsenal"),
                                              p(tags$b('Select Season Range', style = "font-size: 80%; font-family:Helvetica;")),
                                              selectInput(inputId = 'transfer_start',
                                                          label = NULL,
                                                          choices = seq(2001, 2020, 1), 
                                                          selected = 2001),
                                              selectInput(inputId = 'transfer_end',
                                                          label = NULL,
                                                          choices = seq(2001, 2020, 1), 
                                                          selected = 2020),
                                              selectizeInput(inputId = 'inflation_select',
                                                             label = NULL,
                                                             choices = c("Without Inflation",
                                                                         "With Inflation"), 
                                                             selected = "Without Inflation")
                                          ),
                                          valueBoxOutput("stats1", width = 12),
                                          div(id = "transfer1",
                                              valueBoxOutput("stats2", width = 12)),
                                          bsModal("transferModal", "Average Personel Change Per Season", "transfer1", size = "large", textOutput("transfer1")),
                                          valueBoxOutput("stats3", width = 12)),
                                      column(width=9,
                                             box(width = 12,title = "Transfer Spending Graphs", status = "info", solidHeader = T,
                                                 # column(width = 12, 
                                                 #        p(tags$b('Transfer Spending Graph', style = "font-size: 150%; font-family:Helvetica; "))),
                                                 
                                                 box(width = 12,status = "info",
                                                     tabsetPanel(
                                                         tabPanel("Balance", width = 15,
                                                                  plotOutput("transfer_plots_balance")),
                                                         tabPanel("Income", width = 6,
                                                                  plotOutput("transfer_plots_income")),
                                                         tabPanel("Expenditure", width = 6,
                                                                  plotOutput("transfer_plots_expenditure"))
                                                     )
                                                 )),
                                             
                                             # fluidRow(
                                             #     column(8, 
                                             #            p(tags$b('Transfer Spending Stats', style = "font-size: 150%; font-family:Helvetica; ")))),
                                             # hr(),
                                             box(width = 12,title = 'Transfer Spending Stats', status = "success", solidHeader = T, align = "center",
                                                 column(5,align ="center",
                                                        htmlOutput("transfer_stats_L")),
                                                 column(7,align = "center",
                                                        htmlOutput("transfer_stats_R"))))
                                  )
                          ),
                          #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                          tabItem(tabName = "predictions",
                                  fluidRow(
                                      column(9,
                                             box(width = 12,title = "Premier League Match Outcome Prediction", status = "primary", solidHeader = T,
                                                 tabsetPanel(id = 'tabs',
                                                             tabPanel("With Transfer Data", id = 'tab1',
                                                                      column(12,
                                                                             hr(),
                                                                             column(3,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Select Home Team:', style = "font-size: 120%; font-family:Helvetica; "))),
                                                                             column(2,align = 'center',style = 'background-color:#fff;',
                                                                                    selectInput(inputId = 'team_chosen_h',
                                                                                                label = NULL,
                                                                                                choices = unique(results_data$HomeTeam), 
                                                                                                selected = "Arsenal")),
                                                                             column(3,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Select Away Team:', style = "font-size: 120%; font-family:Helvetica; "))),
                                                                             column(2,align = 'center',style = 'background-color:#fff;',
                                                                                    selectInput(inputId = 'team_chosen_a',
                                                                                                label = NULL,
                                                                                                choices = unique(results_data$HomeTeam), 
                                                                                                selected = "Southampton")),
                                                                             column(2,align = 'center',style = 'background-color:#fff;',
                                                                                    actionButton(inputId = "calculateWith", label = "Predict Result",
                                                                                                 style = "color: #fff; background-color: #000201; border-color:#2e6da4"))
                                                                      ),
                                                                      column(12,style = 'background-color:#fff;',
                                                                             hr(),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Select Home Team Expenditure:', style = "font-size: 120%; font-family:Helvetica; "))),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
                                                                                    sliderInput(inputId = "homeExp", "Home Team Expenditure (£m)", 
                                                                                                min = 0, max = 200, value = 100))),
                                                                      column(12,style = 'background-color:#fff;',
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Select Away Team Expenditure:', style = "font-size: 120%; font-family:Helvetica; "))),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
                                                                                    sliderInput(inputId = "awayExp", "Away Team Expenditure (£m)", 
                                                                                                min = 0, max = 200, value = 100))),
                                                                      column(12,style = 'background-color:#fff;',
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Select Home Team Income:', style = "font-size: 120%; font-family:Helvetica; "))),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: red}")),
                                                                                    sliderInput(inputId = "homeInc", "Home Team Income (£m)", 
                                                                                                min = 0, max = 200, value = 100))),
                                                                      column(12,style = 'background-color:#fff;',
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Select Away Team Income:', style = "font-size: 120%; font-family:Helvetica; "))),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: green}")),
                                                                                    sliderInput(inputId = "awayInc", "Away Team Income (£m)", 
                                                                                                min = 0, max = 200, value = 100))),
                                                                      hr(),
                                                                      column(12,align = 'center',style = 'background-color:#fff;',
                                                                             hr(),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Model Match Prediction:', style = "font-size: 150%; font-family:Helvetica; "))),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    textOutput("winnerWithTransfer") %>% withSpinner(color="#0dc5c1"),
                                                                                    tags$head(tags$style("#winnerWithTransfer{color: black;
                                                                                                 font-size: 20px;
                                                                                                 font-style:italic;}"))
                                                                             ))
                                                                      
                                                                      
                                                                      
                                                                      
                                                             ),
                                                             tabPanel("Without Transfer Data", id = 'tab2',
                                                                      column(12,style = 'background-color:#fff;',
                                                                             hr(),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Select Home Team:', style = "font-size: 150%; font-family:Helvetica; "))),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    selectInput(inputId = 'team_chosen_home',
                                                                                                label = NULL,
                                                                                                choices = unique(results_data$HomeTeam), 
                                                                                                selected = "Arsenal")),
                                                                             br()),
                                                                      column(12,style = 'background-color:#fff;',
                                                                             hr(),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Select Away Team:', style = "font-size: 150%; font-family:Helvetica; "))),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    selectInput(inputId = 'team_chosen_away',
                                                                                                label = NULL,
                                                                                                choices = unique(results_data$HomeTeam), 
                                                                                                selected = "Southampton"))),
                                                                      column(12,align = 'center',style = 'background-color:#fff;',
                                                                             hr(),
                                                                             column(12,align = 'center',style = 'background-color:#fff;',
                                                                                    actionButton("calculateWithout", "Predict Result", 
                                                                                                 style = "color: #fff; background-color: #000201; border-color:#2e6da4"
                                                                                    )
                                                                                    
                                                                             )),
                                                                      column(12,align = 'center',style = 'background-color:#fff;',
                                                                             hr(),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    p(tags$b('Model Match Prediction:', style = "font-size: 150%; font-family:Helvetica; "))),
                                                                             column(6,align = 'center',style = 'background-color:#fff;',
                                                                                    textOutput("winnerWithoutTransfer")  %>% withSpinner(color="#0dc5c1"),
                                                                                    tags$head(tags$style("#winnerWithoutTransfer{color: black;
                                                                                                 font-size: 20px;
                                                                                                 font-style:italic;}")),
                                                                                    br()
                                                                             ))
                                                                      
                                                                      
                                                             )
                                                 ))),
                                      
                                      column(3,
                                             box(width = 12,title = "Model Metrics", solidHeader = T,status = "danger",
                                                 
                                                 p(tags$b('Click on a Box for Definition and Information', style = "font-size: 100%; font-family:Helvetica; ")),
                                                 div(id = "clickdiv1",
                                                     valueBoxOutput("stats11", width = 12)
                                                 ),
                                                 bsModal("modalAcc", "Model Accuracy", "clickdiv1", size = "large", textOutput("accuracy")),
                                                 
                                                 div(id = "clickdiv2",
                                                     valueBoxOutput("stats22", width = 12)),
                                                 bsModal("modalPre", "Model Precision", "clickdiv2", size = "large", textOutput("precision")),
                                                 
                                                 div(id = "clickdiv3",
                                                     valueBoxOutput("stats33", width = 12)),
                                                 bsModal("modalRec", "Model Recall", "clickdiv3", size = "large", textOutput("recall")),
                                                 
                                                 div(id = "clickdiv4",
                                                     valueBoxOutput("stats44", width = 12)),
                                                 bsModal("modalF1", "Model F1-Score", "clickdiv4", size = "large", textOutput("f1")),
                                                 
                                                 div(id = "clickdiv5",
                                                     valueBoxOutput("stats55", width = 12)),
                                                 bsModal("modalR2", "Model R-Squared", "clickdiv5", size = "large", textOutput("r2")),
                                                 
                                                 div(id = "clickdiv6",
                                                     valueBoxOutput("stats66", width = 12)),
                                                 bsModal("modalRMSE", "Model RMSE", "clickdiv6", size = "large", textOutput("rmse"))))
                                  ))
                          
                      )
                  )
    )
)
