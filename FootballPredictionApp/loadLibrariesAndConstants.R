# List of libraries used in the project to be installed and loaded

library("shiny")
library("DT")
library("stats")
library("scales")
library("stats")
library("shinythemes")
library("tidyverse")
library("ggridges")
library("gridExtra")
library("extrafont")
library("keras")
library("tensorflow")
library("formattable")
library("tidyrules")
library("outliers")
library("ggplot2")
library("ggpubr")
library("caret")
library("hrbrthemes")
library("dplyr")
library("shiny")
library("shinydashboard")

# constants to be used for this project
OUTPUT_FIELD      <- "FTR"             # Field name of the output class to predict

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded
DISCREET_BINS     <- 5                    # Number of Discreet Bins Required for 
OUTLIER_CONFIDENCE <- 0.99                # Confidence of discreet 
CUTOFF            <- 0.90                 # Correlation cutoff
FREQCUT           <- 99/1                 # To remove zero variance fields

K_FOLDS           <- 8                    # Number of holds for stratified cross validation

LOAD_MODEL        <- F
FOREST_SIZE       <- 1000  

# Source the relevant files
source("adjust_tranfers.R")
source("get_tables.R")
source("getNormalisedDatasets.R")
source("importantFunctions.R")
source("merge_transfers_and_table.R")
source("dataPreprocessing_NN.R")
source("get_averages_for_shiny_app.R")
source("tablesForShinyApp/plotsForShinyApp.R")
source("tablesForShinyApp/resultsTable.R")
source("tablesForShinyApp/loadModels.R")

# Datasets neccesary
results_data       <- read.csv("newData/results_new.csv")
transfer_data_df   <- read.csv("newData/transfer_data_new.csv")
transfer_inflation <- read.csv("newData/transfer_inflation_new.csv")
teams_stadiums <- read.csv("newData/teams_stadiums.csv")
