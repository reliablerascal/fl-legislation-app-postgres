# APP.R
# June and July 2024 RR
#
# This is a Shiny app for the Jacksonville Tributary's legislative dashboard
# my version at https://mockingbird.shinyapps.io/fl-leg-app-postgres/
# original version by Andrew Pantazi at https://shiny.jaxtrib.org/ 


library(shiny)
library(dplyr)
library(plotly)
library(foreach)
library(profvis)
library(data.table)
library(jsonlite)
library(lubridate)
library(forcats)
library(stringr)
library(purrr)
library(readr)
library(tidyr)
library(tibble)
library(ggplot2)
library(tidyverse)
library(DBI)
library(RPostgres)
library(scales)


### After re-opening this project...
### Run this in the console. second line is needed if there's new or updated data
source("read_data.R") # prior to running the app offline
source("save_data.R") # prior to uploading the app to Shiny (saves as RDS = relational data service)

### set up dataframes
all_data <- readRDS("data/all_data.rds")

app_vote_patterns <- all_data$app_vote_patterns
app_data <- all_data$app_data
jct_bill_categories <- all_data$jct_bill_categories

source("ui.R")


########################
#                      #  
# SERVER               #
#                      #
########################
# Loads and preprocesses data.
# Handles server-side logic, including reactive expressions and observers.
# Executes data queries and manipulations.
# Generates outputs based on user inputs and updates the UI accordingly.
# Adapted from Andrew's code- modularized, getting data from Postgres (vs. .RData), U/X improvements

#local = TRUE ensures each sourced file has access to input/output/session
server <- function(input, output, session) {
  source("server1b_partisanship.R", local = TRUE)
  source("server2_leg_activity.R", local = TRUE)
  
  #source("server1_partisanship.R", local = TRUE)
  #source("server1b_partisanship.R", local = TRUE)
  #source("server3_district_context.R", local = TRUE)
}

########################
#                      #  
# SHINY                #
#                      #
########################
shinyApp(ui, server)
