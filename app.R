# APP.R
# June and July 2024 RR

# This is the dev version of the Shiny app for the Jacksonville Tributary's legislative dashboard
# Web app: https://mockingbird.shinyapps.io/fl-leg-app-postgres/
# Repo: https://github.com/reliablerascal/fl-legislation-app-postgres

# adapted from original version by Andrew Pantazi (see https://github.com/apantazi/legislator_dashboard/tree/main) 


#########################
#                       #  
# Data prep             #
#                       #
######################### 
### Run this in the console after re-opening this project, then comment it out
# source("read_data.R") # prior to running the app offline
# source("save_data.R") # prior to uploading the app to Shiny (saves as RDS = relational data service)

#########################
#                       #  
# The App               #
#                       #
######################### 

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
library(patchwork)



########################
#                      #  
# Data                 #
#                      #
########################
### set up dataframes
all_data <- readRDS("data/all_data.rds")

app01_vote_patterns <- all_data$app01_vote_patterns
app02_leg_activity <- all_data$app02_leg_activity
jct_bill_categories <- all_data$jct_bill_categories
app03_district_context <- all_data$app03_district_context
app03_district_context_state <- all_data$app03_district_context_state

########################
#                      #  
# User Interface       #
#                      #
########################
source("ui.R")


########################
#                      #  
# SERVER               #
#                      #
########################
# Handles server-side logic, including reactive expressions and observers, data queries and manipulations.
# Generates outputs based on user inputs and updates the UI accordingly.

#local = TRUE ensures each sourced file has access to input/output/session
server <- function(input, output, session) {
  source("servers/server1_partisanship.R", local = TRUE)
  # source("servers/server2_leg_activity.R", local = TRUE)
  source("servers/server3_district_context.R", local = TRUE)
}

########################
#                      #  
# SHINY                #
#                      #
########################
shinyApp(ui, server)
