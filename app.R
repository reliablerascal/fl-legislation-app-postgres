# APP.R
# June and July 2024 RR

# This is the dev version of the Shiny app for the Jacksonville Tributary's legislative dashboard
# Web app: https://mockingbird.shinyapps.io/fl-leg-app-postgres/
# Repo: https://github.com/reliablerascal/fl-legislation-app-postgres

# adapted from original version by Andrew Pantazi (see https://github.com/apantazi/legislator_dashboard/tree/main) 


#########################
#                       #  
# The App               #
#                       #
######################### 

library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(patchwork) # combines multiple ggplot2 plots into a single cohesive layout. Used for demographics bar charts

library(DBI)
library(RPostgres)
library(scales)

# 7/22/24 removed performance libraries, since they may not be used 
#library(foreach) # enables parallel processing for intense computation- but might not be being used
#library(profvis) # tool for profiling code performance
#library(data.table) # used for fast/efficient join operations

# 7/22/24 other libraries removed b/c they seem more like ETL/ data prep functions
#library(jsonlite) # probably not needed since I parsed data into relational format
#library(lubridate) # parsing dates and times, etc.- shouldn't need it at this phase
#library(forcats) # tools for working with categorical variables (factors)
#library(stringr) #string operations
#library(purrr) # apply a function to each element in a list, etc.
#library(readr) # reads "rectangular" data (like csvs)

#library(tidyr)
#library(tibble)
#library(tidyverse)



#########################
#                       #  
# Data prep             #
#                       #
######################### 
### Run this in the console after re-opening this project, then comment it out
#source("read_data.R") # prior to running the app offline
#source("save_data.R") # prior to uploading the app to Shiny (saves as RDS = relational data service)



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
