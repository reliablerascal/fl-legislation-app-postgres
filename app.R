# APP.R
# adapted from original version by Andrew Pantazi (see https://github.com/apantazi/legislator_dashboard/tree/main)
# June and July 2024 RR

# This is the dev version of the Shiny app for the Jacksonville Tributary's legislative dashboard
# Web app: https://mockingbird.shinyapps.io/fl-leg-app-postgres/
# Repo: https://github.com/reliablerascal/fl-legislation-app-postgres

#########################
#                       #  
# The App               #
#                       #
######################### 
#source("init.R", local = TRUE) # install packages if needed

library(shiny)


library(dplyr)
conflicted::conflict_prefer_all("dplyr", quiet=TRUE)
library(plotly)
conflicted::conflicts_prefer(plotly::layout,.quiet = TRUE)
library(ggplot2)
library(patchwork) # combines multiple ggplot2 plots into a single cohesive layout. Used for demographics bar charts
library(DBI) # access fl_leg_votes database
library(RPostgres) # access fl_leg_votes database
library(scales) # format as percent
library(shinydisconnect) #customize Shiny app disconnect message

#########################
#                       #  
# Data prep             #
#                       #
######################### 

#########################
#                       #  
# Read from Postgres    ###
### Run this in the console after re-opening this project, then comment it out
#source("read_data.R")  # prior to running the app offline#
#source("save_data.R")  # prior to uploading the app to Shiny (saves as RDS = relational data service)#
#                       #
######################### 

### read all_data
#########################
#                       #  
###  Read locally     ###
#all_data <- readRDS("data/all_data.rds")#
######################### 


#########################
#                       #  
###  Read from AWS    ###
#all_data <- readRDS("data/all_data.rds")#
library(httr)
url <- "https://s3.amazonaws.com/data.jaxtrib.org/dev/all_data.rds"

#all_data <- readRDS("C:/Users/Andrew/Documents/fl-legislation-etl-/data-app/all_data.RDS")

temp_file <- tempfile(fileext = ".rds")
GET(url, write_disk(temp_file, overwrite = TRUE))
all_data <- readRDS(temp_file)
unlink(temp_file)
######################### 

### set up dataframes ####
app01_vote_patterns <- all_data$app01_vote_patterns
app02_leg_activity <- all_data$app02_leg_activity
jct_bill_categories <- all_data$jct_bill_categories
app03_district_context <- all_data$app03_district_context
app03_district_context_state <- all_data$app03_district_context_state
app04_district_context <- all_data$app04_district_context
source("servers/voting_history_module.R", local = TRUE)

########################
#                      #  
# User Interface       #
#                      #
########################
source("ui.R", TRUE)

########################
#                      #  
# SERVER               #
#                      #
########################
# Handles server-side logic, including reactive expressions and observers, data queries and manipulations.
# Generates outputs based on user inputs and updates the UI accordingly.


#local = TRUE ensures each sourced file has access to input/output/session
server <- function(input, output, session) {
  source("servers/server1_vote_patterns.R", local = TRUE)
  #source("servers/server2_leg_activity.R", local = TRUE) #we've moved it inside of server3
  source("servers/server3_district_context.R", local = TRUE)
  source("servers/server4_partisanship_scatterplot.R", local = TRUE)
  source("servers/server5_legislator_lookup.R", local = TRUE)
  #print(paste("Number of rows in app02_leg_activity:", nrow(app02_leg_activity)))  # Debug print
  #print(head(app02_leg_activity))  # Debug print
  output$debug_output <- renderPrint({
    print("Columns in app02_leg_activity:")
    print(names(app02_leg_activity))
    print("Summary of vote_with_neither:")
    print(summary(app02_leg_activity$vote_with_neither))
    print("Summary of maverick_votes:")
    print(summary(app02_leg_activity$maverick_votes))
  })
}

########################
#                      #  
# SHINY                #
#                      #
########################
shinyApp(ui, server)
