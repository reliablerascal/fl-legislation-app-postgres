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
source("init.R", local = TRUE) # install packages if needed

library(shiny)
library(shinyMobile)
library(dplyr)
library(data.table)
library(plotly)
library(ggplot2)
library(patchwork)
library(scales)
library(shinydisconnect)
library(shinyjs)
library(shinythemes)
library(bslib)
library(DT)
library(shinyWidgets)
library(lubridate)
library(config)
#library(shinyBS)
library(bsicons)

#library(showtext)
#showtext_auto()
conflicted::conflict_prefer_all("dplyr", quiet=TRUE)
conflicted::conflicts_prefer(plotly::layout,.quiet = TRUE)

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
all_data <- qs::qread("all_data.qs")

######################### 


#########################
#                       #  
###  Read from AWS    ###
#library(httr)
#url <- "https://legislative-compass.s3.us-east-2.amazonaws.com/all_data.RDS"

# Download to a temporary file
#temp_file <- tempfile(fileext = ".rds")
#GET(url, write_disk(temp_file, overwrite = TRUE))

# Load the data
#all_data <- readRDS(temp_file)

# Clean up
#unlink(temp_file)

######################### 

### set up dataframes ####
app01_vote_patterns <- as.data.table(all_data$app01_vote_patterns)
app02_leg_activity <- as.data.table(all_data$app02_leg_activity)
#jct_bill_categories <- as.data.table(all_data$jct_bill_categories)
app03_district_context <- as.data.table(all_data$app03_district_context)
app03_district_context_state <- as.data.table(all_data$app03_district_context_state)
########################
#                      #  
# User Interface       #
#                      #
########################
source("servers/voting_history_module.R", local = TRUE)
source("ui.R", TRUE)

########################
#                      #  
# SERVER               #
#                      #
########################
# Handles server-side logic, including reactive expressions and observers, data queries and manipulations.
# Generates outputs based on user inputs and updates the UI accordingly.

pending_selection <- reactiveValues(legislator = NULL, chamber = NULL)

#local = TRUE ensures each sourced file has access to input/output/session
server <- function(input, output, session) {
  observeEvent(input$go_tab, {
    updateTabsetPanel(session, "navbar_page", selected = input$go_tab)
  })
  source("servers/server1_vote_patterns.R", local = TRUE)
  source("servers/server3_district_context.R", local = TRUE)
  source("servers/server4_partisanship_scatterplot.R", local = TRUE)
  source("servers/server0_about.R", local = TRUE)
  aboutTabServer("about")  
}



########################
#                      #  
# SHINY                #
#                      #
########################
shinyApp(ui, server)
