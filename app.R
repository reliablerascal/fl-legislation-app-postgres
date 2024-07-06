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



### After re-opening this project...
### Run this in the console. second line is needed if there's new or updated data
#source("read_data.R") # prior to running the app offline
#source("save_data.R") # prior to uploading the app to Shiny

### set up dataframes
all_data <- readRDS("data/all_data.rds")

app_vote_patterns <- all_data$app_vote_patterns
app_data <- all_data$app_data
jct_bill_categories <- all_data$jct_bill_categories



source("ui.R")
source("server.R")

shinyApp(ui, server)
