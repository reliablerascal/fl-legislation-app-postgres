# APP.R
# This is a Shiny app for the Jacksonville Tributary's legislative dashboard at https://shiny.jaxtrib.org/ 
#
# 6/13/24 RR
# changes to the original code:
# modularized app to separate ui.R and server.R
# updated connectivity to Postgres rather than data.RData
# moved hover_text creation to this front-end app
# see related scripts for more detail about updates


library(foreach)
library(profvis)
library(data.table)
library(jsonlite)
library(lubridate)
library(forcats)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(tibble)
library(ggplot2)
library(tidyverse)
library(DBI) # added 6/13/24 for Postgres connectivity
library(RPostgres) # added 6/13/24 for Postgres connectivity

#print("look for password prompt, which is currently needed to start the app")

source("read_data.R")

source("ui.R")
source("server_postgres.R")

shinyApp(ui, server)
