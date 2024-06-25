# APP.R
# This is a Shiny app for the Jacksonville Tributary's legislative dashboard at https://shiny.jaxtrib.org/ 
#
# 6/13/24 RR
# changes to the original code:
# modularized app to separate ui.R and server.R
# updated connectivity to Postgres rather than data.RData
# see related scripts for more detail about updates

print("look for password prompt, which is currently needed to start the app")

load("data.RData")

# Source the UI and server components
source("ui.R")
source("server_postgres.R")
# source("temp1004 server_postgres.R")

shinyApp(ui, server)