# Legislator Dashboard

This work-in-progress repo adapts the Jacksonville Tributary's [interactive web application for displaying legislative votes](https://shiny.jaxtrib.org/), connecting to Postgres rather than CSV data. Data is sourced from [LegiScan's 2023 and 2024 legislative session data](https://legiscan.com/FL/datasets). The app consists of two visualizations:
* **Voting Patterns Analysis**- a heatmap of voting patterns on contested bills by party, chamber, and session year
* **Legislator Activity Overview**- an interface for reviewing legislative activity by legislator, as well as searching bills


## Applications

The repo pipeline consists of the following R applications:

- [app.R](app.R): Reads data from the [legislative voting database](https://github.com/reliablerascal/fl-legislation-db), defines the user interface and server logic, and handles reactive expressions for the Shiny web app.

## Data
Bulk downloaded data from [LegiScan's 2023 and 2024 legislative session data](https://legiscan.com/FL/datasets) is initially saved in json_files (hidden), and transformed into the Postgres database.

## Setup
'''
install.packages("DBI")
install.packages("RPostgres")
'''