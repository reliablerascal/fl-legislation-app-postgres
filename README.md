# Legislator Dashboard

This repo contains the Jacksonville Tributary's [interactive web application for displaying legislative votes](https://shiny.jaxtrib.org/), as developed by Andrew Pantazi prior to May 2024. The application is based on [LegiScan's 2023 and 2024 legislative session data](https://legiscan.com/FL/datasets), and consists of two visualizations:
* **Voting Patterns Analysis**- a heatmap of voting patterns on contested bills by party, chamber, and session year
* **Legislator Activity Overview**- an interface for reviewing legislative activity by legislator, as well as searching bills


## Applications

The repo pipeline consists of the following R applications:

- [app.R](app.R): Scrapes the TSA FOIA electronic reading room for all relevant PDFs and downloads them to the [pdfs/](pdfs/) directory.
- [pull-in-process-all-legiscan.R](pull-in-process-all-legiscan.R): Parses LegiScan data, handles missing fields, creates dataframes (sponsors, amendments, referrals, history, votes, and supplements), and transforms/analyzes data for use by the Shiny App.
- additional app.R files in subfolders are prior development versions of the two separate visualizations

## Data
Bulk downloaded data is initially saved in json_files (hidden), and transformed into [data.RData](data.RData)