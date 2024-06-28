# Legislator Dashboard

This work-in-progress repo adapts the Jacksonville Tributary's [interactive Shiny web application](https://github.com/apantazi/legislator_dashboard/blob/main/app.R) (see [original demo app](https://shiny.jaxtrib.org/)), connecting to Postgres rather than CSV or R data. Data is sourced from [LegiScan's 2023 and 2024 legislative session data](https://legiscan.com/FL/datasets). The app consists of two visualizations:
* **Voting Patterns Analysis**- a heatmap of voting patterns on contested bills by party, chamber, and session year
* **Legislator Activity Overview**- an interface for reviewing legislative activity by legislator, as well as searching bills

## Applications

The repo pipeline consists of the following R applications:

- [app.R](app.R): Orchestrates the Shiny app by setting up the user interface, server logic, and handling reactive expressions for the Shiny web app.
- [server.R](server.R): Reads the voting_patterns data processed in the ETL pipeline (see [data dictionary](https://docs.google.com/spreadsheets/d/1qPUk0-wx4sislv_TbE6poKOZBvDpdmpJp6_QWNK77I4/edit?gid=1711212896#gid=1711212896)).
- [ui.R](ui.R): Defines the Shiny app user interface.

## Intended Updates ###
As I'm streamlining the architecture, I'm also improving the usefulness of the application. Following are some intended updates:
* **Visualize voting patterns for cities as well as states** by incorporating **roll call data from Jacksonville and other cities** via Legistar and transforming it into the same data model.
* **Show variation between legislator voting habits and constituent political leanings** by incorporating district-level elections data
* Clean up and simplify user interface
* Create additional static visualizations from the same application layer