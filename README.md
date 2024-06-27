# Legislator Dashboard

This work-in-progress repo adapts the Jacksonville Tributary's [interactive Shiny web application](https://github.com/apantazi/legislator_dashboard/blob/main/app.R) (see [original demo app](https://shiny.jaxtrib.org/)), connecting to Postgres rather than CSV data. Data is sourced from [LegiScan's 2023 and 2024 legislative session data](https://legiscan.com/FL/datasets). The app consists of two visualizations:
* **Voting Patterns Analysis**- a heatmap of voting patterns on contested bills by party, chamber, and session year
* **Legislator Activity Overview**- an interface for reviewing legislative activity by legislator, as well as searching bills

## Intended Updates ###
As I'm streamlining the architecture, I'm also improving the usefulness of the application. Following are some intended updates:
* **Visualize voting patterns for cities as well as states** by incorporating **roll call data from Jacksonville and other cities** via Legistar and transforming it into the same data model.
* **Show variation between legislator voting habits and constituent political leanings** by incorporating district-level elections data
* Clean up and simplify user interface
* Create additional static visualizations from the same application layer

## Applications

The repo pipeline consists of the following R applications:

- [app.R](app.R): Reads data from the [legislative voting database](https://github.com/reliablerascal/fl-legislation-db), defines the user interface and server logic, and handles reactive expressions for the Shiny web app.