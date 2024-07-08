# Legislator Dashboard
7/7/24

This work-in-progress repo adapts the Jacksonville Tributary's interactive legislative dashboard, based on my [revised data pipeline](https://github.com/reliablerascal/fl-legislation-etl).

The app will consist of the following visualizations:
|Tab|Intended Audience|Description
|---|---|---|
|**Voting Patterns Analysis**|data-savvy journalists at Florida partner outlets|heatmap of voting patterns on contested bills by party, chamber, and session year|
|**Legislator Activity Overview**<br>(TEMPORARILY DISCONTINUED)|policy wonks|an interface for reviewing legislative activity by legislator, as well as searching bills|
|**Representation Alignment Analysis**<br>(NEW)|voters in Florida's August primary|an interface for reviewing legislative activity by legislator, as well as searching bills|

Here's the app:
* [development version](https://mockingbird.shinyapps.io/fl-leg-app-postgres/)- my new work-in-progress version, which develops the Voting Patterns Analysis (July 2023).
* [production version](https://shiny.jaxtrib.org)- prior version of the app (May 2023) currently used by the Jacksonville Tributary (see also [dev repo](https://github.com/apantazi/legislator_dashboard/blob/main/app.R))

## Applications

The app consists of the following R components:

- [app.R](app.R): Orchestrates the Shiny app by setting up the user interface, server logic, and handling reactive expressions for the Shiny web app.
- [server.R](server.R): Reads the voting_patterns data processed in the ETL pipeline (see [data dictionary](https://docs.google.com/spreadsheets/d/1qPUk0-wx4sislv_TbE6poKOZBvDpdmpJp6_QWNK77I4/edit?gid=1711212896#gid=1711212896)).
- [ui.R](ui.R): Defines the Shiny app user interface.

## Improvements in This Version
In June, I [re-architected the data pipeline](https://github.com/reliablerascal/fl-legislation-etl), which should speed up development and improve maintainability of this web app. The updated data source eliminates record duplicates and facilitates filtering and sorting. Following are the changes made to date:
* **Legend** displays heatmap color samples and adds count of legislators and roll-call votes in filtered views.
* Legislators (including district #) are now displayed on Y-axis
* Tooltips displays vote by party, improved formatting, and hyperlink to bill on LegiScan.
* Filter added for **bill category**. Note that this currently includeds only a placeholder "education" category with a small number of bills. More work is required to populate a cross-reference table assigning bills to categories.
* **Sort legislators by** option enables sorting by partisanship rank, legislator name, or district.

## Future Updates ###
As I'm streamlining the architecture, I'm also improving the usefulness of the application. Following are some intended updates:
* Continue **UX improvements** including:
    * X-axis labels to include hyperlink to legislators' ballotpedia pages
    * Y-axis labels to include hyperlink to bills
    * Tooltips to display more detail about each legislator's overall voting record

* **Visualize voting patterns for cities as well as states** by incorporating **roll call data from Jacksonville and other cities** via Legistar and transforming it into the same data model.
* **Show variation between legislator voting habits and constituent political leanings** by incorporating district-level elections data