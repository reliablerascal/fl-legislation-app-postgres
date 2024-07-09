# Legislator Dashboard
7/9/24

This work-in-progress repo develops the Jacksonville Tributary's interactive legislative dashboard based on my [revised data pipeline](https://github.com/reliablerascal/fl-legislation-etl).

Here's the app:
* [development version](https://mockingbird.shinyapps.io/fl-leg-app-postgres/) (July 2023), as described in this repo.
* [production version](https://shiny.jaxtrib.org) (prior version, May 2023).

The app consists of the following visualizations:
|Tab|Intended Audience|Description
|---|---|---|
|**District Context**|voters in Florida's August primary|Compare each representative's partisan voting patterns against their demographic and electoral context.|
|**Voting Patterns**|data-savvy journalists at Florida partner outlets|heatmap of voting patterns on contested bills by party, chamber, and session year|

<!---
|**Legislator Activity Overview**<br>(TEMPORARILY DISCONTINUED)|policy wonks|an interface for reviewing legislative activity by legislator, as well as searching bills|
--->


## Applications

The app consists of the following R components:

- [app.R](app.R): Orchestrates the Shiny app by setting up the user interface, server logic, and handling reactive expressions for the Shiny web app.
- [server1_partisanship.R](server1_partisanship.R): Application logic for voting patterns app.
- [server3_district_context.R](server3_district_context.R): Application logic for district context app.
- [ui.R](ui.R): Defines the Shiny app user interface.

See [data pipeline repo](https://github.com/reliablerascal/fl-legislation-etl) for a detailed overview of data used in these apps.

## Improvements in This Version
In June &amp; July, I re-architected the data pipeline in order to speed up development and improve maintainability of this web app. The updated data source eliminates record duplicates, handles historically-changing data, and facilitates filtering and sorting.

### District Context
This is a new app, incorporating some partisanship data from the Voting Patterns dashboard in addition to newly integrated census and electoral data.

See [development notes](https://docs.google.com/document/d/1e3KDrnpXjKL4OJqFR49hqti77TntPRL7k4AkqSfsefU/edit?usp=drive_link) for info on work in progress on this app.

### Voting Patterns
Adapted from pantazi's [Voting Patterns Analysis](https://shiny.jaxtrib.org/), with the following improvements:
* Hyperlinks to legislators' Ballotpedia pages and bills' LegiScan pages.
* **Sorting** options added for legislators (by partisanship, name, district number) and roll calls (by bill number, partisanship).
* **Legend** displays heatmap color samples, more detail on methodology, and adds count of legislators and roll-call votes in filtered views.
* Legislators (including district #) are now displayed on Y-axis
* Tooltips displays vote by party and improved formatting.
* Filter added for **bill category**. Note that this currently includeds only a placeholder "education" category with a small number of bills. More work is required to populate a cross-reference table assigning bills to categories.

See [development notes](https://docs.google.com/document/d/1OGiJH7B_0j3B38gEtgt_FDhkxzL84ZtGistdup2yYHI/edit?usp=drive_link) for info on work in progress on this app.