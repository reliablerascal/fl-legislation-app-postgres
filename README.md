# Legislator Dashboard
7/31/24

This repo tracks development of the Jacksonville Tributary's interactive legislative dashboard, which is based on roll call vote data from [Legiscan](https://legiscan.com/FL/datasets) as well as elections and demographic data from [Dave's Redistricting](https://davesredistricting.org/maps#state::FL). Data is prepared for display in an [ETL data pipeline](https://github.com/reliablerascal/fl-legislation-etl).

Here's the **[development version](https://mockingbird.shinyapps.io/fl-leg-app-postgres/)** of the web app, as described in this repo.

The app consists of the following visualizations:
|Tab|Intended Audience|Description
|---|---|---|
|**Legislator Lookup**|voting public|Find your state and national representatives based on your home address.|
|**District Context**|voting public|Compare each representative's partisan voting patterns against their demographic and electoral context.|
|**Voting Patterns**|data-savvy journalists at Florida partner outlets|Display a heatmap of voting patterns on contested bills by party, chamber, and session year|



## Applications

<div align = "left">

### Voting Patterns Dashboard

<div style = "padding-left: 20px;">
<img src="viz/screenshot_voting_patterns.png" width = 800 style="border: 2px solid black;">

*Figure 1: Dashboard view of **Voting Patterns** web app of Senate Democrats during 2024 legislative session*
</div>

<br><br>

### District Context Web App
This is a new app, incorporating some partisanship data from the Voting Patterns dashboard in addition to newly integrated census and electoral data.
<div style = "padding-left: 20px;">
<img src="viz/screenshot_district_context.png" width = 800 style="border: 2px solid black;">

*Figure 2: **District Context** web app comparing a legislator's voting record with their electorate's partisan leaning*
</div>

<br><br>

## Ongoing Development
See the following documents for info on past and current development:
* [changelog](docs/changelog.md)- lists updates in the alpha and beta versions
* [Voting Patterns development notes](https://docs.google.com/document/d/1OGiJH7B_0j3B38gEtgt_FDhkxzL84ZtGistdup2yYHI/edit?usp=drive_link)
* [District Context development notes](https://docs.google.com/document/d/1e3KDrnpXjKL4OJqFR49hqti77TntPRL7k4AkqSfsefU/edit?usp=drive_link)

<br><br>

## Guide to This Repository
Typical of Shiny apps, this repo consists of the following R components:
- [app.R](app.R): Orchestrates the Shiny app by setting up the user interface, server logic, and handling reactive expressions for the Shiny web app.
- Server scripts: Application logic for each application.
    - [server1_vote_patterns.R](servers/server1_vote_patterns.R)
    - [server3_district_context.R](servers/server3_district_context.R)
    - [server5_legislator_lookup.R](servers/server5_legislator_lookup.R)
- [ui.R](ui.R): Defines the Shiny app user interface.


```
├── app.R
├── data
│   └── all_data.rds
├── docs
│   └── changelog.md
├── read_data.R
├── save_data.R
├── servers
│   ├── server1_partisanship.R
│   └── server3_district_context.R
│   └── server5_legislator_lookup.R
├── ui.R
└── www
│   └── styles.css
```