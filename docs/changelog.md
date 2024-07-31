# Change Log (alpha)

## Week of 29 July

### App- District Context
* District partisan lean now calculated as a weighted average (10% - 2016 Pres, 10% - 2018 Gov, 50% - 2020 Pres, 30% - 2022 Gov)
* Legislator lookup sorted by last name (not first name)

### App- Voting Patterns
* Added sort legislators by district electorate lean

### App- Legislator Lookup
* Created basic version which displays name and party of legislators based on address input

### ETL Pipeline
* setting_district_lean refactored as a data frame to enable weighted calculation of district partisan lean