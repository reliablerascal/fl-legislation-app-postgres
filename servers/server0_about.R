##server0_about.R
aboutTabServer <- function(id) {
  output$aboutText <- renderUI({ h2("Testing AboutTabServer") })
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$aboutText <- renderUI({
      tagList(
        div(class = "about-section",
            h2("About Florida Legislative Compass"),
            p("The Florida Legislative Compass is an interactive tool for exploring how state lawmakers vote, how those votes compare to their districts' partisanship, and what stories might be hiding in the data."),
            p(
              "Originally developed as a beta tool for Florida journalists ahead of the 2024 elections, this project was first built by ",
              tags$a(href = "https://github.com/apantazi", "Andrew Pantazi", target = "_blank"), " and ",
              tags$a(href = "https://github.com/reliablerascal", "Rob Reid", target = "_blank"),
              " for The Tributary. The Florida version (",
              tags$a(href = "https://data.jaxtrib.org/dev/legislative-compass", "see here", target = "_blank"),
              ") helped reporters analyze legislative voting patterns and spot outliers or potential stories."
            ),
            p("This new edition adapts the Legislative Compass to Florida's legislative data, giving journalists, advocates, and the public a fast, intuitive way to spot trends and anomalies in the legislature."),
            
            h3("How to Use the App"),
            
            tags$ul(
              tags$li(
                tags$a("Voting Patterns", href = "#", onclick = "Shiny.setInputValue('go_tab', 'app1', {priority: 'event'}); return false;"),
                ": See how lawmakers voted on partisan bills, filter by party, chamber, or session, and identify who crosses party lines."
              ),
              tags$li(
                tags$a("District Context", href = "#", onclick = "Shiny.setInputValue('go_tab', 'app3', {priority: 'event'}); return false;"),
                ": View each district’s demographics and political lean, and compare them to their legislators’ voting records."
              ),
              tags$li(
                tags$a("Partisanship Scatterplot", href = "#", onclick = "Shiny.setInputValue('go_tab', 'app4', {priority: 'event'}); return false;"),
                ": Explore the relationship between district partisanship and individual lawmaker votes."
              )
            ),
            
            h3("Data Sources"),
            tags$ul(
              tags$li("Florida legislative roll-call votes: ", tags$a(href="https://legiscan.com/FL/", "LegiScan", target="_blank")),
              tags$li("District demographic and political data: ", tags$a(href="https://davesredistricting.org","Dave's Redistricting",target="_blank")),
              tags$li("Partisanship measures: Analysis by project authors based on recent statewide elections")
            ),
            
            div(class = "header-section", "Methodology"),
            div(class = "methodology-notes",
                p(strong("Party Loyalty:"), " Calculated as the proportion of a legislator's votes that align with their party's majority on bills where a majority of Democrats and a majority of Republicans voted differently. This excludes unanimous and broadly bipartisan votes. For each qualifying bill, if a lawmaker votes the same way as their party's majority, it's counted as loyal; otherwise, it's not. A score of 1 means always voting with the party, while 0 means always voting against the party."),
                p(strong("Bills Analyzed:"), " This analysis includes only bills where a majority of Democrats and a majority of Republicans voted differently, highlighting partisan disagreements."),
                p(strong("District Partisan Lean:"), " Calculated from the weighted average of the following election results:"),
                tags$ul(
                  tags$li("2020 Presidential Election (25% weight)"),
                  tags$li("2022 Gubernatorial Election (25% weight)"),
                  tags$li("2024 Presidential Election (50% weight)")
                ),
                p("Positive values indicate a Republican lean, negative values a Democratic lean."),
                p(strong("Median District:"), " The district with the median partisan lean value, representing the 'middle' of the political spectrum for the selected districts.")
            ),
            
            h3("Background"),
            p("The Legislative Compass was inspired by the need for greater transparency and accountability in state government. Its visualizations and interactive filters help surface patterns—like party loyalty, bipartisan coalitions, and lawmakers whose votes defy their district’s lean."),
            p("This project is open-source and open for feedback. Please reach out with suggestions, bug reports, or data corrections by emailing andrew.pantazi [at] gmail [dot] com.")
        )
      )
    })
  })
}