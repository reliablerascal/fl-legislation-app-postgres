library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  titlePanel("Legislator Activity Overview"),
  
  navbarPage("Navigation",
             tabPanel("App 2: Legislator Activity Overview", value = "app2",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Filters"),
                          div(id = "votefilterinfo"),
                          hr(),
                          selectizeInput("legislator", "Select Legislator:", choices = NULL, selected = NULL),
                          checkboxGroupInput("voteType", "Vote Type:", choices = NULL),
                          hr(),
                          actionButton("btn_year_2023", "2023", class = "btn-year-filter"),
                          actionButton("btn_year_2024", "2024", class = "btn-year-filter"),
                          hr(),
                          actionButton("btn_party_R", "Republican", class = "btn-party-filter"),
                          actionButton("btn_party_D", "Democrat", class = "btn-party-filter"),
                          hr(),
                          actionButton("btn_role_Rep", "House", class = "btn-role-filter"),
                          actionButton("btn_role_Sen", "Senate", class = "btn-role-filter"),
                          hr(),
                          actionButton("btn_final_Y", "Yes", class = "btn-final-filter"),
                          actionButton("btn_final_N", "No", class = "btn-final-filter"),
                          hr(),
                          textInput("searchText", "Search Text:"),
                          hr(),
                          uiOutput("page_info"),
                          actionButton("first_page", "First"),
                          actionButton("prev_page", "Previous"),
                          actionButton("next_page", "Next"),
                          actionButton("last_page", "Last")
                        ),
                        mainPanel(
                          uiOutput("legislatorProfile"),
                          uiOutput("votesDisplay"),
                          textOutput("filterFeedback")
                        )
                      )
             )
  )
))
