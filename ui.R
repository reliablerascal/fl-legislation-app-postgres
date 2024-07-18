#UI.R
#
# Defines the layout and appearance of the application.
# Specifies user input controls (e.g., text inputs, sliders, drop-downs).
# Sets up placeholders for outputs (e.g., tables, plots, text).
#
# 6/13/24 RR
# separated this section into its own script, but kept Andrew's code intact
# none of this is dependent on data

library(shinythemes)
library(shinyjs)
library(DT)
library(plotly)
library(shinyWidgets)
library(shiny)

#####################
#                   #  
# app 1A            #
#                   #
#####################
# Define the UI for App 1 ####

app1_ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  uiOutput("dynamicHeader"),
  uiOutput("staticMethodology1"),
  uiOutput("dynamicFilters"),
  uiOutput("dynamicLegend"),
  uiOutput("dynamicRecordCount"),
  uiOutput("noDataMessage"),
  plotlyOutput("heatmapPlot"#, height = "150vh")
  )
)



#####################
#                   #  
# app 2 HOLD        #
#                   #
#####################
# Define the UI for App 2 ####
app2_ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("THIS SECTION BEING RE-ARCHITECTED FOR POSTGRES DEPENDENCE"),
  div(class="filter-group",
      div(class="filter-section", span("Party:"), actionButton("btn_party_R", "R", class="btn-filter btn-party R"), actionButton("btn_party_D", "D", class="btn-filter btn-party D")),
      div(class="filter-section", span("Chamber:"), actionButton("btn_role_Rep", "House", class="btn-filter btn-role Rep"), actionButton("btn_role_Sen", "Senate", class="btn-filter btn-role Sen")),
      div(class="filter-section", span("Final Vote:"), actionButton("btn_final_Y", "Yes", class="btn-filter btn-final Y"), actionButton("btn_final_N", "No", class="btn-filter btn-final N"))
  ),
  div(class="filter-group",
      div(class="filter-section", textInput("searchText", "Search Bills:", placeholder="Type to search in titles or descriptions...")),
      div(class="filter-section",selectizeInput("legislator", "Select Legislator:", choices = NULL,options = list('create' = TRUE, 'persist' = FALSE, 'placeholder' = 'Type to search...', 'onInitialize' = I("function() { this.on('dropdown_open', function() { $('.selectize-dropdown-content').perfectScrollbar(); }); }"))))
  ),
  div(id="filter-info", style="display: flex; align-items: center; margin-bottom: 20px;",
      textOutput("filterFeedback"),
      uiOutput("votefilterinfo") # This will dynamically generate additional info or tooltip content
  ),  div(class="content",
          div(class="votes-section", uiOutput("votesDisplay")),
          div(class="legislator-profile",
              div(class = "filter-section",
                  span("Session Year:"),
                  actionButton("btn_year_2023", "2023", class = "btn-filter"),
                  actionButton("btn_year_2024", "2024", class = "btn-filter")
              ),
              uiOutput("legislatorProfile"),
              div(class="profile-filter-section", checkboxGroupInput("voteType", label=HTML('Key Vote Type: <button id="info" type="button" class="btn btn-default action-button shiny-bound-input" onclick="Shiny.setInputValue(\'info_clicked\', true, {priority: \'event\'});"><i class="fas fa-info-circle"></i></button>'), choices=list("Voted Against Both Parties"="Independent", "Voted With Other Party"="Maverick", "Voted With Own Party"="Normal")))
          )
  ),
  div(class="navigation",
      actionButton("first_page", "First", class="btn-page"),
      actionButton("prev_page", "Previous", class="btn-page"),
      uiOutput("page_info"),  # Changed from textOutput to uiOutput
      actionButton("next_page", "Next", class="btn-page"),
      actionButton("last_page", "Last", class="btn-page"),
      selectInput("items_per_page", "Items per Page:", choices = c(20, 50, 100), selected = 20)
  ),
  
  tags$script(HTML('
      $(document).on("click", "#info", function() {
      // Trigger a Shiny event when the info button is clicked
      Shiny.setInputValue("info_clicked", true, {priority: "event"});
    });
        $(document).ready(function() {
          // Existing functionality: Toggle vote details on click
          $(document).on("click", ".vote-link", function() {
            var detailsId = $(this).attr("id").replace("vote-details-link-", "vote-details-");
            $("#" + detailsId).toggle();
          });
          
          // Detect Enter keypress in the selectize input (legislator search)
          $("#legislator").next(".selectize-control").find(".selectize-input input").on("keypress", function(e) {
            if(e.which == 13) { // Enter key pressed
              // Trigger an event in Shiny to update the dropdown based on current input
              Shiny.setInputValue("legislator_enter_pressed", $("#legislator").val(), {priority: "event"});
            }
          });
          
        });
         $(document).on("focus", "#legislator-selectize-input", function() {
                     Shiny.setInputValue("legislator_focused", true);
                     });
                $(document).on("blur", "#legislator-selectize-input", function() {
                  Shiny.setInputValue("legislator_focused", false);
                });
      '))
)

###########################
#                         #  
# app 3 district context  #
#                         #
###########################

app3_ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://mockingbird.shinyapps.io/fl-leg-app-postgres/styles.css")),
  uiOutput("dynamicHeader3"),
  uiOutput("dynamicFilters3"),
  uiOutput("dynamicPartisanship"),
  uiOutput("dynamicDemographics"),
  #uiOutput("dynamicLegProfile"),
  uiOutput("staticMethodology3")
)

#####################
#                   #  
# navbar page       #
#                   #
#####################

# Combine the UIs into a navbarPage ####
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Florida Legislature Dashboard • The Tributary"),
    tags$link(rel = "icon", href = "https://jaxtrib.org/wp-content/uploads/2021/06/cropped-favicon-32x32.png", sizes = "32x32"),
    tags$link(rel = "icon", href = "https://i2.wp.com/jaxtrib.org/wp-content/uploads/2021/06/cropped-favicon.png?fit=192%2C192&ssl=1", sizes = "192x192"),
    tags$meta(name = "robots", content = "index, follow, max-image-preview:large, max-snippet:-1, max-video-preview:-1"),
    tags$meta(name = "google-site-verification", content = "c-p4lmvJsiiQlKV2swCEQsMzWP3CX46GCRBL7WXjVxk"),
    tags$meta(name = "description", content = "Explore the interactive dashboard for insights into the Florida Legislature's voting patterns, presented by The Tributary."),
    tags$meta(property = "og:locale", content = "en_US"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:title", content = "Florida Legislature Voting Dashboard • The Tributary"),
    tags$meta(property = "og:description", content = "Explore the interactive dashboard for insights into the Florida Legislature's voting patterns, presented by The Tributary."),
    tags$meta(property = "og:url", content = "https://data.jaxtrib.org/legislator_dashboard"),
    tags$meta(property = "og:site_name", content = "The Tributary"),
    tags$meta(property = "article:publisher", content = "https://data.tributary.org/legislature_dashboard.png"),
    tags$meta(property = "og:image:type", content = "image/png"),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(charset = "utf-8"),
    tags$title("Interactive Dashboard • The Tributary"),
    tags$link(rel = "icon", href = "https://jaxtrib.org/wp-content/uploads/2021/06/cropped-favicon-32x32.png", sizes = "32x32"),
    tags$link(rel = "icon", href = "https://i2.wp.com/jaxtrib.org/wp-content/uploads/2021/06/cropped-favicon.png?fit=192%2C192&ssl=1", sizes = "192x192"),
    # Twitter meta tags
    tags$meta(name = "twitter:title", content = "Florida Legislature Voting Dashboard • The Tributary"),
    tags$meta(name = "twitter:description", content = "Explore the interactive dashboard for insights into the Florida Legislature's voting patterns, presented by The Tributary."),
    tags$meta(name = "twitter:image", content = "https://data.tributary.org/legislature_dashboard.png"),
    tags$meta(name = "twitter:creator", content = "@APantazi"),
    tags$meta(name = "twitter:site", content = "@TheJaxTrib"),
    tags$meta(name = "twitter:label1", content = "Written by"),
    tags$meta(name = "twitter:data1", content = "Andrew Pantazi"),
    # Facebook meta tag
    tags$meta(property = "fb:pages", content = "399115500554052"),
    # Additional meta tags
    tags$meta(name = "theme-color", content = "#fff"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-touch-fullscreen", content = "YES"),
    tags$meta(name = "apple-mobile-web-app-title", content = "The Tributary"),
    tags$meta(name = "application-name", content = "The Tributary"),
    tags$meta(property = "article:published_time", content = "2024-02-22T03:02:59+00:00"),
    tags$meta(property = "article:modified_time", content = "2024-02-22T03:02:59+00:00"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Archivo:ital,wght@0,500;0,600;1,500;1,600&display=swap", rel = "stylesheet"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # Banner #####
  div(class = "banner",
      tags$a(href = "https://jaxtrib.org/", 
             tags$img(src = "https://jaxtrib.org/wp-content/uploads/2021/09/TRB_TributaryLogo_NoTagline_White.png", class = "logo-img", alt = "The Tributary")
      )
  ),
  
  #####################
  #                   #  
  # navigation bar    #
  #                   #
  #####################
  div(class="navbar2",
      tabsetPanel(
      tabPanel("District Context", value = "app3", app3_ui),
      tabPanel("Voting Patterns", value = "app1", app1_ui),
      #tabPanel("Legislator Activity Overview", value = "app2", app2_ui),
      id = "navbar_page",
      selected = "app3" #start on this app by default
      )
  )
)