load("data.RData")
library(shinythemes)
library(foreach)
library(shinyjs)
library(profvis)
library(DT)
library(data.table)
library(plotly)
library(shinyWidgets)
library(shiny)
library(jsonlite)
library(lubridate)
library(forcats)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(tibble)
library(ggplot2)
library(tidyverse)

# Define the UI for App 1 ####
app1_ui <- fluidPage(
  uiOutput("dynamicTitle"),
  div(class="filter-row",
      style="display:flex; flex-wrap: wrap; justify-content: center; margin-top:1.5vw; margin-bottom: 0px; padding-bottom:0px; margin-left:auto; margin-right:auto;",
      
      div(class = "filter-party", selectInput("party", "Select Party:", choices = c("D", "R"))),
      div(class = "filter-role", selectInput("role", "Select Chamber:", choices = c("House" = "Rep", "Senate" = "Sen"))),
      
      div(class = "filter-year",selectInput("year", "Select Session Year:",choices = c(2023,2024,"All"),selected = 2024)),
      
      div(class = "filter-final", 
          selectInput("final", "Final (Third Reading) Vote?", 
                      choices = c("Y","N","All"), 
                      selected = "Y"))
  ),
  plotlyOutput("heatmapPlot"#, height = "150vh"
  ) # Adjust plot height as needed
  
)

# Define the UI for App 2 ####
app2_ui <- fluidPage(
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

# Combine the UIs into a navbarPage ####
ui <- fluidPage(
  useShinyjs(),
  theme=shinytheme("flatly"),
  tags$head( #tags #####
    tags$meta(charset="utf-8"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$title("Florida Legislature Dashboard • The Tributary"),
    tags$link(rel="icon", href="https://jaxtrib.org/wp-content/uploads/2021/06/cropped-favicon-32x32.png", sizes="32x32"),
    tags$link(rel="icon", href="https://i2.wp.com/jaxtrib.org/wp-content/uploads/2021/06/cropped-favicon.png?fit=192%2C192&ssl=1", sizes="192x192"),
    tags$meta(name="robots",content="index, follow, max-image-preview:large, max-snippet:-1, max-video-preview:-1"),
    tags$meta(name="google-site-verification",content="c-p4lmvJsiiQlKV2swCEQsMzWP3CX46GCRBL7WXjVxk"),
    tags$meta(name="description",content="Explore the interactive dashboard for insights into the Florida Legislature's voting patterns, presented by The Tributary."),
    tags$meta(property="og:locale",content="en_US"),
    tags$meta(property="og:type",content="website"),
    tags$meta(property="og:title",content="Florida Legislature Voting Dashboard • The Tributary"),
    tags$meta(property="og:description",content="Explore the interactive dashboard for insights into the Florida Legislature's voting patterns, presented by The Tributary."),
    tags$meta(property="og:url",content="https://data.jaxtrib.org/legislator_dashboard"),
    tags$meta(property="og:site_name",content="The Tributary"),
    tags$meta(property="article:publisher",content="https://data.tributary.org/legislature_dashboard.png"),
    tags$meta(property="og:image:type",content="image/png"),
    tags$meta(name="twitter:card",content="summary_large_image"),
    tags$meta(charset="utf-8"),
    tags$title("Interactive Dashboard • The Tributary"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$link(rel="icon", href="https://jaxtrib.org/wp-content/uploads/2021/06/cropped-favicon-32x32.png", sizes="32x32"),
    tags$link(rel="icon", href="https://i2.wp.com/jaxtrib.org/wp-content/uploads/2021/06/cropped-favicon.png?fit=192%2C192&ssl=1", sizes="192x192"),
    # Twitter meta tags
    tags$meta(name="twitter:title", content="Florida Legislature Voting Dashboard • The Tributary"),
    tags$meta(name="twitter:description", content="Explore the interactive dashboard for insights into the Florida Legislature's voting patterns, presented by The Tributary."),
    tags$meta(name="twitter:image", content="https://data.tributary.org/legislature_dashboard.png"),
    tags$meta(name="twitter:creator", content="@APantazi"),
    tags$meta(name="twitter:site", content="@TheJaxTrib"),
    tags$meta(name="twitter:label1", content="Written by"),
    tags$meta(name="twitter:data1", content="Andrew Pantazi"),
    # Facebook meta tag
    tags$meta(property="fb:pages", content="399115500554052"),
    # Additional meta tags
    tags$meta(name="theme-color", content="#fff"),
    tags$meta(name="apple-mobile-web-app-capable", content="yes"),
    tags$meta(name="mobile-web-app-capable", content="yes"),
    tags$meta(name="apple-touch-fullscreen", content="YES"),
    tags$meta(name="apple-mobile-web-app-title", content="The Tributary"),
    tags$meta(name="application-name", content="The Tributary"),
    tags$meta(property="article:published_time", content="2024-02-22T03:02:59+00:00"),
    tags$meta(property="article:modified_time", content="2024-02-22T03:02:59+00:00"),
    
    tags$link(href="https://fonts.googleapis.com/css2?family=Archivo:ital,wght@0,500;0,600;1,500;1,600&display=swap", rel="stylesheet"),
    
    tags$style(HTML( #css #####
    "
    body {
    font-family: 'Archivo', sans-serif;
    background-color: #fbfdfb;
    padding-inline: 2%;
    margin-inline: 2%;
    margin-block: 1%;
    }

     button.btn-filter.active-filter {
      color: white !important;
    transform: scale(1.1);
    box-shadow: 0 6px 10px rgba(0,0,0,0.2);
    background-color:#ccc;
    }
      .banner {
        background-color: #064875; /* Adjust the background color as needed */
        padding: 10px 0;
        text-align: center;
        height: 10vh;
      }
      img.logo-img {
        height: 80px; /* Adjust the logo size as needed */
      }
        @import url('https://fonts.googleapis.com/css2?family=Archivo:ital,wght@0,500;0,600;1,500;1,600&display=swap');

        h1, h2, h3, h4, h5, h6 { font-family: 'Archivo', sans-serif; color: #064875; text-transform: uppercase; }
        h2{ display: flex;align-items: center;justify-content: center;align-content: center;flex-wrap: wrap;}
        .filter-group { display: flex; flex-wrap: wrap; justify-content: space-evenly; align-items: flex-start; width: 100%;flex-direction: row;align-content: center }
        .filter-section { margin: 5px;flex-basis: calc(33.333% - 10px);box-sizing: border-box;display: flex;flex-direction: column;flex-wrap: wrap;align-content: center;justify-content: center;align-items: center;}
        .btn-filter { margin: 5px; padding: 5px 10px; border: none; border-radius: 4px; cursor: pointer; width: 30%; min-width: 80px; box-sizing: border-box; background-color: #098677; color: white; font-family: 'Archivo', sans-serif; 
        }
        .bill-container, .legislator-profile { border-radius: 7px; margin: 1.5vw; border: 1px solid #8dd4df; background-color: #fbfdfb; }
        .bill-container{padding-inline:2.5vw;padding-block: 0.5vw; display:flex;flex-wrap:nowrap;flex-direction:column;justify-content:space-evenly;}
        .btn-party.R { background-color: #B22234; color: white; }
        .btn-party.D { background-color: #0047AB; color: white; }
        .btn-role.Rep { background-color: #f99a10; color: white; }
        .btn-role.Sen { background-color: #064875; color: white; }
        .btn-final.Y { background-color: #750648; color: white; }
        .btn-final.N { background-color: #355604; color: white; }
.btn-filter.selected {
    filter: saturate(1.5) brightness(1.25) contrast(1.25);
    transform: scale(1.1);
}        .content { display: flex; justify-content: space-evenly; width: 100%; }
        .votes-section { width: 65%; }
        .legislator-profile { border: 1px solid #ddd; padding: 20px; border-radius: 15px; width: 35%; margin-inline: 4%; }
        .vote-link { cursor: pointer; text-decoration: underline; color: blue; }
        .votes {border-color: #8dd4df;  padding: 2px; border-style: solid; border-radius: 7px; margin-block: 5px; }
        .independent-vote {  border-color: #ffc41d; }
        .maverick-vote { border-color: #098677; }
        .regular-vote { border-color: #064875; }
        .profile-filter-section{margin-block: 5%;line-height: 1.5;}
        label#voteType-label {line-height: .95;}
        .vote-details-button { background-color: #064875; color: #fbfdfb; display:flex;justify-content:center;align-items:center;  font-size: 1.5rem; width: auto;}
        .navigation{background-color:#fbf7ed;display: flex;flex-direction: row;flex-wrap: wrap;align-content: center;justify-content: space-evenly;align-items: center;margin-top: 10px;padding-top:10px;}
        label #items_per_page-label{font-size:1.2rem;}
        .items{font-size:1.2rem;}
        .btn-page:hover, .vote-link:hover, .info-button:hover {
    transform: scale(1.05);
    box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        } 
        .container-fluid{padding:0px;}
  div#filterFeedback {
      display: flex;
      align-items: center;
      justify-content: center;
      align-content: center;
      flex-wrap: wrap;
      flex-direction: column;
      padding-inline:5px;
  }
  button#info{font-size:1rem;display:contents;} 
  button#filterinfo{font-size:1rem;display:contents;}
        div .filter-section span{ font-family: 'Archivo', sans-serif; color: #064875; text-transform: uppercase; font-size: 1.7rem;font-weight: bold;}
              div .filter-section label{ font-family: 'Archivo', sans-serif; color: #064875; text-transform: uppercase; font-size: 1.7rem;font-weight: bold;}
              .btn-filter:hover, .btn-page:hover {
    transform: scale(1.05);
    box-shadow: 0 4px 8px rgba(0,0,0,0.2);
  }
              @media (max-width: 768px) {
      .content {
      flex-direction: column-reverse;}
      .legislator-profile{    display: flex;
      flex-direction: column;
      flex-wrap: wrap;
      align-content: center;
      justify-content: center;
      width: 100%;
      margin: 1vw;
      padding: 0;}
    .votes-section{    display: flex;
      flex-direction: column;
      flex-wrap: wrap;
      align-content: center;
      justify-content: center;
      width: 100%;
      margin: 1vw;
      padding: 0;}
              }

div#votefilterinfo {
    padding-inline: 5px;
    }
div#filter-info {
    display: flex;
    align-items: baseline;
    /* margin-bottom: 20px; */
    flex-wrap: wrap;
    flex-direction: row;
    justify-content: center;
    }

.navigation .form-group.shiny-input-container{width:max-content;padding:1px;}
.form-group.shiny-input-container{padding:1px;}
.shiny-input-container:not(.shiny-input-container-inline) {
    width: 300px;
    max-width: 80%;
  }
      body, .shiny-output-error {
        max-width: 100%;
        margin: 0 auto;
        padding: 0 10px;
      }
      @media (min-width: 768px) {
        body, .shiny-output-error {
          max-width: 95%;
        }
      }

  ul.nav-tabs {
    display: flex;
    justify-content: center; /* Centers tabs horizontally */
    flex-wrap: wrap; /* Allows tabs to wrap on smaller screens */
    padding-left: 0; /* Removes default padding */
    margin-bottom: 0; /* Removes default bottom margin */
    list-style: none; /* Removes default list styling */
    background-color: #064875; /* Main: Dark Blue for overall navbar background */
  }
  
  .nav-tabs > li {
    margin: 5px; /* Spacing around each tab */
    border: 3px solid #00204D; /* Alt-Dark Blue for borders, makes tabs distinct */
    border-radius: 10px; /* Rounds the corners for a button-like appearance */
  }
  
  .nav-tabs > li > a {
    display: block; /* Makes the entire tab area clickable */
    padding: 10px 15px; /* Adjusts padding to make tabs larger and more button-like */
    color: #064875; /* Main: Dark Blue for inactive tab text */
    background-color: #FBFBFB; /* Off-white for inactive tab background */
    transition: background-color 0.3s, color 0.3s; /* Smooth transition for hover effect */
    border-radius: 8px; /* Ensures the border-radius matches the <li> for seamless look */
  }
  
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus {
    color: #FBFBFB; /* Off-white for active tab text */
    background-color: #00204D; /* Main: Dark Blue for active tab background */
    border-color: #000; /* Alt-Dark Blue for consistent border */
    }
  
    .nav-tabs > li.active > a{  font-weight:1000;
    font-size:110%;
    }
    .nav-tabs > li > a:hover{  font-weight:500;
    font-size:105%;
    }
    
  .nav-tabs > li > a:hover,
  .nav-tabs > li > a:focus {
    background-color: #098677; /* Teal/Aqua for hovered tab background */
    color: #FBFBFB; /* Off-white for hovered tab text */
    border-color: #8dd4df; /* Light Blue for a soft border on hover */
  }

  /* Adjustments for links within the app */
  a,
  a:hover,
  a:focus {
    color: #8dd4df; /* Light Blue */
  }
  
  /* Additional styles for overall alignment and appearance */
  .container-fluid, .navbar {
    text-align: center; /* Center-aligns elements within the container, if needed */
  }
    "
    )
    )),#banner#####
    div(class="banner",
        tags$a(href="https://jaxtrib.org/", 
               tags$img(src="https://jaxtrib.org/wp-content/uploads/2021/09/TRB_TributaryLogo_NoTagline_White.png", class="logo-img", alt="The Tributary")
        )
    ),
  #tabs#####
  tabsetPanel(
    tabPanel("Voting Patterns Analysis", value = "voting_patterns", app1_ui),
    tabPanel("Legislator Activity Overview", value = "legislator_activity", app2_ui),
    id = "main_nav"
    )
)

server <- function(input, output, session) {
    data <- heatmap_data 

  # App-specific logic
  observeEvent(input$navbarPage == "app1", {
    output$dynamicTitle <- renderUI({
      year <- input$year
      partytext <- if(input$party == "D") "Democrats" else if(input$party == "R") "Republicans" else "All Parties"
      roleTitle <- if(input$role == "Rep") "Florida House" else if(input$role == "Sen") "Florida Senate" else "Florida Legislature"
      fullTitle <- paste0(roleTitle, " Voting Patterns: ", partytext)
      partytext2 <- if(input$party == "D") "Republicans" else if(input$party == "R") "Democrats" else "All Parties"
      color1 <- if(input$party == "D") "Red" else if(input$party == "R") "Blue"
      color1hex <-if(input$party == "D") "#d73027" else if(input$party == "R") "#4575b4"
      color2 <-if(input$party == "D") "Blue" else if(input$party == "R") "Red"
      color2hex <-if(input$party == "D") "#4575b4" else if(input$party == "R") "#d73027"
      HTML(paste0("<h2 style='text-align: center;'>", fullTitle, "</h2>
                <span style='font-size: 14px;line-height:0.5;'>\n<b style ='font-size:1.75rem;color: ",color1hex,";'>",color1," votes</b>: Legislator aligned <i>against</i> most ",partytext," and <i>with</i> most ",partytext2,".</span>
                <span style='font-size: 14px;line-height:0.5;'>\n<b style='font-size:1.75rem;color:",color2hex,";'>",color2," votes</b>: Legislator aligned <i>with</i> most ",partytext, ".</span>","<span style='font-size: 14px;line-height:0.5;'>\n<b style='color: #6DA832;font-size:1.75rem;'>Green votes</b>: Legislator aligned <i>against</i> both parties in bipartisan decisions.<br/><br/>\nBlank spaces indicate the legislators did not vote, either because they weren't assigned to those committees or they missed those votes.\n</span><span style='font-size: 14px;'>Displayed votes exclude ones where all members of a party voted unanimously. The table includes both amendment and bill votes. Data comes from the Florida Legislature's voting records via the Legiscan API.</span>"))
    })
    filteredData <- reactive({
      data <- heatmap_data %>% filter(true_pct!= 1 & true_pct != 0)
      # Apply filters based on input
      if (input$year != "All") {data <- data %>% filter(year == input$year)}
      
      if (input$final != "All") {
        data <- data %>% filter(final == input$final)
      }
      
      if (input$party != "All") {
        data <- data %>% dplyr::filter(party == input$party)
        if (input$party == "D") {
          data <- data %>% dplyr::filter(roll_call_id %in% d_votes$roll_call_id)
        } else if (input$party == "R") {
          data <- data %>% dplyr::filter(roll_call_id %in% r_votes$roll_call_id)
        }
      }
      
      if (input$role != "All") {
        data <- data %>% dplyr::filter(role == input$role)
      }
      return(data)
    })
    output$heatmapPlot <- renderPlotly({
      data <- filteredData()
      # Determine colors based on party
      
      numBills <- n_distinct(data$roll_call_id) # Adjust with your actual identifier
      
      # Dynamic height calculation
      baseHeight <- 500 # Minimum height
      perBillHeight <- 10 # Height per bill
      totalHeight <- baseHeight + (numBills * perBillHeight) # Total dynamic height
      
      
      low_color <- if(input$party == "D") "#4575b4" else if(input$party == "R") "#d73027" else "#4575b4"
      mid_color <- "#6DA832"
      high_color <- if(input$party == "D") "#d73027" else if(input$party == "R") "#4575b4" else "#d73027"
      
      # Generate the plot
      p <- ggplot(data, aes(x = name, y = as.factor(roll_call_id), fill = partisan_metric2, text = hover_text)) +
        geom_tile(color = "white", linewidth = 0.1) +
        scale_fill_gradient2(low = low_color, high = high_color, mid = "#6DA832", midpoint = 1,
        ) +
        theme_minimal() +
        #scale_fill_manual(values = color_mapping) + # Apply the dynamic color mapping
        scale_y_discrete(labels = y_labels) +
        scale_x_discrete(position = "top") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_text(size = 10),
              legend.position = "none",
              plot.title = element_blank(),
              plot.subtitle = element_blank())
      
      ggplotly(p, tooltip = "text") %>%
        layout(autosize=TRUE,xaxis=list(side="top"),
               font = list(family = "Archivo"),
               margin = list(t=85), #list(l = 0, r = 0, t = 60, b = 10),  # Adjust margins to ensure the full title and subtitle are visible,
               # annotations = list(
               #   x = 0.95, 
               #   y = 1.1,  # Position the annotation at the top center
               #   xref = 'paper', 
               #   yref = 'paper',
               #   text = 'Source: Florida Legislature Voting Records via LegiScan. Analysis by Andrew Pantazi',
               #   showarrow = FALSE,
               #   xanchor = 'right',
               #   yanchor = 'bottom',
               #   font = list(size = 11,family="Archivo")  # Adjust font size as needed
               # ) ,
               plot_bgcolor = "rgba(255,255,255,0.85)",  # Transparent plot background
               paper_bgcolor = "rgba(255,255,255,0.85)",
               height = totalHeight
        ) %>% 
        config(displayModeBar = FALSE)
    })
  })
  
  observeEvent(input$navbarPage == "app2",{
                 values <- reactiveValues(party = character(0), role = character(0), final = character(0),selectedYears = list(),  year2023Active = FALSE,
                                        year2024Active = FALSE)
               
               all_legislators <- unique(heatmap_data$name)
               all_legislators_with_all <- c("All" = "All", all_legislators) #not working, also below I changed the filter away from the >0 length to != "all"
               observeEvent(input$btn_year_2023, {
                 values$year2023Active <- !values$year2023Active
               })
               observe({
                 shinyjs::toggleClass("btn_year_2023", "active-filter", values$year2023Active)
               })
               observeEvent(input$btn_year_2024, {
                 values$year2024Active <- !values$year2024Active
               })
               observe({
                 shinyjs::toggleClass("btn_year_2024", "active-filter", values$year2024Active)
               })
               observeEvent(input$btn_year_2023, {
                 if("2023" %in% values$selectedYears) {
                   values$selectedYears <- values$selectedYears[values$selectedYears != "2023"]
                   
                 } else {
                   values$selectedYears <- c(values$selectedYears, "2023")
                 }
               })
               observeEvent(input$btn_year_2024, {
                 if("2024" %in% values$selectedYears) {
                   values$selectedYears <- values$selectedYears[values$selectedYears != "2024"]
                   
                 } else {
                   values$selectedYears <- c(values$selectedYears, "2024")
                 }
               })
               observe({
                 shinyjs::toggleClass("btn_year_2023", "active-filter", values$year2023Active)
                 shinyjs::toggleClass("btn_year_2024", "active-filter", values$year2024Active)
                 # Repeat for other buttons as needed
               })
               output$votefilterinfo <- renderUI({
                 div(
                   actionButton("filterinfo", icon("info-circle"))
                 )
               })
               observeEvent(input$filterinfo, {
                 showModal(modalDialog(
                   title = "Legislator Dashboard",HTML("
        <p>This dashboard allows you to see every vote a lawmaker has taken, highlighting votes where they voted against their own party or against both parties. The data includes every committee, amendment and final vote on a bill. Use the filters to limit legislators by party or chamber. You can also filter the votes so you only see final roll call votes or just those votes against their own party.</p>
      ")
                 ))
               })
               observeEvent(input$btn_party_R, {
                 if ("R" %in% values$party) {
                   values$party <- setdiff(values$party, "R") # Remove R if selected
                 } else {
                   values$party <- c(values$party, "R") # Add R if not selected
                 }
               })
               observeEvent(input$btn_party_D, {
                 if ("D" %in% values$party) {
                   values$party <- setdiff(values$party, "D")
                 } else {
                   values$party <- c(values$party, "D")
                 }
               })
               observeEvent(input$btn_role_Rep, {
                 if ("Rep" %in% values$role) {
                   values$role <- setdiff(values$role, "Rep")
                 } else {
                   values$role <- c(values$role, "Rep")
                 }
               })
               observeEvent(input$btn_role_Sen, {
                 if ("Sen" %in% values$role) {
                   values$role <- setdiff(values$role, "Sen")
                 } else {
                   values$role <- c(values$role, "Sen")
                 }
               })
               observeEvent(input$btn_final_Y, {
                 if ("Y" %in% values$final){
                   values$final <- setdiff(values$final,"Y")
                 } else{values$final <- c(values$final,"Y")
                 }
               })
               observeEvent(input$btn_final_N, {
                 if ("N" %in% values$final){
                   values$final <- setdiff(values$final,"N")
                 } else{values$final <- c(values$final,"N")
                 }
               })
               filtered_data <- reactive({
                 data <- heatmap_data
                 if (length(values$party) > 0) {
                   data <- data %>% filter(party %in% values$party)
                 }
                 if (length(values$role) > 0) {
                   data <- data %>% filter(role %in% values$role)
                 }
                 if (length(values$final) > 0) {
                   data <- data %>% filter(final %in% values$final)
                 }
                 if ("Independent" %in% input$voteType) {
                   data <- data %>% filter(vote_with_neither == 1)
                 }
                 if ("Maverick" %in% input$voteType) {
                   data <- data %>% filter(maverick_votes == 1)
                 }
                 if ("Normal" %in% input$voteType) {
                   data <- data %>% filter(maverick_votes == 0 & vote_with_neither == 0)
                 }
                 if (!is.null(input$searchText) && input$searchText != "") {
                   data <- data %>% filter(grepl(input$searchText, title, fixed = TRUE) | grepl(input$searchText, description, fixed = TRUE))
                 }
                 # Apply legislator filter based on dropdown selection
                 if (!is.null(input$legislator) && input$legislator != "All") {
                   data <- data %>% filter(grepl(input$legislator, name, fixed = TRUE))
                 }
                 
                 if(length(values$selectedYears) > 0) {
                   data <- data %>% filter(session_year %in% values$selectedYears)
                 }
                 
                 data
               })
               filtered_legdata <- reactive({
                 data <- heatmap_data
                 if (length(values$party) > 0) {
                   data <- data %>% filter(party %in% values$party)
                 }
                 if (length(values$role) > 0) {
                   data <- data %>% filter(role %in% values$role)
                 }
                 if (length(values$final) > 0) {
                   data <- data %>% filter(final %in% values$final)
                 }
                 if ("Independent" %in% input$voteType) {
                   data <- data %>% filter(vote_with_neither == 1)
                 }
                 if ("Maverick" %in% input$voteType) {
                   data <- data %>% filter(maverick_votes == 1)
                 }
                 if ("Normal" %in% input$voteType) {
                   data <- data %>% filter(maverick_votes == 0 & vote_with_neither == 0)
                 }
                 if(length(values$selectedYears) > 0) {
                   data <- data %>% filter(session_year %in% values$selectedYears)
                 }
                 # Remember the current selection to possibly reapply it later
                 data
               })
               observe({
                 current_selection <- input$legislator
                 filtered_legislators <- unique(filtered_legdata()$name)
                 
                 if (!is.null(input$legislator_enter_pressed)) {
                   data <- data %>% filter(grepl(input$legislator_enter_pressed, title, fixed = TRUE) | grepl(input$legislator_enter_pressed, description, fixed = TRUE))}
                 else{
                   # Update dropdown choices. This does not inherently cause recursion.
                   updateSelectizeInput(session, "legislator", choices = filtered_legislators,selected = current_selection)
                 }})
               count_independent_votes <- reactive({
                 sum(filtered_data()$vote_with_neither == 1)
               })
               count_maverick_votes <- reactive({
                 sum(filtered_data()$maverick_votes == 1)
               })
               count_normal_votes <- reactive({
                 sum(filtered_data()$maverick_votes == 0 & filtered_data()$vote_with_neither == 0)
               })
               pct_independent_votes <- reactive({
                 round(count_independent_votes()/sum(!is.na(filtered_data()$vote_with_neither)),3)*100
               })
               pct_maverick_votes <- reactive({
                 round(count_maverick_votes()/sum(!is.na(filtered_data()$vote_with_neither)),3)*100
               })
               pct_normal_votes <- reactive({
                 round(count_normal_votes()/sum(!is.na(filtered_data()$vote_with_neither)),3)*100
               })
               observe({
                 current_selections <- input$voteType
                 choices_vector <- c("Independent", "Maverick", "Normal")
                 names(choices_vector) <- c(
                   paste0("Voted Against Both Parties (", count_independent_votes()," - ",pct_independent_votes(), "%)"),
                   paste0("Voted With Other Party (", count_maverick_votes()," - ",pct_maverick_votes(), "%)"),
                   paste0("Voted With Their Party (", count_normal_votes()," - ",pct_normal_votes(), "%)")
                 )
                 
                 updateCheckboxGroupInput(session, "voteType",
                                          #label = "Key Vote Type:",
                                          choices = choices_vector,selected = current_selections)
               })
               output$legislatorProfile <- renderUI({
                 # Assuming 'heatmap_data' contains all the necessary legislator information
                 selected_legislator <- input$legislator
                 if (!is.null(selected_legislator) && selected_legislator != "") {
                   legislator_info <- heatmap_data %>%
                     filter(name == selected_legislator) %>% distinct(name, district, party, role, ballotpedia2) %>% slice(1)
                   div(
                     h3(a(href = legislator_info$ballotpedia2, target = "_blank", selected_legislator)),
                     p("District: ", legislator_info$district),
                     p("Party: ", legislator_info$party),
                     p("Chamber: ", legislator_info$role)
                   )
                 }
               })
               current_page <- reactiveVal(1)
               items_per_page <- reactive({ as.numeric(input$items_per_page) })
               paginated_data <- reactive({
                 if (nrow(filtered_data()) == 0) return(data.frame())
                 start_index <- (current_page() - 1) * items_per_page() + 1
                 end_index <- min(nrow(filtered_data()), start_index + items_per_page() - 1)
                 filtered_data()[start_index:end_index, ]
               })
               output$votesDisplay <- renderUI({
                 data <- paginated_data()
                 if (is.null(input$legislator) || input$legislator == "" || nrow(data) == 0) {
                   return(div(class = "no-data", "No bills available for display."))
                 }
                 
                 # Dynamically create UI elements for each bill
                 ui_elements <- lapply(unique(data$number), function(bill) {
                   bill_data <- data[data$number == bill, ]
                   bill_title <- unique(bill_data$title)[1]
                   descriptions <- unique(bill_data$description)
                   html_string <- paste0("<h4>", bill_title, " - ", "<a href='", bill_data$url[1], "' target='_blank'>", bill_data$number[1],"</a></h4>","<h5>",bill_data$session_name[1],"</h5><p>",bill_data$description[1],"</p>")
                   
                   div(class = 'bill-container',
                       html_content <- HTML(html_string),
                       actionButton(inputId = sprintf('vote-details-link-%s', gsub("[^A-Za-z0-9]", "", bill)), 
                                    label = "Vote Details Info", 
                                    class = "btn btn-info vote-details-button"),
                       div(
                         id = sprintf('vote-details-%s', gsub("[^A-Za-z0-9]", "", bill)),
                         class = 'vote-details',
                         style = 'display:none;',
                         lapply(unique(bill_data$roll_call_id), function(roll_call) {
                           roll_call_data <- bill_data[bill_data$roll_call_id == roll_call, ]
                           
                           pct_yes <- paste0(round(roll_call_data$true_pct[1] * 100, 2), "%")
                           date_format <- format(as.Date(roll_call_data$date),"%b %d, %Y")
                           special_vote_class <- ifelse(roll_call_data$vote_with_neither[1] == 1, "independent-vote",
                                                        ifelse(roll_call_data$maverick_votes[1] == 1, "maverick-vote", "regular-vote"))
                           
                           special_vote_text <- ifelse(roll_call_data$vote_with_neither[1] == 1,
                                                       "This legislator voted <b><i>against</i></b> the majorities of both parties.",
                                                       ifelse(roll_call_data$maverick_votes[1] == 1,
                                                              sprintf("This legislator voted <b><i>against</i></b> the majority of their party (%s) and <b><i>with</i></b> the majority of the other party.", roll_call_data$party[1]),
                                                              "This legislator voted with their party majority."))
                           
                           legislator_vote <- paste(roll_call_data$name[1], "voted", roll_call_data$vote_text[1])
                           
                           div(class = paste("votes",special_vote_class), 
                               HTML(paste0(
                                 "<p>",roll_call_data$desc," - <b>", date_format,"</b></p><p>", pct_yes, " of legislators voted Yea. </p><p>", special_vote_text, "</p><p>", legislator_vote,"</p><p class='disclaimer'><i>This vote wasn't necessarily a vote of the bill, and it could have been a vote on an amendment. For more details, examine the bill's <a href='",roll_call_data$state_link,"' target='_blank'>vote information</a> on the Legislature's website or examine the <a href='", bill_data$url[1], "' target='_blank'>bill page.</a></i></p>"))
                           )
                         }) #close lapply
                       ) #close vote detail div
                   ) #close bill container div
                 }) #close other lapply
                 do.call(tagList, ui_elements)
               }) #close renderui
               observe({
                 if("R" %in% values$party) {
                   runjs('$("#btn_party_R").addClass("selected");')
                 } else {
                   runjs('$("#btn_party_R").removeClass("selected");')
                 }
               })
               observe({
                 if("D" %in% values$party) {
                   runjs('$("#btn_party_D").addClass("selected");')
                 } else {
                   runjs('$("#btn_party_D").removeClass("selected");')
                 }
                 # Repeat for other buttons
               })
               observe({
                 if("Rep" %in% values$role) {
                   runjs('$("#btn_role_Rep").addClass("selected");')
                 } else {
                   runjs('$("#btn_role_Rep").removeClass("selected");')
                 }
                 # Repeat for other buttons
               })
               observe({
                 if("Sen" %in% values$role) {
                   runjs('$("#btn_role_Sen").addClass("selected");')
                 } else {
                   runjs('$("#btn_role_Sen").removeClass("selected");')
                 }
                 # Repeat for other buttons
               })
               observe({
                 if("N" %in% values$final) {
                   runjs('$("#btn_final_N").addClass("selected");')
                 } else {
                   runjs('$("#btn_final_N").removeClass("selected");')
                 }
                 # Repeat for other buttons
               })
               observe({
                 if("Y" %in% values$final) {
                   runjs('$("#btn_final_Y").addClass("selected");')
                 } else {
                   runjs('$("#btn_final_Y").removeClass("selected");')
                 }
               })
               observeEvent(input$prev_page, { if (current_page() > 1) current_page(current_page() - 1) })
               observeEvent(input$next_page, {
                 total_items <- nrow(filtered_data())
                 if ((current_page() * items_per_page) < total_items) current_page(current_page() + 1)
               })
               observeEvent(input$first_page, {
                 current_page(1)
               })
               observeEvent(input$info, {
                 showModal(modalDialog(
                   title = "Vote Types",HTML("
        <p>There are three main ways legislators vote:</p>
        <p>1. They vote with the majority of their own party, even when it means voting with a majority of the opposing party on bipartisan or unanimous votes.</p>
        <p>2. They vote against the majority of legislators in both parties.</p>
        <p>3. Or they vote against the majority of the legislators in their own party AND with the majority of the opposing party.</p>
      ")
                 ))
               })
               observeEvent(input$last_page, {
                 total_pages <- ceiling(nrow(filtered_data()) / items_per_page)
                 current_page(total_pages)
                 shinyjs::toggleState("first_page", condition = current_page() > 1)
                 shinyjs::toggleState("prev_page", condition = current_page() > 1)
                 shinyjs::toggleState("next_page", condition = current_page() < total_pages)
                 shinyjs::toggleState("last_page", condition = current_page() < total_pages)
               })
               output$page_info <- renderUI({
                 total_items <- nrow(filtered_data())
                 start_item <- (current_page() - 1) * items_per_page() + 1
                 end_item <- min(start_item + items_per_page() - 1, total_items)
                 
                 # Using HTML() to include a line break (<br>) or any other HTML tags
                 formatted_start_item <- format(start_item, big.mark = ",")
                 formatted_end_item <- format(end_item, big.mark = ",")
                 formatted_total_items <- format(total_items, big.mark = ",")
                 
                 HTML(paste0("Showing items ", formatted_start_item, " to<br>", formatted_end_item, " of ", formatted_total_items))
               })
               output$filterFeedback <- renderText({
                 filtered_count <- nrow(filtered_data())
                 formatted_count <- format(filtered_count, big.mark = ",")
                 paste("Showing", formatted_count, "results based on current filters.")
               })
               shinyjs::runjs('
    $(document).on("click", ".vote-details-button", function() {
      var detailsId = $(this).attr("id").replace("vote-details-link-", "vote-details-");
      $("#" + detailsId).toggle();
    });
  ')})
}


shinyApp(ui, server)
