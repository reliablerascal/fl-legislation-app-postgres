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
  # uiOutput("dynamicTitle"),
  # uiOutput("methodology"),
  uiOutput("dynamicLegend"),
  uiOutput("dynamicFilters"),
  uiOutput("dynamicRecordCount"),
  uiOutput("noDataMessage"),
  plotlyOutput("heatmapPlot"#, height = "150vh")
  )
)
  # fluidRow tries to prevent plot from overwriting footer, but it fails when switching filters creates different recordset sizes
  # fluidRow(
  #   column(12, plotlyOutput("heatmapPlot", height = "150vh"))
  # ),
  # fluidRow(
  #   column(12, uiOutput("methodology"))
# )



#####################
#                   #  
# app 2 HOLD        #
#                   #
#####################
# Define the UI for App 2 ####
app2_ui <- fluidPage(
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
  
  #####################
  #                   #  
  # navigation bar    #
  #                   #
  #####################
  tabsetPanel(
    tabPanel("Voting Patterns Analysis", value = "voting_patterns", app1_ui),
    #tabPanel("Voting Patterns Analysis B", value = "voting_patterns2", app1b_ui),
    #tabPanel("Legislator Activity Overview", value = "legislator_activity", app2_ui),
    id = "main_nav"
  )
)