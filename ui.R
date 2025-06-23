#UI.R
#
# Defines the layout and appearance of the application.
# Specifies user input controls (e.g., text inputs, sliders, drop-downs).
# Sets up placeholders for outputs (e.g., tables, plots, text).
#
# 5/20/25 RR
# separated this section into its own script, but kept Andrew's code intact
# none of this is dependent on data

library(shinythemes)
library(shinyjs)
library(DT)
library(plotly)
library(shinyWidgets)
library(bslib)
library(shiny)
library(shinydisconnect)

verbatimTextOutput("debug_output")

#########################
#                       #  
# Global functions      #
#                       #
#########################
# perplexingly, I had to put this here (and not in app.R) to prevent "Error in c_disconnect_message() : could not find function "c_disconnect_message""... despite defining it BEFORE sourcing ui.r

c_disconnect_message <- function() {
  disconnectMessage(
    text = "Your session has been disconnected due to inactivity. Please refresh the page.",
    refresh = "Refresh",
    background = "#fbfdfb",  # Off-white background for approachability
    colour = "#064875",       # Dark blue text color for clarity and emphasis
    size = 24,                # Text size remains 24 for readability
    overlayColour = "#00204D", # Alt-dark blue for the overlay
    overlayOpacity = 0.75,    # 75% opacity for subtle overlay effect
    top = "center",           # Centered message
    refreshColour = "#f99a10" # Orange color for refresh button to grab attention
  )
}
#####################
#                   #  
# about tab    #
#                   #
##################### 
aboutTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    # You can style this as needed
    div(class = "about-section",
        uiOutput(ns("aboutText"))
    )
  )
}

#####################
#                   #  
# app 1A            #
#                   #
#####################
# Define the UI for App 1 ####

`app1_ui` <- tagList( 
  tags$head( 
    tags$style(HTML("
      .swipe-right {
        display: none;
        font-size: 1.2rem;
        color: #888;
        position: fixed;
        bottom: 10%;
        left: 50%;
        transform: translateX(-50%);
        background-color: rgba(255, 255, 255, 0.9);
        padding: 8px 15px;
        border-radius: 8px;
        z-index: 1000;
        text-align: center;
      }
      @media (max-width: 768px) {
        .swipe-right {
          display: block;
        }
      }
    ")),
    tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      var swipeRight = document.querySelector('.swipe-right');
      var plotlyDiv = document.querySelector('.plotly');
      
      // Hide the message when the user scrolls
      plotlyDiv.addEventListener('scroll', function() {
        swipeRight.style.display = 'none';
        clearTimeout(plotlyDiv.stickyTimeout);
        plotlyDiv.stickyTimeout = setTimeout(function() {
          swipeRight.style.display = 'block';
        }, 1000);
      });
    });
  ")),
    #tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"), 
    
    tags$script(src = "https://cdn.jsdelivr.net/npm/mobile-detect@1.4.5/mobile-detect.min.js"), # Include MobileDetect.js 
    tags$script(HTML("
       $(document).on('shiny:connected', function(event) {
        var md = new MobileDetect(window.navigator.userAgent);
        Shiny.setInputValue('isMobile', !!md.mobile());
      });
    "))
  ),
  uiOutput("dynamicHeader"), 
  uiOutput("dynamicFilters"), 
  uiOutput("dynamicLegend"), 
  uiOutput("dynamicRecordCount"), 
  uiOutput("noDataMessage"), 
  plotlyOutput("heatmapPlot",width = "100%", height = "auto"),
  
  uiOutput("staticMethodology1")
)

###########################
#                         #  
# app 3 district context  #
#                         #
###########################

app3_ui <- tagList(
  tags$head(
    tags$script(HTML(
      "
    $(document).on('hidden.bs.modal', function () {
      $('body').css('overflow', 'auto');
    });
    "
    ))
  ),
  column(12, uiOutput("dynamicHeader3")),
  column(12, uiOutput("dynamicFilters3")),
  column(12, uiOutput("dynamicContextComparison")),
  votingHistoryUI("votingHistory"),
  fluidRow(column(12, uiOutput("staticMethodology3")))
)

###########################

#                         #  

# app 4 partisan scatterplot #

#                         #

###########################
app4_ui <- 
  tagList(
    div(class = "header-tab", "Legislator vs District Partisanship"),
    sidebarLayout(
      sidebarPanel(
        div(class = "filter-row query-input",
            selectInput("chamber4", "Select Chamber:", 
                        choices = c("All", "House", "Senate"),
                        selected = "All"),
            selectInput("party4", "Select Party:", 
                        choices = c("All", "D", "R"),
                        selected = "All")
        )
      ),
      mainPanel(
        plotlyOutput("scatterplot"),
        htmlOutput("medianInfo"),
        htmlOutput("explanationText")
      )
    )
  )


#####################
#                   #  
# navbar page       #
#                   #
#####################

# Combine the UIs into a navbarPage ####
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  c_disconnect_message(),
  
  
  #theme = shinytheme("flatly"),
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Florida Legislative Compass: How Lawmakers Vote"),
    tags$link(rel = "icon", type = "image/png", href = "https://legislative-compass.s3.us-east-2.amazonaws.com/favicon.png"),
    tags$link(rel = "shortcut icon", href = "https://legislative-compass.s3.us-east-2.amazonaws.com/favicon.ico"),
    tags$meta(name = "robots", content = "index, follow, max-image-preview:large, max-snippet:-1, max-video-preview:-1"),
    tags$meta(name = "description", content = "Explore the interactive dashboard for insights into the Florida Legislature's voting patterns."),
    tags$meta(property = "og:locale", content = "en_US"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:title", content = "Florida Legislative Compass: How Lawmakers Vote"),
    tags$meta(property = "og:description", content = "Explore the interactive dashboard for insights into the Florida Legislature's voting patterns."),
    tags$meta(property = "og:url", content = "https://apantazi.shinyapps.io/FLLegislativeCompass/"),
    tags$meta(property = "og:site_name", content = "Legislative Compass"),
    tags$meta(property = "og:image:type", content = "image/png"),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(property = "og:image", content = "https://legislative-compass.s3.us-east-2.amazonaws.com/az-preview.png"),
    tags$meta(name = "twitter:image", content = "https://legislative-compass.s3.us-east-2.amazonaws.com/az-preview.png"),
    tags$meta(name = "theme-color", content = "#fff"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-touch-fullscreen", content = "YES"),
    tags$meta(property = "article:published_time", content = "2024-02-22T03:02:59+00:00"),
    tags$meta(property = "article:modified_time", content = "2024-02-22T03:02:59+00:00"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Archivo:ital,wght@0,500;0,600;1,500;1,600&display=swap", rel = "stylesheet"),
    #tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
    tags$link(rel = "stylesheet", type = "text/css", href = "https://legislative-compass.s3.us-east-2.amazonaws.com/styles.css")
  ),
  tags$script(src = "https://cdn.jsdelivr.net/npm/mobile-detect@1.4.5/mobile-detect.min.js"),
  tags$script(HTML("
    $(document).on('shiny:connected', function(event) {
      var md = new MobileDetect(window.navigator.userAgent);
      Shiny.setInputValue('isMobile', !!md.mobile());
    });
  ")),
  # Banner #####
  div(class = "banner",
      HTML('        <svg width="650" height="180" viewBox="0 0 650 180" fill="none" xmlns="http://www.w3.org/2000/svg">
          <text x="20" y="70" font-family="Archivo Black, Archivo, Arial Black, Arial, sans-serif" font-size="60" font-weight="bold" fill="white" letter-spacing="2">LEGISLATIVE</text>
          <text x="20" y="140" font-family="Archivo Black, Archivo, Arial Black, Arial, sans-serif" font-size="60" font-weight="bold" fill="white" letter-spacing="2">COMPASS</text>
          <g transform="translate(530,90)">
            <polygon points="0,-62 12,-32 0,-18 -12,-32" fill="#E74C3C"/>
            <polygon points="0,62 12,32 0,18 -12,32" fill="white"/>
            <polygon points="62,0 32,12 18,0 32,-12" fill="#5DADE2"/>
            <polygon points="-62,0 -32,12 -18,0 -32,-12" fill="white"/>
            <polygon points="44,-44 28,-16 0,-18 16,-28" fill="white"/>
            <polygon points="44,44 28,16 0,18 16,28" fill="white"/>
            <polygon points="-44,44 -16,28 0,18 -28,16" fill="#5DADE2"/>
            <polygon points="-44,-44 -16,-28 0,-18 -28,-16" fill="white"/>
            <circle cx="0" cy="0" r="10" fill="#1A2D49"/>
          </g>
        </svg>')
  ),
  
  
  
  
  #####################
  #                   #  
  # navigation bar    #
  #                   #
  #####################
  div(class="navbar2",
      tabsetPanel(
        tabPanel("About", value = "about", aboutTabUI("about")),
        
        tabPanel("Voting Patterns", value = "app1", app1_ui),
        tabPanel("Legislator Lookup", value = "app3", app3_ui),
        tabPanel("Partisanship Scatterplot", value = "app4",app4_ui),
        id = "navbar_page",
        selected = "app1"
      )
  )
)