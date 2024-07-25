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
library(shinydisconnect) # not sure why this is needed here AND in app.R, but it prevents Error in c_disconnect_message() : could not find function "c_disconnect_message"

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
    background = "#ffcccc",
    colour = "#ff0000",
    size = 24,
    overlayColour = "#ffffff",
    overlayOpacity = 0.75,
    top = "center",
    refreshColour = "#0000ff"
  )
}

#####################
#                   #  
# app 1A            #
#                   #
#####################
# Define the UI for App 1 ####

# tags$script(HTML("
#       Shiny.setInputValue('is_mobile', /iPhone|iPad|iPod|Android/i.test(navigator.userAgent));
#     "))),
#check for mobile access, so we can simplify heatmap accordingly
app1_ui <- fluidPage( 
  tags$head( 
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), 
    tags$script(src = "https://cdn.jsdelivr.net/npm/mobile-detect@1.4.5/mobile-detect.min.js"), # Include MobileDetect.js 
    tags$script(HTML("
      $(document).on('shiny:connected', function(event) {
        var md = new MobileDetect(window.navigator.userAgent);
        if (md.mobile()) {
          Shiny.setInputValue('isMobile', true);
        } else {
          Shiny.setInputValue('isMobile', false);
        }
      });
    "))
  ),
  c_disconnect_message(), 
  uiOutput("dynamicHeader"), 
  uiOutput("staticMethodology1"), 
  uiOutput("dynamicFilters"), 
  uiOutput("dynamicLegend"), 
  uiOutput("dynamicRecordCount"), 
  uiOutput("noDataMessage"), 
  plotlyOutput("heatmapPlot") 
)

# app1_ui <- fluidPage(
#   tags$head(
#     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
#     tags$script("
#       Shiny.setInputValue('is_mobile', /iPhone|iPad|iPod|Android/i.test(navigator.userAgent));
#     ")),
#   c_disconnect_message(),
#   uiOutput("dynamicHeader"),
#   uiOutput("staticMethodology1"),
#   uiOutput("dynamicFilters"),
#   uiOutput("dynamicLegend"),
#   uiOutput("dynamicRecordCount"),
#   uiOutput("noDataMessage"),
#   plotlyOutput("heatmapPlot")
# )



###########################
#                         #  
# app 3 district context  #
#                         #
###########################

app3_ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://mockingbird.shinyapps.io/fl-leg-app-postgres/styles.css")),
  c_disconnect_message(),
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
      selected = "app1" #start on this app by default
      )
  )
)