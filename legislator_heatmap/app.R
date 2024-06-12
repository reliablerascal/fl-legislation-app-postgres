### heatmap tile ###
# add in filter by legislator & ability to click to vote & click on legislator & filter by vote type
load("data.RData")

library(foreach)
library(shinyjs)
library(plotly)
library(shinyWidgets)
library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(tibble)
library(ggplot2)

heatmap_data <- heatmap_data %>% filter(true_pct!= 1 & true_pct != 0)

ui <- fluidPage(
  tags$head(
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
    tags$style(HTML("
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
body {
    font-family: 'Georgia', serif;
    background-color: #fbfdfb;
    display: flex;
    flex-wrap: wrap;
    width: fit-content;
    padding-inline: 2%;
    margin-inline: 2%;
    margin-block: 1%;
}
h1, h2, h3, h4, h5, h6 { font-family: 'Archivo', sans-serif; color: #064875; text-transform: uppercase; }
        h2{ display: flex;align-items: center;justify-content: center;align-content: center;flex-wrap: wrap;}
        .filter-group { display: flex; flex-wrap: wrap; justify-content: space-evenly; align-items: center; width: 100%;flex-direction: row;align-content: center }
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
  button#view_liberal_conservative {
    margin-bottom: 10px;
    width:100%;
    font-size: 14px;
    text-transform: uppercase;
    font-family: 'Archivo', sans-serif;
      white-space: normal; /* Ensure that whitespace and breaks work as normal */
  text-align: center; /* Center the text */
  background-color:#23707C;
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
.form-group.shiny-input-container{width:max-content;padding:1px;}
      "))
  ),
  div(class="banner",
      tags$a(href="https://jaxtrib.org/", 
             tags$img(src="https://jaxtrib.org/wp-content/uploads/2021/09/TRB_TributaryLogo_NoTagline_White.png", class="logo-img", alt="The Tributary")
      )
  ),
  uiOutput("dynamicTitle"),
  div(class="filter-row",style="display:flex; justify-content: space-evenly;margin-top:1.5vw; margin-bottom: 0px;padding-bottom:0px;margin-left:10vw;margin-right:10vw;",
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



server <- function(input, output) {
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
    data <- heatmap_data 
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
  
}

shinyApp(ui, server)
