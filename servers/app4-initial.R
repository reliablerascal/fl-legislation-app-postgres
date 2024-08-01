library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
#app03_district_context <- read_csv("c:/Users/Andrew/documents/fl-legislation-etl/data-app/app03_district_context.csv")


# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://raw.githubusercontent.com/reliablerascal/fl-legislation-app-postgres/master/www/styles.css")
  ),
  titlePanel("Legislator vs District Partisanship"),
  sidebarLayout(
    sidebarPanel(
      selectInput("chamber", "Select Chamber:", 
                  choices = c("All", "House", "Senate")),
      selectInput("party", "Select Party:", 
                  choices = c("All", "D", "R"))
    ),
    mainPanel(
      plotlyOutput("scatterplot"),
      htmlOutput("medianInfo"),
      htmlOutput("explanationText")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- app03_district_context
    if (input$chamber != "All") {
      data <- data %>% filter(chamber == input$chamber)
    }
    if (input$party != "All") {
      data <- data %>% filter(party == input$party)
    }
    data
  })
  
  output$scatterplot <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = avg_party_lean_points_R, y = leg_party_loyalty, 
                          color = party, 
                          text = paste0(legislator_name, 
                                       "<br>District Lean: ", round(avg_party_lean_points_R, 2),
                                       "<br>Party Loyalty: ", round(leg_party_loyalty, 3),
                                       "<br>",chamber," District ",district_number))) +
      geom_point() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_color_manual(values = c("D" = "#4575b4", "R" = "#d73027")) +
      labs(x = "District Partisan Lean (R+)", 
           y = "Legislator Party Loyalty",
           color = "Party") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  
  output$medianInfo <- renderUI({
    data <- filtered_data()
    median_district <- median(data$avg_party_lean_points_R)
    median_loyalty <- median(data$leg_party_loyalty)
    
    HTML(paste("Median District Lean:", round(median_district, 2),
               "<br>Median Party Loyalty:", round(median_loyalty, 2)))
  })
  
  output$explanationText <- renderUI({
    HTML("<hr><div class='header-section'>Methodology</div>
         <p><strong>Party Loyalty:</strong> Calculated as the proportion of a legislator's votes that align with their party's majority. A score of 1 indicates perfect alignment with the party, while 0 indicates consistent voting against the party.</p>
         <p><strong>District Partisan Lean:</strong> Calculated from the average of selected election results (presidential and gubernatorial races). Positive values indicate a Republican lean, negative values a Democratic lean.</p>")
  })
}

# Run the app
shinyApp(ui = ui, server = server)