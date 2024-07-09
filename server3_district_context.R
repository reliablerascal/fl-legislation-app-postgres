# SERVER_DISTRICT_CONTEXT.R
# This app shows demographic and electoral political leaning characteristics of each district compared to legislators' voting records

########################################
#                                      #  
# app 3: district context              #
#                                      #
########################################

# App-specific logic
observeEvent(input$navbarPage == "app3", {
  
  print("app3")
  ########################################
  #                                      #  
  # Header- methodology and legend       #
  #                                      #
  ########################################   
  output$dynamicHeader3 <- renderUI({
    HTML(paste0(
      '<h2 style="text-align: center;">(DEV) Representation Alignment for ', input$chamber, ' district ', input$district, '</h2>',
      '<div align="left">',
      'This page displays each legislator\'s partisan leanings compared to their district\'s voting record and demographics.'
    ))
  })
  
  
  
  ##############################
  #                            #  
  # USER FILTER                #
  #                            #
  ##############################
  createFilterBox <- function(inputId, label, choices, selected = NULL) {
    div(
      selectInput(inputId, label, choices = choices, selected = selected)
    )
  }
  
  output$dynamicFilters3 <- renderUI({
    div(class = "filter-row",
        style = "display:flex; flex-wrap: wrap; justify-content: center; margin-top:1.5vw; margin-bottom: 0px; padding-bottom:0px; margin-left:auto; margin-right:auto;",
        
        createFilterBox("chamber", "Select Chamber:", c("House", "Senate")),
        createFilterBox("district", "Select District:", c(1,2,3,4,5), selected = 1)
    )
  })
  
  data_filtered <- reactive({
    req(input$chamber, input$district)  # Ensure inputs are available
    data <- app03_district_context
    
    data <- data %>%
      filter(
        chamber == input$chamber,
        district_number = input$distrct
        )
    
    return (list(data = data, is_empty = nrow(data) == 0))
  })
  
  
  

  
# END OBSERVER EVENT  
})