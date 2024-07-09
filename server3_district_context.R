# SERVER_DISTRICT_CONTEXT.R
# This app shows demographic and electoral political leaning characteristics of each district compared to legislators' voting records

########################################
#                                      #  
# app 3: district context              #
#                                      #
########################################

# App-specific logic
observeEvent(input$navbarPage == "app3", {

  ########################################
  #                                      #  
  # Header- methodology and legend       #
  #                                      #
  ########################################   
  output$dynamicHeader3 <- renderUI({
    HTML(paste0(
      '<h2 style="text-align: center;">(DEV) Representation Alignment for <br>', input$chamber, ' district ', input$district, '</h2>',
      '<div align="left">',
      'This page displays each legislator\'s partisan leanings compared to their district\'s voting record and demographics.'
    ))
  })
  
  ##############################
  #                            #  
  # USER FILTER                #
  #                            #
  ##############################
  #define the standard filter box
  createFilterBox <- function(inputId, label, choices, selected = NULL) {
    div(
      selectInput(inputId, label, choices = choices, selected = selected)
    )
  }
  
  output$dynamicFilters3 <- renderUI({
    div(class = "filter-row",
        style = "display:flex; flex-wrap: wrap; justify-content: center; margin-top:1.5vw; margin-bottom: 0px; padding-bottom:0px; margin-left:auto; margin-right:auto;",
        
        createFilterBox("chamber", "Select Chamber:", c("House", "Senate"), selected = "House"),
        createFilterBox("district", "Select District:", 1:120, selected = 1)
    )
  })
  
  # Observe the chamber selection and update the district options accordingly
  observeEvent(input$chamber, {
    if (input$chamber == "House") {
      updateSelectInput(session, "district", choices = 1:120, selected = 1)
    } else if (input$chamber == "Senate") {
      updateSelectInput(session, "district", choices = 1:40, selected = 1)
    }
  })
  
  data_filtered <- reactive({
    req(input$chamber, input$district)  # Ensure inputs are available
    data <- app03_district_context
    
    data <- data %>%
      filter(
        chamber == input$chamber,
        district_number == input$district
        )
    
    return (list(data = data, is_empty = nrow(data) == 0))
  })
  
  ########################################
  #                                      #  
  # Display demographics                 #
  #                                      #
  ########################################   
  # Reactive subset of app03_district_context based on input$chamber and input$district
  qry_demo_district <- reactive({
    req(input$chamber, input$district)
    app03_district_context %>%
      filter(chamber == input$chamber & district_number == input$district)
  })
  
  # Reactive state data (assuming only one record in app03_district_context_state)
  qry_demo_state <- reactive({
    app03_district_context_state
  })
  
  output$dynamicDemographics <- renderUI({
    tagList(
      HTML(paste0(
        '<h4 style="text-align: center;">District Demographics</h4>'
      )),
      plotOutput("demographicsPlot")
    )
  })
  
  output$demographicsPlot <- renderPlot({
    req(qry_demo_district, qry_demo_state)
    demo_district <- qry_demo_district()
    demo_state <- qry_demo_state()
    
    # need to have 5x2 for each of category, percent, and demographic
    data <- data.frame(
      Category = factor(rep(c("District", "State"), 5), levels = c("State", "District")),  # Reverse factor levels
      Percent = c(
        demo_district$pct_white, demo_state$pct_white,
        demo_district$pct_black, demo_state$pct_black,
        demo_district$pct_asian, demo_state$pct_asian,
        demo_district$pct_hispanic, demo_state$pct_hispanic,
        demo_district$pct_pacific, demo_state$pct_pacific
      ),
      Demographic = rep(c("White", "Black", "Asian", "Hispanic", "Pacific"), each = 2)
    )
    
    create_plot <- function(demo) {
      ggplot(subset(data, Demographic == demo), aes(x = Category, y = Percent, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge", width=1) +
        scale_fill_manual(values = c("District" = "#17becf", "State" = "#7f7f7f")) +
        scale_y_continuous(labels = percent_format(),, limits = c(0, 1)) +
        labs(title = demo, x = "", y = "") +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()   # Remove minor grid lines
        ) +
        coord_flip()
    }
    
    # Create plots for each demographic
    plot_white <- create_plot("White")
    plot_black <- create_plot("Black")
    plot_asian <- create_plot("Asian")
    plot_hispanic <- create_plot("Hispanic")
    plot_pacific <- create_plot("Pacific")
    
    # Combine plots using patchwork
    plot_white / plot_black/ plot_asian/ plot_hispanic / plot_pacific
  })
  
  

  
# END OBSERVER EVENT  
})