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
    data_district <- qry_demo_district()
    
    HTML(paste0(
      '<h2 style="text-align: center;">Representation Alignment for ',
      '<br>', data_district$legislator_name, ' (', data_district$party, ')',
      '<br>', input$chamber, ' district ', input$district, '</h2>',
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
        createFilterBox("district", "Select District:", 1:120, selected = 1),
        #createFilterBox("legislator", "Select Legislator:", c("Adam Anderson", "Adam Botana"), selected = 1),
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
  
  ########################################
  #                                      #  
  # comparative partisanship             #
  #                                      #
  ######################################## 
  
  output$dynamicPartisanship <- renderUI({
    data_district <- qry_demo_district()
    
    n_districts <- if (data_district$chamber == "House") {
      120
    } else if (data_district$chamber == "Senate") {
      40
    }
    
    same_party <- if (data_district$party == "R") {
      "Republican"
    } else if (data_district$party == "D") {
      "Democrat"
    }
    
    count_legislators_in_party <- function(data, party, chamber) {
      data %>%
        filter(party == !!party, chamber == !!chamber) %>%
        tally()
    }
    
    n_legislators_in_party <- if (same_party == "Republican") {
      count_legislators_in_party(app03_district_context, "R", input$chamber)$n
    } else if (same_party == "Democrat") {
      count_legislators_in_party(app03_district_context, "D", input$chamber)$n
    }
    
    rank_dist <- if (data_district$party == "R") {
      data_district$rank_partisan_dist_R
    } else if (data_district$party == "D") {
      data_district$rank_partisan_dist_D
    }
    
    rank_leg <- if (data_district$party == "R") {
      data_district$rank_partisan_leg_R
    } else if (data_district$party == "D") {
      data_district$rank_partisan_leg_D
    }
    
    tagList(
      HTML(paste0(
    '<h4 style="text-align: left;">Legislative Voting vs. Population Voting</h4>',
    '<div style="text-align: left;">',
    'This district is ranked #<span style="font-size: 1.5em;">', rank_dist, '</span> most ', same_party,
    '-leaning of <span style="font-size: 1.5em;">', n_districts, '</span> ', data_district$chamber, ' districts.<br>',
    'Voting preferences (based on a composite of 2016 Presidential, 2018 Gubernatorial, and 2020 Presidential elections):<br>',
    '<span style="font-size: 1.5em;">', percent(data_district$pct_R), '</span> Republican<br>',
    '<span style="font-size: 1.5em;">', percent(data_district$pct_D), '</span> Democrat<br>',
    '<br>',
    'In comparison, <span style="font-size: 1.5em;">', data_district$legislator_name, '\'s</span> voting record is ranked #<span style="font-size: 1.5em;">', rank_leg,
    '</span> most ',same_party , '-leaning amongst <span style="font-size: 1.5em;">', n_legislators_in_party, '</span> ', input$chamber, ' legislators in the ', same_party, ' party.',
    '<hr></div>'
      ))
    )
  })
  
  
  ########################################
  #                                      #  
  # demographics                         #
  #                                      #
  ########################################   
  
  output$dynamicDemographics <- renderUI({
    tagList(
      HTML(paste0(
        '<h4 style="text-align: left;">District Demographics</h4>'
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