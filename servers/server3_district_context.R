# SERVER3_DISTRICT_CONTEXT.R
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
      '<div class = "header-tab-small">Representation Alignment for </div>',
      '<h2>', data_district$legislator_name, ' (', data_district$party, ')</h2>',
      '<h3>', input$chamber, ' district ', input$district, '</h3>',
      '<div align="left">',
      'This tab displays each legislator\'s partisan leanings compared to their district\'s voting record and demographics.',
      'The intended audience includes prospective voters in <a href="https://ballotpedia.org/Florida_elections,_2024#Offices_on_the_ballot">Florida\'s primary election on August 20</a>.<br>'
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
    div(class = "filter-row query-input",
        createFilterBox("chamber", "Select Chamber:", c("House", "Senate"), selected = "House"),
        createFilterBox("district", "Select District:", 1:120, selected = 1)
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
    '<div class="header-section">Legislative Voting vs. Population Voting</div>',
    '<div align="left">',
    '<span class="stat-bold">', data_district$legislator_name, '\'s</span> voting record is ranked #<span class="stat-bold">', rank_leg,
    '</span> most ',same_party , '-leaning amongst <span class="stat-bold">', n_legislators_in_party, '</span> ', input$chamber, ' legislators in the ', same_party, ' party.',
    '<br>',
    'In comparison, this district is ranked #<span class="stat-bold">', rank_dist, '</span> most ', same_party,
    '-leaning of <span class="stat-bold">', n_districts, '</span> ', data_district$chamber, ' districts:<br>',
    '<span class="stat-bold">', percent(data_district$pct_R), '</span> Republican<br>',
    '<span class="stat-bold">', percent(data_district$pct_D), '</span> Democrat<br>',
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
        '<div class = "header-section">District Demographics</div>'
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
      Category = factor(rep(c("District", "State"), 4), levels = c("State", "District")),  # Reverse factor levels
      Percent = c(
        demo_district$pct_white, demo_state$pct_white,
        demo_district$pct_black, demo_state$pct_black,
        demo_district$pct_asian, demo_state$pct_asian,
        demo_district$pct_hispanic, demo_state$pct_hispanic
        # demo_district$pct_napi, demo_state$pct_napi
      ),
      Demographic = rep(c("White", "Black", "Asian", "Hispanic"), each = 2)
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
    # plot_napi <- create_plot("Pacific")
    
    # Combine plots using patchwork
    plot_white / plot_black/ plot_asian/ plot_hispanic
  })
  
  ########################################
  #                                      #  
  # Footer- methodology                  #
  #                                      #
  ########################################   
  output$staticFooter3 <- renderUI({
    HTML(paste0(
      '<hr>',
      '<div class="header-section">Methodology</div>',
      '<div class="methodology-notes">',
      'Legislator partisanship is calculated across all legislative sessions in 2023 and 2024, as a weighted average of votes with party/against oppo (0) and against party/with oppo (1), excluding votes with both parties or against both parties.<br>',
      'District partisanship is calculated based on voting in the 2016 Presidential, 2018 Gubernatorial, and 2020 Presidential elections.<br>',
      '<strong>Data sources:</strong>',
      '<ul>',
      '<li>Legislator voting info from <a href="https://legiscan.com/FL/datasets">LegiScan\'s Florida Legislative Datasets for all 2023 and 2024 Regular Session</a>.<br>',
      '<li>District demographics and election results curated by <a href="https://davesredistricting.org/maps#state::FL">Dave\'s Redistricting</a>.',
      '</ul>',
      'For details on wishlist items and work in progress, see <a href="https://docs.google.com/document/d/1e3KDrnpXjKL4OJqFR49hqti77TntPRL7k4AkqSfsefU/edit"><strong>development notes</strong></a>.',
      '<br></div>'
    ))
  })

  
# END OBSERVER EVENT  
})