# SERVER3_DISTRICT_CONTEXT.R
# This app shows demographic and electoral political leaning characteristics of each district compared to legislators' voting records

########################################
#                                      #  
# app 3: district context              #
#                                      #
########################################
source("servers/voting_history_module.R")
library(patchwork)
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
      '<h3>', data_district$chamber, ' district ', data_district$district, '</h3>',
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
    sort_legislators_by_last_name <- function(legislators) {
      # Extract last names assuming legislator_name is in "First Last" format
      last_names <- sapply(strsplit(legislators, " "), function(x) x[length(x)])
      sorted_indices <- order(last_names)
      legislators[sorted_indices]
    }
    
    legislators <- unique(app03_district_context$legislator_name)
    sorted_legislators <- sort_legislators_by_last_name(legislators)
    sorted_data <- app03_district_context  %>%
      arrange(match(legislator_name, sorted_legislators))
    
    div(class = "filter-row query-input",
        radioButtons("filter_method", "Filter By:", choices = c("Legislator Name", "District"), selected = "Legislator Name"),
                     #choices = c( "District","Legislator Name"), selected = "District"),
        conditionalPanel(
          condition = "input.filter_method == 'District'",
          createFilterBox("chamber3", "Select Chamber:", c("House", "Senate"), selected = "House"),
          createFilterBox("district3", "Select District:", 1:120, selected = 1)
        ),
        conditionalPanel(
          condition = "input.filter_method == 'Legislator Name'",
          selectInput("legislator3", "Select Legislator:", choices = sorted_legislators, selected = sorted_legislators[1])
        )
    )
  })
  
  
  # Observe the chamber selection and update the district options accordingly
  observeEvent(input$chamber3, {
    if (input$chamber3 == "House") {
      updateSelectInput(session, "district3", choices = 1:120, selected = 1)
    } else if (input$chamber3 == "Senate") {
      updateSelectInput(session, "district3", choices = 1:40, selected = 1)
    }
  })
  
  selected_legislator_chamber <- reactive({
    req(input$filter_method == "Legislator Name")
    data <- app03_district_context
    leg_data <- data[data$legislator_name == input$legislator3, ]
    return(leg_data$chamber)
  })
  
  observeEvent(input$legislator3, {
    selected_legislator_chamber <- selected_legislator_chamber()
    updateSelectInput(session, "chamber3", selected = selected_legislator_chamber)
  })
  
  # Reactive subset of app03_district_context based on input$chamber3 and input$district3, or input$legislator3
  qry_demo_district <- reactive({
    req(input$filter_method)
    data <- app03_district_context
    
    if (input$filter_method == "District") {
      req(input$chamber3, input$district3)  # Ensure inputs are available
      data <- data[data$chamber == input$chamber3 & data$district_number == input$district3, ]
    } else if (input$filter_method == "Legislator Name") {
      req(input$legislator3)
      data <- data[data$legislator_name == input$legislator3, ]
    }
    
    return (data)
  })
  
  count_legislators_in_party <- function(data, party, chamber) {
    data %>%
      filter(party == !!party, chamber == !!chamber) %>%
      tally()
  }
  
  ########################################
  #                                      #  
  # comparative partisanship             #
  #                                      #
  ######################################## 
  
  output$helper3_party_loyalty <- renderUI({
    data_district <- qry_demo_district()
    same_party <- if (data_district$party == "R") "Republican" else "Democrat"
    n_legislators_in_party <- if (same_party == "Republican") {
      count_legislators_in_party(app03_district_context, "R", input$chamber3)$n
    } else {
      count_legislators_in_party(app03_district_context, "D", input$chamber3)$n
    }
    rank_leg <- if (data_district$party == "R") {
      data_district$rank_partisan_leg_R
    } else if (data_district$party == "D") {
      data_district$rank_partisan_leg_D
    }
    
    HTML(paste0(
      '<div class="flex-item legislative-voting">',
      '<h3 class="flex-header-section">LEGISLATIVE VOTING</h3>',
      '<h4 class="legislator-name">', data_district$legislator_name, '\'S VOTING RECORD:</h4>',
      '<ul class="main-list">',
      '<li>Ranked #<span class="stat-bold">', rank_leg, '</span> most loyal out of <span class="stat-bold">', n_legislators_in_party, '</span> ', input$chamber3, ' ', same_party, 's</li>',
      '<li>Party loyalty calculated from <span class="stat-bold">', data_district$leg_n_votes_denom_loyalty, '</span> key votes:',
      '<ul>',
      '<li>Party-Line Votes: <span class="stat-bold">', data_district$leg_n_votes_party_line_partisan, '</span> ', 
      '<span class="percentage">', percent(data_district$leg_n_votes_party_line_partisan/data_district$leg_n_votes_denom_loyalty, accuracy = 0.1), '</span></li>',
      '<li>Cross-Party Votes: <span class="stat-bold">', data_district$leg_n_votes_cross_party, '</span> ',
      '<span class="percentage">', percent(data_district$leg_n_votes_cross_party/data_district$leg_n_votes_denom_loyalty, accuracy = 0.1), '</span></li>',
      '</ul></li>',
      '<li>Additional voting data (not in loyalty calculation):',
      '<ul>',
      '<li>Bipartisan votes: <span class="stat-bold">', data_district$leg_n_votes_party_line_bipartisan, '</span></li>',
      '<li>Votes against both parties: <span class="stat-bold">', data_district$leg_n_votes_independent, '</span></li>',
      '<li>Absent or no votes: <span class="stat-bold">', data_district$leg_n_votes_absent_nv, '</span></li>',
      '<li>Other votes: <span class="stat-bold">', data_district$leg_n_votes_other, '</span></li>',
      '</ul></li>',
      '<li><a href="', data_district$ballotpedia, '" target="_blank">View ', data_district$legislator_name, '\'s profile on Ballotpedia</a></li>',
      '</ul>',
      '</div>'
    ))
  })
    
  #############################
  #                           #  
  # district lean             #
  #                           #
  #############################
# output$helper3_district_lean <- renderUI({
#   data_district <- qry_demo_district()
#   
#   same_party <- if (data_district$party == "R") {"Republican"} else if (data_district$party == "D") {"Democrat"}
#   
#   n_districts <- if (data_district$chamber == "House") {120} else if (data_district$chamber == "Senate") {40}
#   
#   rank_dist <- if (data_district$party == "R") {
#     data_district$rank_partisan_dist_R
#   } else if (data_district$party == "D") {
#     data_district$rank_partisan_dist_D
#   }
# 
#   
#   HTML(paste0(
#     '<div class="flex-item">',
#     '<div class="flex-header-section">Population Voting</div>',
#   
#         'In comparison, this district is ranked<br>#<span class="stat-bold">', rank_dist, '</span> most ', same_party,
#         '-leaning of <span class="stat-bold">', n_districts, '</span> ', data_district$chamber, ' districts:<br>',
#         '<span class="stat-bold">', data_district$avg_party_lean, ' + ', data_district$avg_party_lean_points_abs, '</span><br>',
#         '<span class="stat-bold">', percent(data_district$avg_pct_R), '</span> Republican<br>',
#         '<span class="stat-bold">', percent(data_district$avg_pct_D), '</span> Democrat<br>',
#     '</div>'
#       ))
#   })
  output$helper3_district_lean <- renderUI({
    data_district <- qry_demo_district()
    same_party <- if (data_district$party == "R") "Republican" else "Democrat"
    n_districts <- if (data_district$chamber == "House") 120 else 40
    rank_dist <- if (data_district$party == "R") data_district$rank_partisan_dist_R else data_district$rank_partisan_dist_D
    
    HTML(paste0(
      '<div class="flex-item population-voting">',
      '<h3 class="flex-header-section">POPULATION VOTING</h3>',
      '<ul class="main-list">',
      '<li>This district is ranked #<span class="stat-bold">', rank_dist, '</span> most ', same_party,
      '-leaning of <span class="stat-bold">', n_districts, '</span> ', data_district$chamber, ' districts</li>',
      '<li>Partisan lean: <span class="stat-bold">', data_district$avg_party_lean, ' + ', data_district$avg_party_lean_points_abs, '</span></li>',
      '<li>Voting breakdown:',
      '<ul>',
      '<li><span class="stat-bold">', percent(data_district$avg_pct_R), '</span> Republican</li>',
      '<li><span class="stat-bold">', percent(data_district$avg_pct_D), '</span> Democrat</li>',
      '</ul></li>',
      '</ul>',
      '</div>'
    ))
  }) 
  
  ########################################
  #                                      #  
  # demographics                         #
  #                                      #
  ########################################   
  
  # output$helper3_demographics <- renderUI({
  #   tagList(
  #     HTML(paste0(
  #       '<div class="flex-item"><div class = "flex-header-section">District Demographics</div>'
  #     )),
  #     tags$div(
  #     plotOutput("demographicsPlot"),
  #     style = "align-items:center;"),
  #     HTML('</div>')
  #   )
  # })
  output$helper3_demographics <- renderUI({
    tagList(
      div(class = "flex-item district-demographics",
          h3(class = "flex-header-section", "DISTRICT DEMOGRAPHICS"),
          plotOutput("demographicsPlot", height = "auto")
      )
    )
  })
  
  output$demographicsPlot <- renderPlot({
    req(qry_demo_district)
    demo_district <- qry_demo_district()
    demo_state <- app03_district_context_state
    
    # Create a district name
    district_name <- paste(demo_district$chamber, "District", demo_district$district)
    
    data <- data.frame(
      Category = factor(c(district_name, "Florida", district_name, "Florida", 
                          district_name, "Florida", district_name, "Florida"),
                        levels = c("Florida", district_name)),
      Percent = c(
        demo_district$pct_white, demo_state$pct_white,
        demo_district$pct_black, demo_state$pct_black,
        demo_district$pct_asian, demo_state$pct_asian,
        demo_district$pct_hispanic, demo_state$pct_hispanic
      ),
      Demographic = rep(c("White", "Black", "Asian", "Hispanic"), each = 2)
    )
    
    create_plot <- function(demo) {
      ggplot(subset(data, Demographic == demo), aes(x = Category, y = Percent, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        geom_text(aes(label = scales::percent(Percent, accuracy = 0.1)), 
                  position = position_dodge(width = 0), 
                  hjust = -.1,
                  size = 5,
                  family = "Archivo") +
        scale_fill_manual(values = c(setNames(c("#17becf", "#dfdfdf"), c(district_name, "Florida")))) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(data$Percent) * 1.175)) +
        labs(title = demo, x = "", y = "") +
        theme_minimal(base_size = 14,base_family = "Archivo") +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5,colour = "#064875"),
          axis.text.y = element_text(size = 14),
          plot.margin = margin(20, 10, 20, 10)
        ) +
        coord_flip()
    }
    
    plots <- lapply(c("White", "Black", "Asian", "Hispanic"), create_plot)
    do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
  }, width = 400, height = 600)
  
  # output$demographicsPlot <- renderPlot({
  #   req(qry_demo_district)
  #   demo_district <- qry_demo_district()
  #   demo_state <- app03_district_context_state
  #   
  #   # need to have 5x2 for each of category, percent, and demographic
  #   data <- data.frame(
  #     Category = factor(rep(c("District", "State"), 5), levels = c("State", "District")),  # Reverse factor levels
  #     Percent = c(
  #       demo_district$pct_white, demo_state$pct_white,
  #       demo_district$pct_black, demo_state$pct_black,
  #       demo_district$pct_asian, demo_state$pct_asian,
  #       demo_district$pct_hispanic, demo_state$pct_hispanic,
  #       demo_district$pct_napi, demo_state$pct_napi
  #     ),
  #     Demographic = rep(c("White", "Black", "Asian", "Hispanic", "Native American/Pacific Islander"), each = 2)
  #   )
  #   
  #   create_plot <- function(demo) {
  #     ggplot(subset(data, Demographic == demo), aes(x = Category, y = Percent, fill = Category)) +
  #       geom_bar(stat = "identity", position = "dodge", width=.9) +
  #       geom_text(aes(label = scales::percent(Percent,accuracy=0.1)), 
  #                 position = position_dodge(width = .9), 
  #                 vjust = 0.5,
  #                 hjust = -0.15,
  #                 size = 6) +
  #       scale_fill_manual(values = c("District" = "#17becf", "State" = "#dfdfdf")) +
  #       scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
  #       labs(title = demo, x = "", y = "") +
  #       theme_minimal() +
  #       theme(
  #         legend.position = "none",
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         axis.text.x = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         plot.title=element_text(size=16),
  #         axis.text.y = element_text(size = 15, hjust = 1, margin = margin(r = 0)),
  #         plot.margin=margin(5,15,5,10)
  #       ) +
  #       coord_flip()
  #   }
  #   # Create plots for each demographic
  #   plot_white <- create_plot("White")
  #   plot_black <- create_plot("Black")
  #   plot_asian <- create_plot("Asian")
  #   plot_hispanic <- create_plot("Hispanic")
  #   plot_napi <- create_plot("Native American/Pacific Islander")
  #   
  #   # Combine plots using patchwork
  #   plot_white / plot_black / plot_asian / plot_hispanic / plot_napi
  #   }, width = 400, height = "auto")
  

  ########################################
  #                                      #  
  # Combined output                      #
  #                                      #
  ######################################## 
  output$dynamicContextComparison <- renderUI({
    tagList(
      HTML('<div class="flex-section">'),
      uiOutput("helper3_party_loyalty"),
      uiOutput("helper3_district_lean"),
      uiOutput("helper3_demographics"),
      HTML('</div>')
    )
  })
  
  
  ########################################
  #                                      #  
  # Footer- profile                      #
  #                                      #
  ########################################   
  # output$dynamicLegProfile <- renderUI({
  #   data_district <- qry_demo_district()
  #   
  #   HTML(paste0(
  #     '<hr>',
  #     '<div class="header-section">Legislator Profile</div>',
  #     '<div align="left">',
  #     '<a href = "', data_district$ballotpedia ,'" target="_blank">Ballotpedia Profile</a>',
  #     '</div>'
  #   ))
  # })
  
  selected_legislator <- reactive({
    data <- qry_demo_district()
    print(paste("Selected legislator:", data$legislator_name))  # Debug print
    data$legislator_name
  })
  
  # Call the voting history module
  votingHistoryServer("votingHistory", selected_legislator)
  
  ########################################
  #                                      #  
  # Footer- methodology                  #
  #                                      #
  ########################################   
  output$staticMethodology3 <- renderUI({
    HTML(paste0(
      '<hr>',
      '<div class="header-section">Methodology</div>',
      '<div class="methodology-notes">',
      '*Other votes include those marked absent or "no vote", voting with party when party is equally divided, and voting against party when oppo is equally divided.</span><br>',
      'Legislator party loyalty is calculated across all legislative sessions in 2023 and 2024, as a weighted average of votes with party/against oppo (1) and against party/with oppo (0), excluding votes with both parties or against both parties.<br>',
      'District electoral lean is calculated based on voting in the 2016 Presidential, 2018 Gubernatorial, and 2020 Presidential elections.<br>',
      '<strong>Data sources:</strong>',
      '<ul>',
      '<li>Legislator voting info from <a href="https://legiscan.com/FL/datasets">LegiScan\'s Florida Legislative Datasets for all 2023 and 2024 Regular Session</a>.<br>',
      '<li>District demographics and election results curated by <a href="https://davesredistricting.org/maps#state::FL">Dave\'s Redistricting</a>.',
      '</ul>',
      'For details on wishlist items and work in progress, see <a href="https://docs.google.com/document/d/1e3KDrnpXjKL4OJqFR49hqti77TntPRL7k4AkqSfsefU/edit" target="_blank"><strong>development notes</strong></a>.',
      '<br><br></div>'
    ))
  })
  
# END OBSERVER EVENT  
})