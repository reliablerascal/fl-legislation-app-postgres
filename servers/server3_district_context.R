# SERVER3_DISTRICT_CONTEXT.R
# This app shows demographic and electoral political leaning characteristics of each district compared to legislators' voting records

########################################
#                                      #  
# app 3: district context              #
#                                      #
########################################
source("servers/voting_history_module.R")
library(patchwork)
library(bslib)
library(shinyBS)

# App-specific logic
observeEvent(input$navbar_page == "app3", {
  req(app02_leg_activity) # Ensure data is loaded
  methodology_years_numeric <- reactive({
    years <- unique(app02_leg_activity$session_year)
    years <- years[!is.na(years)]
    years <- suppressWarnings(as.numeric(years))
    sort(unique(years)) # Sort ascending for min/max
  })
  
  # Create the dynamic year string for display
  methodology_year_string <- reactive({
    years <- methodology_years_numeric()
    req(length(years) > 0) # Ensure we have years
    
    if (length(years) == 1) {
      as.character(years) # Single year
    } else if (length(years) == 2) {
      paste(years[1], "and", years[2]) # Two years
    } else {
      # Check if years are sequential for range format
      if (all(diff(years) == 1)) {
        paste0(min(years), "-", max(years)) # Contiguous range (e.g., 2023-2025)
      } else {
        # Non-contiguous years (e.g., "2023, 2024, and 2026") - more complex formatting needed if required
        # For now, let's just show min-max as a fallback
        paste0(min(years), "-", max(years))
        # Or alternatively, list them:
        # paste(paste(head(years, -1), collapse=", "), "and", tail(years, 1))
      }
    }
  })
  ########################################
  #                                      #  
  # Header- methodology and legend       #
  #                                      #
  ########################################   
  output$dynamicHeader3 <- renderUI({
    data_district <- qry_demo_district()
    
    HTML(paste0(
      '<div class = "header-tab-small">Legislator Spotlight</div>',
      '<h2>', data_district$legislator_name, ' (', data_district$party, ')</h2>',
      '<h3>', data_district$chamber, ' District ', data_district$district_number, '</h3>',
      '<div align="left">',
      'This tool compares each legislator\'s voting record with their district\'s political leanings. ',
      'Use it to understand how well a legislator represents their constituents\' views and characteristics. The tool below allows you to look up every single vote every legislator has taken, from committee votes to bill amendments to final roll calls.',
      '</div>'
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
    same_party_adj <- if (data_district$party == "R") "Republican" else "Democratic"
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
      '<h3 class="flex-header-section">VOTING RECORD</h3>',
      '<h4 class="legislator-name">', data_district$legislator_name, '\'S VOTING RECORD:</h4>',
      '<ul class="main-list">',
      '<li><div>Party Loyalty Ranking:</div> <div><span class="stat-bold">#', rank_leg, '</span> most loyal out of <span class="stat-bold">', n_legislators_in_party, '</span> ', input$chamber3, ' ', same_party, 's</div></li>',
      '<li>Based on <span class="stat-bold">', data_district$leg_n_votes_denom_loyalty, '</span> key votes:',
      '<ul>',
      '<li>Voted with the ',same_party_adj,' Party: <span class="stat-bold">', data_district$leg_n_votes_party_line_partisan, '</span> ', 
      '(<span class="percentage">', percent(data_district$leg_n_votes_party_line_partisan/data_district$leg_n_votes_denom_loyalty, accuracy = 0.1), '</span>)</li>',
      '<li>Voted against the ',same_party_adj,' Party: <span class="stat-bold">', data_district$leg_n_votes_cross_party, '</span> ',
      '(<span class="percentage">', percent(data_district$leg_n_votes_cross_party/data_district$leg_n_votes_denom_loyalty, accuracy = 0.1), '</span>)</li>',
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
  output$helper3_district_lean <- renderUI({
    data_district <- qry_demo_district()
    # Ensure data is valid and has at least one row
    req(nrow(data_district) > 0,
        !is.na(data_district$avg_party_lean),
        !is.na(data_district$avg_party_lean_points_abs),
        !is.na(data_district$avg_pct_R),
        !is.na(data_district$avg_pct_D))
    
    # Determine district lean party and associated values
    district_lean_party_abbr <- data_district$avg_party_lean # "R" or "D"
    district_lean_party_name <- ifelse(district_lean_party_abbr == "R", "Republican", "Democratic")
    district_rank_col <- ifelse(district_lean_party_abbr == "R", data_district$rank_partisan_dist_R, data_district$rank_partisan_dist_D)
    district_rank_formatted <- ifelse(is.na(district_rank_col), "?", as.integer(district_rank_col))
    
    # Construct lean display string (e.g., "R+15.5")
    lean_value_abs_formatted <- format(round(data_district$avg_party_lean_points_abs, 1), nsmall = 1)
    lean_display_string <- paste0(district_lean_party_abbr, "+", lean_value_abs_formatted)
    
    # Determine winning party for description
    winning_party_name_desc <- ifelse(district_lean_party_abbr == "R", "Republicans", "Democrats")
    
    # Construct simplified and corrected description sentence
    lean_description <- paste0(
      "This means ", winning_party_name_desc,
      " on average won recent statewide elections by ",
      lean_value_abs_formatted, # Use formatted absolute value
      " percentage points in this district."
    )
    
    # Chamber info
    n_districts <- if (data_district$chamber == "House") 120 else 40
    
    # Format percentages
    avg_pct_R_formatted <- scales::percent(data_district$avg_pct_R, accuracy = 0.1)
    avg_pct_D_formatted <- scales::percent(data_district$avg_pct_D, accuracy = 0.1)
    
    HTML(paste0(
      '<div class="flex-item population-voting">',
      '<h3 class="flex-header-section">POPULATION VOTING</h3>',
      '<ul class="main-list">',
      # Use district's lean party and rank
      '<li>District Partisanship: This district votes the #<span class="stat-bold">', district_rank_formatted, '</span> most ', district_lean_party_name, # Use district lean party
      '-leaning of <span class="stat-bold">', n_districts, '</span> ', data_district$chamber, ' districts</li>',
      # Use constructed display string
      '<li>Partisan lean: <span class="stat-bold">', lean_display_string, '</span></li>',
      # Use corrected description
      '<li>', lean_description, '</li>',
      '<li>Recent Election Results:',
      '<ul>',
      '<li>Republican:  <span class="stat-bold">', avg_pct_R_formatted, '</span></li>',
      '<li>Democratic:  <span class="stat-bold">', avg_pct_D_formatted, '</span></li>',
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
    district_name <- paste(demo_district$chamber, "District", demo_district$district_number)
    
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
        scale_fill_manual(values = c(setNames(c("#098677", "#cccccc"), c(district_name, "Florida")))) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
        labs(title = demo, x = "", y = "") +
        theme_minimal(base_size = 14,base_family = "Archivo") +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x =element_blank(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5,colour = "#064875"),
          axis.text.y = element_text(size = 14),
          plot.margin = margin(20, 30, 20, 10),
          
        panel.background = element_rect(fill = "#f9f9f9", colour = NA),

        plot.background = element_rect(fill = "#ffffff", colour = NA)
        ) +
        coord_flip()
    }
    
    plots <- lapply(c("White", "Black", "Asian", "Hispanic"), create_plot)
    do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
  }, 
  width = 400, height = 600)
  
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
    year_text <- methodology_year_string()
    
    HTML(paste0(
      '<hr>',
      '<div class="header-section"><h3>Methodology</h3></div>',
      '<div class="methodology-notes">',
      '<p>*Other votes include those marked absent or "no vote", voting with party when party is equally divided, and voting against party when oppo is equally divided.</p>',
      # Use dynamic year_text
      '<p><strong>Legislator Party Loyalty:</strong> Calculated using all votes from ', year_text, ' legislative sessions where parties disagreed.</p>',
      '<p>Scores range from 0 to 1, where 1 indicates always voting with party majority and 0 indicates always voting against.</p>',
      '<p><strong>District Partisan Lean:</strong> Based on a weighted average of recent election results:</p>',
      '<ul>',
      '<li>2022 Gubernatorial Election (30% weight)</li>',
      '<li>2020 Presidential Election (50% weight)</li>',
      '<li>2018 Gubernatorial Election (10% weight)</li>',
      '<li>2016 Presidential Election (10% weight)</li>',
      '</ul>',
      '<strong>Data sources:</strong>',
      '<ul>',
      # Use dynamic year_text again
      '<li>Legislator voting info from <a href="https://legiscan.com/FL/datasets">LegiScan\'s Florida Legislative Datasets for ', year_text, ' Regular Session</a>.</li>',
      '<li>District demographics and election results curated by <a href="https://davesredistricting.org/maps#state::FL">Dave\'s Redistricting</a>.</li>',
      '</ul>',
      '<br></div>'
    ))
  })
  
# END OBSERVER EVENT  
})