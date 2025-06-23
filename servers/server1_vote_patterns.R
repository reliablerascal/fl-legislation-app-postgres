# server1_vote_patterns.R
# version from 5/16/25 AP
########################################
#                                      #  
# app 1: voting patterns analysis      #
#                                      #
########################################
# App-specific logic
observeEvent(input$navbar_page == "app1", {
  
  #--- Is the user on mobile?
  is_mobile <- reactive({isTRUE(input$isMobile)})
  #observe({ print(input$isMobile); print(is_mobile())})
  
  #--- Filtering: one time, used everywhere downstream
  filtered_votes <- reactive({
    req(input$party, input$chamber, input$year, input$final, input$sort_by_leg, input$sort_by_rc)
    data <- app01_vote_patterns
    if (input$year != "All") data <- data %>% filter(session_year == input$year)
    if (input$final != "All") data <- data %>% filter(final_vote == input$final)
    if (input$party != "All") {
      data <- data %>% filter(party == input$party)
      data <- if (input$party == "D") filter(data, is_include_d == 1) else filter(data, is_include_r == 1)
    }
    if (input$chamber != "All") data <- data %>% filter(chamber == input$chamber)
    list(data = data, is_empty = nrow(data) == 0)
  })
  
  n_legislators <- reactive({
    fd <- filtered_votes(); if (fd$is_empty) 0 else n_distinct(fd$data$legislator_name)
  })
  n_roll_calls <- reactive({
    fd <- filtered_votes(); if (fd$is_empty) 0 else n_distinct(fd$data$roll_call_id)
  })
  
  output$dynamicHeader <- renderUI({
    
    HTML(paste0(
      '<div class="header-tab">Florida Legislative Compass: How Lawmakers Vote</div>',
      '<div class="subtitle">Navigate Lawmakers\' Voting Records</div>',
      '<div align="left">',
      'Explore how Florida legislators vote, compare their choices to their district\'s political leanings, ',
      'and see how closely they align with their party. This easy-to-use tool helps you understand ',
      'your representative\'s voting patterns on key issues.',
      'This tool visualizes how Florida legislators vote on bills and amendments. Each square represents a vote, color-coded to show alignment or divergence from party lines. ',
      'Hover over any square for detailed information about the vote. ',
      'Use the filters below to explore voting patterns by party, chamber, year, and more.',
      '</div>'
    ))
  })
  
  #--- Legend (dynamic by party/chamber)
  output$dynamicLegend <- renderUI({
    req(input$year, input$party)
    party_same <- if (input$party == "D") "Democrats" else if (input$party == "R") "Republicans" else "All Parties"
    party_oppo <- if (input$party == "D") "Republicans" else if (input$party == "R") "Democrats" else "All Parties"
    color_oppo <- if (input$party == "D") "#d73027" else if (input$party == "R") "#4575b4"
    color_same <- if (input$party == "D") "#4575b4" else if (input$party == "R") "#d73027"
    HTML(paste0(
      '<hr><div align="left">',
      '<div class="header-section">', input$chamber, ' ', party_same, '</div>',
      '<div class="legend-box">',
      '  <div class="legend-item"><div class="legend-color" style="background-color: ', color_same, ';"></div>Voted <em>in line with</em> most ', party_same, '.</div>',
      '  <div class="legend-item"><div class="legend-color" style="background-color: ', color_oppo, ';"></div>Voted <em>against</em> the majority of ', party_same, ' and <em>with</em> the majority of ', party_oppo, '.</div>',
      '  <div class="legend-item"><div class="legend-color" style="background-color: #6DA832;"></div>Voted <em>against</em> both parties\' majorities in bipartisan decisions.</div>',
      '  <div class="legend-item"><div class="legend-color" style="background-color: #FFFFFF; border: 1px solid black;"></div>Legislator did not vote (missed vote or not assigned to that committee).</div>',
      '</div></div>'
    ))
  })
  
  #--- Record count message
  output$dynamicRecordCount <- renderUI({
    req(input$year, input$chamber, input$party)
    party_same <- if (input$party == "D") "Democrat" else if (input$party == "R") "Republican" else "All Parties"
    div(class = "record-count",
        HTML(paste0(
          'Displaying <span class="stat-bold">', n_legislators(), '</span> ', input$chamber, ' ', party_same, 's across <span class="stat-bold">', n_roll_calls(), '</span> partisan roll calls in ', input$year, ' where at least one ', input$chamber, ' ', party_same, ' voted against their party majority.'
        ))
    )
  })
  
  ##############################
  #                            #  
  # USER FILTER & SORT PARAMS  #
  #                            #
  ##############################
  createFilterBox <- function(inputId, label, choices, selected = NULL, tooltip = NULL) {
    tagList(
      div(
        style = "display: flex; align-items: center; gap: 6px;",
        selectInput(inputId, label, choices = choices, selected = selected),
        if (!is.null(tooltip))
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            tooltip,
            placement = "right"
          )
      )
    )
  }
  
  output$dynamicFilters <- renderUI({
    req(app01_vote_patterns)
    available_years <- unique(app01_vote_patterns$session_year)
    available_years <- sort(as.numeric(available_years[!is.na(as.numeric(available_years))]), decreasing = TRUE)
    year_choices <- c("All", available_years)
    default_year <- max(available_years)
    tagList(
      div(class = "filter-row query-input",
          createFilterBox("party", "Select Party:", c("D", "R"),
                          tooltip = "Choose whether to view Democratic or Republican legislators' votes."),
          createFilterBox("chamber", "Select Chamber:", c("House", "Senate"),
                          tooltip = "Choose to see Florida's House Representatives or Florida's Senators."),
          createFilterBox("year", "Select Session Year:", choices = year_choices, selected = default_year,
                          tooltip = "Pick a legislative year to see votes from that session."),
          createFilterBox("final", "Final (Third Reading) Vote?", c("Y", "N", "All"), selected = "Y",
                          tooltip = "Show only votes labeled as “Third Reading.” These are usually the final passage votes for bills or major amendments. Sometimes you’ll see more than one for a single bill: if a bill fails, the chamber can vote to reconsider and try again. Or, after the other chamber makes changes, the original chamber may have to vote again to approve those changes."),
          createFilterBox("sort_by_leg", "Sort Legislators By:", c("Name", "Party Loyalty", "District #", "Electorate Lean"), selected = "Party Loyalty",
                          tooltip = "Choose how to order the list of legislators: by name, by how often they vote with their party (“party loyalty”), by district number, or by how their district typically leans in elections."),
          createFilterBox("sort_by_rc", "Sort Roll Calls By:", c("Bill Number", "Party Unity"), selected = "Party Unity",
                          tooltip = "Choose how to order the votes: by bill number or by “party unity,” which shows how much the party agreed or split on each vote.")
      )
    )
  })
  
  
  #--- Fast, vectorized plot prep: all heavy-lifting here (not in renderPlotly)
  plot_ready_data <- reactive({
    fd <- filtered_votes()
    data <- fd$data
    if (fd$is_empty) return(NULL)
    
    # Prepare hover text and rank
    data <- data %>%
      mutate(
        hover_text = paste0(
          "<b>", legislator_name, "</b> voted <i>", vote_text, "</i> on Vote <b>",roll_call_id," - ", roll_call_desc, "</b> on <b>", roll_call_date, "</b><br>",
          "for bill <b>", bill_number, "</b> - <b>", bill_title, "</b><br>",
          "<b>", percent(pct_of_present, 1), "</b> of roll call present supported this bill (",
          "<b>Democrat ", percent(D_pct_of_present, 1), "</b> / <b>Republican ", percent(R_pct_of_present, 1), ")</b><br><br>",
          "<b>Bill Description:</b> ", sapply(bill_desc, function(desc) paste(strwrap(desc, width = 90), collapse = "<br>")), "<br>"
        ),
        rank_partisan_leg = coalesce(rank_partisan_leg_D, rank_partisan_leg_R)
      )
    
    # Sorting (set factors so ggplot does not reorder in renderPlotly)
    if (input$sort_by_leg == "Name") {
      data <- data %>% arrange(desc(last_name))
    } else if (input$sort_by_leg == "Party Loyalty") {
      data <- data %>% arrange(desc(rank_partisan_leg))
    } else if (input$sort_by_leg == "District #") {
      data <- data %>% arrange(desc(district_number))
    }
    data$legislator_name <- factor(data$legislator_name, levels = unique(data$legislator_name))
    
    # Sort roll calls
    if (input$sort_by_rc == "Bill Number") {
      data <- data %>% arrange(bill_number)
    } else if (input$sort_by_rc == "Party Unity") {
      if (input$party == "D") data <- data %>% arrange(desc(rc_unity_D))
      else if (input$party == "R") data <- data %>% arrange(desc(rc_unity_R))
      else data <- data %>% arrange(desc(rc_mean_partisanship))
    }
    data$roll_call_id <- factor(data$roll_call_id, levels = unique(data$roll_call_id))
    
    return(data)
  })
  
  #####################
  #                   #  
  # PLOT              #
  #                   #
  #####################
  #format pop-ups for when user hovers over a heatmap square
  
  output$heatmapPlot <- renderPlotly({
    data <- plot_ready_data()
    req(!is.null(data), "No bills match selected filters.")
    
    legislator_levels <- levels(data$legislator_name)
    roll_call_levels  <- levels(data$roll_call_id)
    
    labels_y <- unique(data[, c("legislator_name", "district_number", "last_name", "ballotpedia")])
    y_labels <- setNames(
      paste(labels_y$last_name, " (", labels_y$district_number, ")", sep = ""),
      labels_y$legislator_name
    )
    y_labels_with_links <- setNames(
      paste('<a href="', labels_y$ballotpedia, '">', y_labels[as.character(labels_y$legislator_name)], '</a>', sep = ""),
      #      y_labels[as.character(labels_y$legislator_name)],
      labels_y$legislator_name
    )
    
    labels_x <- unique(data[, c("roll_call_id", "bill_number", "session_year", "bill_url")])
    x_labels <- setNames(paste(labels_x$bill_number, labels_x$session_year, sep = " - "), labels_x$roll_call_id)
    x_labels_with_links <- setNames(
      paste('<a href="', labels_x$bill_url, '">', x_labels[as.character(labels_x$roll_call_id)], '</a>', sep = ""),
      labels_x$roll_call_id
    )
    
    # Color choices
    color_with_party <- if (input$party == "D") "#4575b4" else if (input$party == "R") "#d73027" else "#4575b4"
    color_against_party <- if (input$party == "D") "#d73027" else if (input$party == "R") "#4575b4" else "#d73027"
    color_against_both <- "#6DA832"
    
    numLegislators <- n_legislators()
    numBills <- n_roll_calls()
    totalHeight <- 500 + numLegislators * 10
    totalWidth <- 500 + numBills * 10
    
    p <- ggplot(data, aes(y = legislator_name, x = roll_call_id, fill = partisan_vote_plot, text = hover_text
                          #,key = legislator_name,customdata = roll_call_id
    )) +
      geom_tile(
        color = "white",
        linewidth = if (!is_mobile()) 0.5 else 0.2,
        height = if (is_mobile()) 0.8 else 1,   # supply a single value, NOT NULL
        width  = if (is_mobile()) 0.8 else 1
      )+
      scale_fill_gradient2(low = color_with_party, mid = color_against_party, high = color_against_both, midpoint = 1) +
      theme_minimal() +
      scale_y_discrete(labels = y_labels_with_links) +
      scale_x_discrete(labels = x_labels_with_links, position = "top") +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = if (!is_mobile()) 10 else 6),
        axis.text.y = element_text(size = if (!is_mobile()) 10 else 8),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.margin = margin(5, 5, 5, 5)
      )
    
    observeEvent(event_data("plotly_click", "leg-heatmap"), {
      cd <- event_data("plotly_click", "leg-heatmap")
      req(cd)
      
      ## factor levels that ggplot/plotly is using
      leg_levels <- levels(data$legislator_name)   # rows (y-axis)
      rc_levels  <- levels(data$roll_call_id)      # columns (x-axis)
      
      ## 1-based indices come from cd$y and cd$x
      leg_val <- leg_levels[ cd$y ]                # 11  -> 11th name
      rc_val  <- rc_levels[ cd$x ] 
      
      pending_selection$legislator <- leg_val
      pending_selection$chamber    <- input$chamber
      
      updateTabsetPanel(session, "navbar_page", "app3")
    })
    
    plotly_output <- plotly::event_register(p = ggplotly(p, tooltip = "text", height = totalHeight, width = totalWidth,source = "leg-heatmap") %>%
                                              layout(
                                                xaxis = list(side = "top"),
                                                font = list(family = "Archivo"),
                                                margin = list(l = 25, t = 50, b = 25),
                                                plot_bgcolor = "rgba(255,255,255,0.85)",
                                                paper_bgcolor = "rgba(255,255,255,0.85)",
                                                dragmode = FALSE
                                              ) %>%
                                              plotly::config(scrollZoom = FALSE, displayModeBar = FALSE),event = "plotly_click")
    
    return(plotly_output)
  }) %>%   bindCache(input$party, input$chamber, input$year, input$final, input$sort_by_leg, input$sort_by_rc)
  
  
  
  #--- Touch UI: scroll instructions
  output$swipeMessage <- renderUI({
    HTML(div(class = "swipe-right", "Scroll right for more votes and down for more legislators"))
  })
  
  #--- Nicely format the session years for display
  format_years_nicely <- function(years) {
    years <- sort(unique(years))
    if (length(years) == 1) return(years)
    if (length(years) == 2) return(paste(years, collapse = " and "))
    paste0(paste(years[-length(years)], collapse = ", "), ", and ", years[length(years)])
  }
  
  #--- Methodology with dynamic session years
  output$staticMethodology1 <- renderUI({
    fd <- filtered_votes()
    years_text <- if (fd$is_empty) "no sessions"
    else format_years_nicely(sort(unique(fd$data$session_year)))
    HTML(paste0(
      '<div class="header-section">About This Tool</div>',
      '<div class="methodology-notes">',
      'Party loyalty scores are calculated using partisan votes across all sessions in <strong>', years_text, '</strong>. ',
      'The votes include every committee vote, every amendment and every final roll call. The score ignores bipartisan votes. Scores range from 0 to 1, where 1 indicates a legislator always votes with the majority of their party and 0 indicates a legislator always votes against their own party.<br>',
      '<strong>Data source:</strong> <a href="https://legiscan.com/FL/datasets">LegiScan\'s Florida Legislative Datasets/a>.<br>',
      '</div>'
    ))
  })
  
}) # END OBSERVER EVENT




