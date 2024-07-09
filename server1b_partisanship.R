# redundant, but somehow necessary to reload some libraries?
# library(shiny)
# library(dplyr)
# library(plotly)

########################################
#                                      #  
# app 1: voting patterns analysis      #
#                                      #
########################################

# App-specific logic
observeEvent(input$navbar_page == "app1", {
  
  n_legislators <- reactive({
    data <- data_filtered()
    if (data$is_empty) {
      return(0)
    } else {
      return(n_distinct(data$data$legislator_name))
    }
  })
  
  n_roll_calls <- reactive({
    data <- data_filtered()
    if (data$is_empty) {
      return(0)
    } else {
      return(n_distinct(data$data$roll_call_id))
    }
  })
  
  
  
  ########################################
  #                                      #  
  # Header- methodology and legend       #
  #                                      #
  ########################################   
  output$dynamicHeader <- renderUI({
    req(input$year, input$party)
    year <- input$year
    party_same <- if(input$party == "D") "Democrats" else if(input$party == "R") "Republicans" else "All Parties"
    party_oppo <- if(input$party == "D") "Republicans" else if(input$party == "R") "Democrats" else "All Parties"
    color_oppo <-if(input$party == "D") "#d73027" else if(input$party == "R") "#4575b4"
    color_same <-if(input$party == "D") "#4575b4" else if(input$party == "R") "#d73027"
      
    HTML(paste0(
      '<!DOCTYPE html>',
      '<html lang="en">',
      '<head>',
      '<meta charset="UTF-8">',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0">',
      '<title>Voting Patterns</title>',
      '<style>',
      '.legend {',
      '  display: flex;',
      '  flex-direction: column;',
      '  align-items: start;',
      '  margin: 0 auto;',
      '  max-width: 600px;',
      '  border: 1px solid black;',
      '}',
      '.legend-item {',
      '  display: flex;',
      '  align-items: center;',
      '  margin: 1px 0;',
      '  margin-left: 5px;',
      '}',
      '.color-box {',
      '  width: 20px;',
      '  height: 20px;',
      '  display: inline-block;',
      '  margin-right: 10px;',
      '}',
      '</style>',
      '</head>',
      '<body>',
      '<h2 style="text-align: center;">(DEV VERSION) Florida House Voting Patterns: ', party_same, '</h2>',
      '<div align="left">',
      'Recommended viewing on desktop display.<br>',
      'This chart displays each legislator\'s vote on each roll call for bills &amp; amendments where their party voted in favor but not unanimously. ',
      'Bills may have multiple roll calls; hover over plot for more info about specific roll calls.<br>',
      'The intended audience for this app is Florida journalists focused on politics, policy, and elections.<br>',
      '<div style="font-size: 10px;">',
      'Partisanship for each legislator is calculated across all sessions in 2023 and 2024, as a weighted average of with party/against oppo (0) and against party/with oppo (1), excluding votes with both parties or against both parties.<br>',
      '<strong>Data source:</strong> <a href="https://legiscan.com/FL/datasets">LegiScan\'s Florida Legislative Datasets for all 2023 and 2024 Regular Session</a>.<br>',
      'For details on wishlist items, work in progress, and tabled updates, see <a href="https://docs.google.com/document/d/1OGiJH7B_0j3B38gEtgt_FDhkxzL84ZtGistdup2yYHI/edit"><strong>development notes</strong></a>.',
      '</div>',
      '<div class="legend">',
      '  <div class="legend-item">',
      '    <div class="color-box" style="background-color: ',color_same, ';"></div>',
      '    <span style="font-size: 14px;">Legislator aligned <i>with</i> most ', party_same,'.</span>',
      '  </div>',
      '  <div class="legend-item">',
      '    <div class="color-box" style="background-color: ',color_oppo, ';"></div>',
      '    <span style="font-size: 14px;">Legislator aligned <i>against</i> most ',party_same,' and <i>with</i> most ',party_oppo,'.</span>',
      '  </div>',
      '  <div class="legend-item">',
      '    <div class="color-box" style="background-color: #6DA832;"></div>',
      '    <span style="font-size: 14px;">Legislator aligned <i>against</i> both parties in bipartisan decisions.</span>',
      '  </div>',
      '  <div class="legend-item">',
      '    <div class="color-box" style="background-color: #FFFFFF; border: 1px solid black;"></div>',
      '    <span style="font-size: 14px;">Legislator did not vote (missed vote or not assigned to that committee).</span>',
      '  </div>',
      '</div>',
      '</body>',
      '</html>'
    ))
  })
  
  output$dynamicRecordCount <- renderUI({
    HTML(paste0(
      '<p style="font-size: 14px;">Displaying <strong style="font-size: 18px;">', n_legislators(), 
      '</strong> legislators across <strong style="font-size: 18px;">', n_roll_calls(), '</strong> roll call votes.</p>'
    ))
  })
  
  
  ##############################
  #                            #  
  # USER FILTER & SORT PARAMS  #
  #                            #
  ##############################
  createFilterBox <- function(inputId, label, choices, selected = NULL) {
    div(
      selectInput(inputId, label, choices = choices, selected = selected)
    )
  }
  
  output$dynamicFilters <- renderUI({
    div(class = "filter-row",
        style = "display:flex; flex-wrap: wrap; justify-content: center; margin-top:1.5vw; margin-bottom: 0px; padding-bottom:0px; margin-left:auto; margin-right:auto;",
        
        createFilterBox("party", "Select Party:", c("D", "R")),
        createFilterBox("chamber", "Select Chamber:", c("House", "Senate")),
        createFilterBox("year", "Select Session Year:", c(2023, 2024, "All"), selected = 2024),
        createFilterBox("final", "Final (Third Reading) Vote?", c("Y", "N", "All"), selected = "Y"),
        createFilterBox("bill_category", "Bill Category (demo)", c("education", "All"), selected = "All"),
        createFilterBox("sort_by_leg", "Sort Legislators By:", c("Name", "Partisanship", "District"), selected = "Partisanship"),
        createFilterBox("sort_by_rc", "Sort Roll Calls By:", c("Bill Number", "Partisanship"), selected = "Partisanship")
    )
  })
  
  #filter junction table to restrict bills by category, if applicable
  filtered_jct <- reactive({
    req(input$bill_category)
    jct_bill_categories %>% filter(bill_category == input$bill_category)
  })
  
  data_filtered <- reactive({
    #data <- app01_vote_patterns %>% filter(true_pct!= 1 & true_pct != 0)
    req(input$party, input$chamber, input$year, input$final, input$bill_category, input$sort_by_leg, input$sort_by_rc)  # Ensure inputs are available
    data <- app01_vote_patterns
    
    if (input$year != "All") {
      data <- data %>% filter(session_year == input$year)
    }
    
    if (input$final != "All") {
      data <- data %>% filter(final_vote == input$final)
    }
    
    if (input$party != "All") {
      #first, filter candidates, then filter relevant roll calls
      data <- data %>% dplyr::filter(party == input$party)
      if (input$party == "D") {
        data <- data %>% filter(is_include_d == 1)
      } else if (input$party == "R") {
        data <- data %>% filter(is_include_r == 1)
      }
    }
    
    if (input$chamber != "All") {
      data <- data %>% dplyr::filter(chamber == input$chamber)
    }
    
    if (input$bill_category != "All") {
      data <- data %>% dplyr::filter(bill_id %in% filtered_jct()$bill_id)
    }
    
    
    
    return (list(data = data, is_empty = nrow(data) == 0))
  })
  
  output$noDataMessage <- renderUI({
    filtered_data <- data_filtered()
    if (filtered_data$is_empty) div("No bills match selected filters.", style = "color: red; font-weight: bold;")
  })
  
  #####################
  #                   #  
  # PLOT              #
  #                   #
  #####################
  #clarify with Andrew- do we want to highlight % of present for party, vs. % of total for bill?
  #format pop-ups for when user hovers over a heatmap square
  createHoverText <- function(numbers, descriptions, urls, pcts, pct_d, pct_r, vote_texts, descs, title, date, names, width = 100) {
    wrapped_descriptions <- sapply(descriptions, function(desc) paste(strwrap(desc, width = width), collapse = "<br>"))
    paste0(
      "<b>", names, "</b> voted <i>", vote_texts, "</i> on <b>", descs, "</b> on <b>", date, "</b><br>",
      "for bill <b>", numbers, "</b> - <b>", title, "</b></a><br>",
      "<b>", pcts, "</b> of total roll call (including absent & no vote) supported this bill.<br>",
      "Support amongst those present: <b>Democrat ", pct_d, "</b> / <b>Republican ", pct_r, "</b>.<br><br>",
      "<b>Bill Description:</b> ", wrapped_descriptions, "<br>"
    )
  }
  
  
  
  output$heatmapPlot <- renderPlotly({
    filtered_data <- data_filtered()
    # Determine colors based on party
    
    #don't plot if no bills found within selected filter
    req(!filtered_data$is_empty, "No bills match selected filters.")
    
    data <- filtered_data$data
    
    if (nrow(data) == 0) return(NULL)
    
    #create hover text
    #RR not sure why these are all plural? maybe b/c earlier version wasn't deduplicated
    data$hover_text <- mapply(
      createHoverText,
      numbers = data$bill_number,
      descs = data$roll_call_desc,
      title = data$bill_title,
      date = data$roll_call_date,
      descriptions = data$bill_desc,
      urls = data$bill_url,
      pcts = percent(data$pct_of_total),
      pct_d = percent(data$D_pct_of_present, accuracy = 1),
      pct_r = percent(data$R_pct_of_present, accuracy = 1),
      vote_texts = data$vote_text,
      names = data$legislator_name,
      SIMPLIFY = FALSE  # Keep it as a list
    )
    data$hover_text <- sapply(data$hover_text, paste, collapse = " ") # Collapse the list into a single string
    
    
    # Dynamic plot size calculation
    numLegislators <- n_legislators()
    baseHeight <- 500 # Minimum height
    perLegHeight <- 10 # Height per legislator
    totalHeight <- baseHeight + (numLegislators * perLegHeight) # Total dynamic height
    
    numBills <- n_roll_calls()
    baseWidth <- 500 # Minimum width
    perBillWidth <- 10 # Height per bill
    totalWidth <- baseWidth + (numBills * perBillWidth) # Total dynamic width
    
    # sort legislators. "reorder" function is used to sort based on another variable
    if (input$sort_by_leg == "Name") {
      data <- data %>%
        arrange(desc(last_name))  # Arrange the data by last_name in descending order
      data$legislator_name <- factor(data$legislator_name, levels = unique(data$legislator_name))  # Set factor levels
    } else if (input$sort_by_leg == "Partisanship") {
      data$legislator_name <- reorder(data$legislator_name, -data$mean_partisanship)
    } else if (input$sort_by_leg == "District") {
      data$legislator_name <- reorder(data$legislator_name, -data$district_number)
    }
    
    # sort roll calls
    data$bill_number <- as.character(data$bill_number)
    if (input$sort_by_rc == "Bill Number") {
      data <- data[order(data$bill_number), ]
    } else if (input$sort_by_rc == "Partisanship") {
      data <- data[order(data$rc_mean_partisanship), ]
    }
    data$roll_call_id <- factor(data$roll_call_id, levels = unique(data$roll_call_id))
    
    # data$bill_number <- as.character(data$bill_number)
    # data <- data[order(data$bill_number), ]
    # data$roll_call_id <- factor(data$roll_call_id, levels = unique(data$roll_call_id))
    
    # set y-axis labels for legislators
    labels <- unique(data[, c("legislator_name", "district_number", "last_name")])
    y_labels <- setNames(paste(labels$last_name, " (", labels$district_number, ")", sep = ""), labels$legislator_name)
    y_urls <- unique(data[, c("legislator_name", "ballotpedia")])
    y_labels_with_links <- setNames(paste('<a href="', y_urls$ballotpedia, '">', y_labels[as.character(y_urls$legislator_name)], '</a>', sep=""), y_urls$legislator_name)
    # y_labels_with_links <- ifelse(
    #   labels$last_name == "Nixon",
    #   paste('<a href="', y_urls$ballotpedia[match(labels$legislator_name, y_urls$legislator_name)], '"><strong>', y_labels, '</strong></a>', sep=""),
    #   paste('<a href="', y_urls$ballotpedia[match(labels$legislator_name, y_urls$legislator_name)], '">', y_labels, '</a>', sep="")
    # )
    y_labels_with_links <- setNames(y_labels_with_links, labels$legislator_name)
    
    # bill titles tend to be too long for clean rendering on x-axis
    labels <- unique(data[, c("roll_call_id", "bill_number", "session_year")])
    x_labels <- setNames(paste(labels$bill_number, labels$session_year, sep = " - "), labels$roll_call_id)
    x_urls <- unique(data[, c("roll_call_id", "bill_url")])
    x_labels_with_links <- setNames(paste('<a href="', x_urls$bill_url , '">', x_labels[as.character(labels$roll_call_id)], '</a>', sep=""), labels$roll_call_id)
    #x_labels_tooltips <- setNames(paste('Bill:', labels$bill_number), labels$roll_call_id)
    
    # trying to use tickvals was problematic. x axis labels disappeared entirely
    # tickvals <- labels$roll_call_id
    # ticktext <- x_labels_with_links[as.character(tickvals)]
    # print(length(tickvals) == length(ticktext))
    # print("ticktext values:")
    # cat(ticktext, sep = "\n")
    
    # set up plot colors
    color_with_party <- if(input$party == "D") "#4575b4" else if(input$party == "R") "#d73027" else "#4575b4"
    color_against_party <- if(input$party == "D") "#d73027" else if(input$party == "R") "#4575b4" else "#d73027"
    color_against_both <- "#6DA832"
    color_na <- "#FFFFFF"
    
    #gradient-based plot is a hack, but it works quite well. scale_fill_manual takes *much* longer to render. 
    #scale_fill_manual(values = fill_colors, na.value = color_na) +
    #fill_colors <- c("0" = color_with_party, "1" = color_against_party, "99" = color_against_both, "999" = color_na)
    #x_labels_tooltips <- setNames(paste('Bill:', labels$bill_number), labels$roll_call_id)
    
    # Generate the plot
    p <- ggplot(data, aes(y = legislator_name, x = roll_call_id, fill = partisan_vote_plot, text = hover_text)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(low = color_with_party, mid = color_against_party, high = color_against_both, midpoint = 1) +
      theme_minimal() +
      scale_y_discrete(labels = y_labels_with_links) + 
      scale_x_discrete(labels = x_labels_with_links, position = "top") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 10),
            legend.position = "none",
            plot.title = element_blank(),
            plot.subtitle = element_blank())
    
    ggplotly(p, tooltip = "text", height = totalHeight, width= totalWidth) %>%
      ###ATTEMPT TO ADD TOOLTIPS ON X AXIS
      # add_trace(
      #   type = 'scatter',
      #   x = labels$roll_call_id,
      #   y = rep(length(levels(data$legislator_name)) + 1, length(labels$roll_call_id)),  # Position above the plot
      #   text = x_labels_tooltips[as.character(labels$roll_call_id)],
      #   hoverinfo = 'text',
      #   mode = 'markers',
      #   marker = list(opacity = 0),
      #   showlegend = FALSE,
      #   inherit = FALSE
      # ) %>%
      layout(
        autosize = TRUE,
        xaxis = list(side = "top"),
        font = list(family = "Archivo"),
        margin = list(l=200, t = 85, b = 150),  # Fix margins to ensure rows and columns don't get compressed
        plot_bgcolor = "rgba(255,255,255,0.85)",  # Transparent plot background
        paper_bgcolor = "rgba(255,255,255,0.85)"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  

  
# END OBSERVER EVENT  
})