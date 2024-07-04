# SERVER.R
#
# Loads and preprocesses data.
# Handles server-side logic, including reactive expressions and observers.
# Executes data queries and manipulations.
# Generates outputs based on user inputs and updates the UI accordingly.
#
# 6/13/24 RR
# adapted from Andrew's code, initially to update connections from data.RData to Postgres



server <- function(input, output, session) {
  
########################################
#                                      #  
# app 1: voting patterns analysis      #
#                                      #
########################################

#format pop-ups for when user hovers over a heatmap square
  createHoverText <- function(numbers, descriptions, urls, pcts, vote_texts, descs, title, date, names, width = 100) {
    wrapped_descriptions <- sapply(descriptions, function(desc) paste(strwrap(desc, width = width), collapse = "<br>"))
    paste0(
      "<b>", names, "</b> voted <i>", vote_texts, "</i> on <b>", descs, "</b> on <b>", date, "</b><br>",
      "for bill <a href='", urls, "'> <b>", numbers, "</b> - <b>", title, "</b></a><br>",
      "<b>", pcts, "</b> voted for this bill<br><br>",
      "<b>Bill Description:</b> ", wrapped_descriptions, "<br>"
    )
  }
  
# App-specific logic
  observeEvent(input$navbarPage == "app1", {
    # count data output
    # n_legislators <- reactive({
    #   n_distinct(data_filtered()$legislator_name)
    # })
    # 
    # n_roll_calls <- reactive({
    #   n_distinct(data_filtered()$roll_call_id)
    # })
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
    # app 1: legend/header                 #
    #                                      #
    ########################################
    output$dynamicTitle <- renderUI({
      #calculate variables
      year <- input$year
      party_same <- if(input$party == "D") "Democrats" else if(input$party == "R") "Republicans" else "All Parties"
      roleTitle <- if(input$role == "Rep") "Florida House" else if(input$role == "Sen") "Florida Senate" else "Florida Legislature"
      fullTitle <- paste0(roleTitle, " Voting Patterns: ", party_same)
      party_oppo <- if(input$party == "D") "Republicans" else if(input$party == "R") "Democrats" else "All Parties"
      color_oppo <-if(input$party == "D") "#d73027" else if(input$party == "R") "#4575b4"
      color_same <-if(input$party == "D") "#4575b4" else if(input$party == "R") "#d73027"
      
      #display legend
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
        '<h2 style="text-align: center;">Florida House Voting Patterns: ', party_same, '</h2>',
        '<p style="font-size: 14px;">This chart displays each legislator\'s vote on each roll call for amendments and bills where their party did not vote unanimously.</p>',
        '<p style="font-size: 14px;">Data source: <a href="https://legiscan.com/FL/datasets">LegiScan\'s Florida Legislative Datasets for all 2023 and 2024 Regular Session</a>.</p>',
        '<p style="font-size: 14px;">Displaying ',n_legislators(),' legislators across ',n_roll_calls(),' roll call votes.</p>',
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
#####################
#                   #  
# app 1 filter      #
#                   #
#####################
    #filter junction table to restrict bills by category, if applicable
    filtered_jct <- reactive({
      jct_bill_categories %>% filter(bill_category == input$bill_category)
    })
    
    data_filtered <- reactive({
      #data <- app_vote_patterns %>% filter(true_pct!= 1 & true_pct != 0)
      data <- app_vote_patterns
      
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
      
      if (input$role != "All") {
        data <- data %>% dplyr::filter(role == input$role)
      }
      
      if (input$bill_category != "All") {
        data <- data %>% dplyr::filter(bill_id %in% filtered_jct()$bill_id)
      }
      
      # validate(
      #   need(nrow(data) > 0, "No bills match selected filters.")
      # )
      # Return the filtered data and a flag indicating if it's empty
      return (list(data = data, is_empty = nrow(data) == 0))
    })
    
    output$noDataMessage <- renderUI({
      filtered_data <- data_filtered()
      if (filtered_data$is_empty) div("No bills match selected filters.", style = "color: red; font-weight: bold;")
    })
    
#####################
#                   #  
# app 1 plot        #
#                   #
#####################
    output$heatmapPlot <- renderPlotly({
      filtered_data <- data_filtered()
      # Determine colors based on party
      
      #don't plot if no bills found within selected filter
      req(!filtered_data$is_empty, "No bills match selected filters.")
      
      data <- filtered_data$data
      
      if (nrow(data) == 0) return(NULL)
      
      #create hover text
      data$hover_text <- mapply(
        createHoverText,
        numbers = data$bill_number,
        descs = data$roll_call_desc,
        title = data$bill_title,
        date = data$roll_call_date,
        descriptions = data$bill_desc,
        urls = data$bill_url,
        pcts = data$pct_voted_for,
        vote_texts = data$vote_text,
        names = data$legislator_name,
        SIMPLIFY = FALSE  # Keep it as a list
      )
      data$hover_text <- sapply(data$hover_text, paste, collapse = " ") # Collapse the list into a single string
      # data$hover_text = "DEBUG"
      
      
      numBills <- n_distinct(data$roll_call_id) # Adjust with your actual identifier
      
      # Dynamic height calculation
      baseHeight <- 500 # Minimum height
      perBillHeight <- 10 # Height per bill
      totalHeight <- baseHeight + (numBills * perBillHeight) # Total dynamic height
      
      low_color <- if(input$party == "D") "#4575b4" else if(input$party == "R") "#d73027" else "#4575b4"
      mid_color <- "#6DA832"
      high_color <- if(input$party == "D") "#d73027" else if(input$party == "R") "#4575b4" else "#d73027"
      
      # configure y-axis sort order
      #data$legislator_name <- reorder(data$legislator_name, data$mean_partisan_metric)
      if (input$sort_by == "Name") {
        data$legislator_name <- reorder(data$legislator_name, data$legislator_name)
          } else if (input$sort_by == "Partisanship") {
            data$legislator_name <- reorder(data$legislator_name, data$mean_partisan_metric)
          } else if (input$sort_by == "District") {
            data$legislator_name <- reorder(data$legislator_name, data$district_number)
          }
      
      
      data$bill_number <- as.character(data$bill_number)
      data <- data[order(data$bill_number), ]
      data$roll_call_id <- factor(data$roll_call_id, levels = unique(data$roll_call_id))
      
      # set axis labels
      labels <- unique(data[, c("legislator_name", "district_number")])
      x_labels <- setNames(paste(labels$legislator_name, " (", labels$district_number, ")", sep = ""), labels$legislator_name)
      labels <- unique(data[, c("roll_call_id", "bill_number", "session_year")])
      y_labels <- setNames(paste(labels$bill_number, labels$session_year, sep = " - "), labels$roll_call_id)
      
      # Generate the plot
      p <- ggplot(data, aes(x = legislator_name, y = roll_call_id, fill = partisan_metric, text = hover_text)) +
        geom_tile(color = "white", linewidth = 0.1) +
        scale_fill_gradient2(low = low_color, high = high_color, mid = "#6DA832", midpoint = 1,
        ) +
        theme_minimal() +
        scale_y_discrete(labels = y_labels) + 
        scale_x_discrete(labels = x_labels, position = "top") +
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
               plot_bgcolor = "rgba(255,255,255,0.85)",  # Transparent plot background
               paper_bgcolor = "rgba(255,255,255,0.85)",
               height = totalHeight
        ) %>% 
        config(displayModeBar = FALSE)
    })
  })
  
  
  
  ########################################
  #                                      #  
  # app 2: legislator activity overview  #
  #                                      #
  ########################################  
  
  observeEvent(input$navbarPage == "app2",{
    values <- reactiveValues(party = character(0), role = character(0), final = character(0),selectedYears = list(),  year2023Active = FALSE,
                             year2024Active = FALSE)
    
    all_legislators <- unique(app_data$name)
    all_legislators_with_all <- c("All" = "All", all_legislators) #not working, also below I changed the filter away from the >0 length to != "all"
    observeEvent(input$btn_year_2023, {
      values$year2023Active <- !values$year2023Active
    })
    observe({
      shinyjs::toggleClass("btn_year_2023", "active-filter", values$year2023Active)
    })
    observeEvent(input$btn_year_2024, {
      values$year2024Active <- !values$year2024Active
    })
    observe({
      shinyjs::toggleClass("btn_year_2024", "active-filter", values$year2024Active)
    })
    observeEvent(input$btn_year_2023, {
      if("2023" %in% values$selectedYears) {
        values$selectedYears <- values$selectedYears[values$selectedYears != "2023"]
        
      } else {
        values$selectedYears <- c(values$selectedYears, "2023")
      }
    })
    observeEvent(input$btn_year_2024, {
      if("2024" %in% values$selectedYears) {
        values$selectedYears <- values$selectedYears[values$selectedYears != "2024"]
        
      } else {
        values$selectedYears <- c(values$selectedYears, "2024")
      }
    })
    observe({
      shinyjs::toggleClass("btn_year_2023", "active-filter", values$year2023Active)
      shinyjs::toggleClass("btn_year_2024", "active-filter", values$year2024Active)
      # Repeat for other buttons as needed
    })
    output$votefilterinfo <- renderUI({
      div(
        actionButton("filterinfo", icon("info-circle"))
      )
    })
    observeEvent(input$filterinfo, {
      showModal(modalDialog(
        title = "Legislator Dashboard",HTML("
        <p>This dashboard allows you to see every vote a lawmaker has taken, highlighting votes where they voted against their own party or against both parties. The data includes every committee, amendment and final vote on a bill. Use the filters to limit legislators by party or chamber. You can also filter the votes so you only see final roll call votes or just those votes against their own party.</p>
      ")
      ))
    })
    observeEvent(input$btn_party_R, {
      if ("R" %in% values$party) {
        values$party <- setdiff(values$party, "R") # Remove R if selected
      } else {
        values$party <- c(values$party, "R") # Add R if not selected
      }
    })
    observeEvent(input$btn_party_D, {
      if ("D" %in% values$party) {
        values$party <- setdiff(values$party, "D")
      } else {
        values$party <- c(values$party, "D")
      }
    })
    observeEvent(input$btn_role_Rep, {
      if ("Rep" %in% values$role) {
        values$role <- setdiff(values$role, "Rep")
      } else {
        values$role <- c(values$role, "Rep")
      }
    })
    observeEvent(input$btn_role_Sen, {
      if ("Sen" %in% values$role) {
        values$role <- setdiff(values$role, "Sen")
      } else {
        values$role <- c(values$role, "Sen")
      }
    })
    observeEvent(input$btn_final_Y, {
      if ("Y" %in% values$final){
        values$final <- setdiff(values$final,"Y")
      } else{values$final <- c(values$final,"Y")
      }
    })
    observeEvent(input$btn_final_N, {
      if ("N" %in% values$final){
        values$final <- setdiff(values$final,"N")
      } else{values$final <- c(values$final,"N")
      }
    })
    filtered_data <- reactive({
      data <- app_data
      if (length(values$party) > 0) {
        data <- data %>% filter(party %in% values$party)
      }
      if (length(values$role) > 0) {
        data <- data %>% filter(role %in% values$role)
      }
      if (length(values$final) > 0) {
        data <- data %>% filter(final %in% values$final)
      }
      if ("Independent" %in% input$voteType) {
        data <- data %>% filter(vote_with_neither == 1)
      }
      if ("Maverick" %in% input$voteType) {
        data <- data %>% filter(maverick_votes == 1)
      }
      if ("Normal" %in% input$voteType) {
        data <- data %>% filter(maverick_votes == 0 & vote_with_neither == 0)
      }
      if (!is.null(input$searchText) && input$searchText != "") {
        data <- data %>% filter(grepl(input$searchText, title, fixed = TRUE) | grepl(input$searchText, description, fixed = TRUE))
      }
      # Apply legislator filter based on dropdown selection
      if (!is.null(input$legislator) && input$legislator != "All") {
        data <- data %>% filter(grepl(input$legislator, legislator_name, fixed = TRUE))
      }
      
      if(length(values$selectedYears) > 0) {
        data <- data %>% filter(session_year %in% values$selectedYears)
      }
      
      data
    })
    filtered_legdata <- reactive({
      data <- app_data
      if (length(values$party) > 0) {
        data <- data %>% filter(party %in% values$party)
      }
      if (length(values$role) > 0) {
        data <- data %>% filter(role %in% values$role)
      }
      if (length(values$final) > 0) {
        data <- data %>% filter(final %in% values$final)
      }
      if ("Independent" %in% input$voteType) {
        data <- data %>% filter(vote_with_neither == 1)
      }
      if ("Maverick" %in% input$voteType) {
        data <- data %>% filter(maverick_votes == 1)
      }
      if ("Normal" %in% input$voteType) {
        data <- data %>% filter(maverick_votes == 0 & vote_with_neither == 0)
      }
      if(length(values$selectedYears) > 0) {
        data <- data %>% filter(session_year %in% values$selectedYears)
      }
      # Remember the current selection to possibly reapply it later
      data
    })
    observe({
      current_selection <- input$legislator
      filtered_legislators <- unique(filtered_legdata()$name)
      
      if (!is.null(input$legislator_enter_pressed)) {
        data <- data %>% filter(grepl(input$legislator_enter_pressed, title, fixed = TRUE) | grepl(input$legislator_enter_pressed, description, fixed = TRUE))}
      else{
        # Update dropdown choices. This does not inherently cause recursion.
        updateSelectizeInput(session, "legislator", choices = filtered_legislators,selected = current_selection)
      }})
    count_independent_votes <- reactive({
      sum(filtered_data()$vote_with_neither == 1)
    })
    count_maverick_votes <- reactive({
      sum(filtered_data()$maverick_votes == 1)
    })
    count_normal_votes <- reactive({
      sum(filtered_data()$maverick_votes == 0 & filtered_data()$vote_with_neither == 0)
    })
    pct_independent_votes <- reactive({
      round(count_independent_votes()/sum(!is.na(filtered_data()$vote_with_neither)),3)*100
    })
    pct_maverick_votes <- reactive({
      round(count_maverick_votes()/sum(!is.na(filtered_data()$vote_with_neither)),3)*100
    })
    pct_normal_votes <- reactive({
      round(count_normal_votes()/sum(!is.na(filtered_data()$vote_with_neither)),3)*100
    })
    observe({
      current_selections <- input$voteType
      choices_vector <- c("Independent", "Maverick", "Normal")
      names(choices_vector) <- c(
        paste0("Voted Against Both Parties (", count_independent_votes()," - ",pct_independent_votes(), "%)"),
        paste0("Voted With Other Party (", count_maverick_votes()," - ",pct_maverick_votes(), "%)"),
        paste0("Voted With Their Party (", count_normal_votes()," - ",pct_normal_votes(), "%)")
      )
      
      updateCheckboxGroupInput(session, "voteType",
                               #label = "Key Vote Type:",
                               choices = choices_vector,selected = current_selections)
    })
    output$legislatorProfile <- renderUI({
      # Assuming 'heatmap_data' contains all the necessary legislator information
      selected_legislator <- input$legislator
      if (!is.null(selected_legislator) && selected_legislator != "") {
        legislator_info <- app_data %>%
          filter(name == selected_legislator) %>% distinct(name, district, party, role, ballotpedia2) %>% slice(1)
        div(
          h3(a(href = legislator_info$ballotpedia2, target = "_blank", selected_legislator)),
          p("District: ", legislator_info$district),
          p("Party: ", legislator_info$party),
          p("Chamber: ", legislator_info$role)
        )
      }
    })
    current_page <- reactiveVal(1)
    items_per_page <- reactive({ as.numeric(input$items_per_page) })
    paginated_data <- reactive({
      if (nrow(filtered_data()) == 0) return(data.frame())
      start_index <- (current_page() - 1) * items_per_page() + 1
      end_index <- min(nrow(filtered_data()), start_index + items_per_page() - 1)
      filtered_data()[start_index:end_index, ]
    })
    output$votesDisplay <- renderUI({
      data <- paginated_data()
      if (is.null(input$legislator) || input$legislator == "" || nrow(data) == 0) {
        return(div(class = "no-data", "No bills available for display."))
      }
      
      # Dynamically create UI elements for each bill
      ui_elements <- lapply(unique(data$number), function(bill) {
        bill_data <- data[data$number == bill, ]
        bill_title <- unique(bill_data$title)[1]
        descriptions <- unique(bill_data$description)
        html_string <- paste0("<h4>", bill_title, " - ", "<a href='", bill_data$url[1], "' target='_blank'>", bill_data$number[1],"</a></h4>","<h5>",bill_data$session_name[1],"</h5><p>",bill_data$description[1],"</p>")
        
        div(class = 'bill-container',
            html_content <- HTML(html_string),
            actionButton(inputId = sprintf('vote-details-link-%s', gsub("[^A-Za-z0-9]", "", bill)), 
                         label = "Vote Details Info", 
                         class = "btn btn-info vote-details-button"),
            div(
              id = sprintf('vote-details-%s', gsub("[^A-Za-z0-9]", "", bill)),
              class = 'vote-details',
              style = 'display:none;',
              lapply(unique(bill_data$roll_call_id), function(roll_call) {
                roll_call_data <- bill_data[bill_data$roll_call_id == roll_call, ]
                
                pct_yes <- paste0(round(roll_call_data$true_pct[1] * 100, 2), "%")
                date_format <- format(as.Date(roll_call_data$date),"%b %d, %Y")
                special_vote_class <- ifelse(roll_call_data$vote_with_neither[1] == 1, "independent-vote",
                                             ifelse(roll_call_data$maverick_votes[1] == 1, "maverick-vote", "regular-vote"))
                
                special_vote_text <- ifelse(roll_call_data$vote_with_neither[1] == 1,
                                            "This legislator voted <b><i>against</i></b> the majorities of both parties.",
                                            ifelse(roll_call_data$maverick_votes[1] == 1,
                                                   sprintf("This legislator voted <b><i>against</i></b> the majority of their party (%s) and <b><i>with</i></b> the majority of the other party.", roll_call_data$party[1]),
                                                   "This legislator voted with their party majority."))
                
                legislator_vote <- paste(roll_call_data$name[1], "voted", roll_call_data$vote_text[1])
                
                div(class = paste("votes",special_vote_class), 
                    HTML(paste0(
                      "<p>",roll_call_data$desc," - <b>", date_format,"</b></p><p>", pct_yes, " of legislators voted Yea. </p><p>", special_vote_text, "</p><p>", legislator_vote,"</p><p class='disclaimer'><i>This vote wasn't necessarily a vote of the bill, and it could have been a vote on an amendment. For more details, examine the bill's <a href='",roll_call_data$state_link,"' target='_blank'>vote information</a> on the Legislature's website or examine the <a href='", bill_data$url[1], "' target='_blank'>bill page.</a></i></p>"))
                )
              }) #close lapply
            ) #close vote detail div
        ) #close bill container div
      }) #close other lapply
      do.call(tagList, ui_elements)
    }) #close renderui
    observe({
      if("R" %in% values$party) {
        runjs('$("#btn_party_R").addClass("selected");')
      } else {
        runjs('$("#btn_party_R").removeClass("selected");')
      }
    })
    observe({
      if("D" %in% values$party) {
        runjs('$("#btn_party_D").addClass("selected");')
      } else {
        runjs('$("#btn_party_D").removeClass("selected");')
      }
      # Repeat for other buttons
    })
    observe({
      if("Rep" %in% values$role) {
        runjs('$("#btn_role_Rep").addClass("selected");')
      } else {
        runjs('$("#btn_role_Rep").removeClass("selected");')
      }
      # Repeat for other buttons
    })
    observe({
      if("Sen" %in% values$role) {
        runjs('$("#btn_role_Sen").addClass("selected");')
      } else {
        runjs('$("#btn_role_Sen").removeClass("selected");')
      }
      # Repeat for other buttons
    })
    observe({
      if("N" %in% values$final) {
        runjs('$("#btn_final_N").addClass("selected");')
      } else {
        runjs('$("#btn_final_N").removeClass("selected");')
      }
      # Repeat for other buttons
    })
    observe({
      if("Y" %in% values$final) {
        runjs('$("#btn_final_Y").addClass("selected");')
      } else {
        runjs('$("#btn_final_Y").removeClass("selected");')
      }
    })
    observeEvent(input$prev_page, { if (current_page() > 1) current_page(current_page() - 1) })
    observeEvent(input$next_page, {
      total_items <- nrow(filtered_data())
      if ((current_page() * items_per_page) < total_items) current_page(current_page() + 1)
    })
    observeEvent(input$first_page, {
      current_page(1)
    })
    observeEvent(input$info, {
      showModal(modalDialog(
        title = "Vote Types",HTML("
        <p>There are three main ways legislators vote:</p>
        <p>1. They vote with the majority of their own party, even when it means voting with a majority of the opposing party on bipartisan or unanimous votes.</p>
        <p>2. They vote against the majority of legislators in both parties.</p>
        <p>3. Or they vote against the majority of the legislators in their own party AND with the majority of the opposing party.</p>
      ")
      ))
    })
    observeEvent(input$last_page, {
      total_pages <- ceiling(nrow(filtered_data()) / items_per_page)
      current_page(total_pages)
      shinyjs::toggleState("first_page", condition = current_page() > 1)
      shinyjs::toggleState("prev_page", condition = current_page() > 1)
      shinyjs::toggleState("next_page", condition = current_page() < total_pages)
      shinyjs::toggleState("last_page", condition = current_page() < total_pages)
    })
    output$page_info <- renderUI({
      total_items <- nrow(filtered_data())
      start_item <- (current_page() - 1) * items_per_page() + 1
      end_item <- min(start_item + items_per_page() - 1, total_items)
      
      # Using HTML() to include a line break (<br>) or any other HTML tags
      formatted_start_item <- format(start_item, big.mark = ",")
      formatted_end_item <- format(end_item, big.mark = ",")
      formatted_total_items <- format(total_items, big.mark = ",")
      
      HTML(paste0("Showing items ", formatted_start_item, " to<br>", formatted_end_item, " of ", formatted_total_items))
    })
    output$filterFeedback <- renderText({
      filtered_count <- nrow(filtered_data())
      formatted_count <- format(filtered_count, big.mark = ",")
      paste("Showing", formatted_count, "results based on current filters.")
    })
    shinyjs::runjs('
    $(document).on("click", ".vote-details-button", function() {
      var detailsId = $(this).attr("id").replace("vote-details-link-", "vote-details-");
      $("#" + detailsId).toggle();
    });
  ')})
}
