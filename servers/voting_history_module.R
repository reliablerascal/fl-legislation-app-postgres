# voting_history_module.R
votingHistoryUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "header-section", "Detailed Voting History"),
    div(class="flex-section",
        uiOutput(ns("voteTypeUI")),  
        textInput(ns("searchText"), "Search Bills:", ""),
        #actionButton(ns("btn_year_2023"), "2023"),
        #actionButton(ns("btn_year_2024"), "2024"),
        div(class="year-button-container",  # <-- WRAP HERE!
            uiOutput(ns("yearButtonsUI"))),
        selectInput(ns("items_per_page"), "Items per page:",
                    choices = c(10, 25, 50, 100),
                    selected = 25)),
    uiOutput(ns("filterCounts")),
    uiOutput(ns("votesDisplay")),
    uiOutput(ns("paginationControls")),
    verbatimTextOutput(ns("debugOutput"))
    
  )
}

votingHistoryServer <- function(id, selected_legislator) {
  last_legislator <- reactiveVal(NULL)
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(app02_leg_activity)
    
    # Treat session_year as numeric consistently
    available_years <- reactive({
      years <- unique(app02_leg_activity$session_year)
      years <- years[!is.na(years)]
      years <- suppressWarnings(as.numeric(years))
      sort(unique(years), decreasing = TRUE)
    })
    
    values <- reactiveValues(
      selectedYears = numeric(0),
      yearActive = list(),
      selectedVoteTypes = c('Voted Against Both Parties', 'Voted With Opposing Party', 'Voted With Own Party'),
      popupObservers = list()
    )
    
    observeEvent(available_years(), {
      years <- available_years()
      values$selectedYears <- years
      values$yearActive <- setNames(as.list(rep(TRUE, length(years))), as.character(years))
    }, once = TRUE)
    
    output$yearButtonsUI <- renderUI({
      req(available_years())
      tagList(
        lapply(available_years(), function(year) {
          actionButton(ns(paste0("btn_year_", year)), label = year, class = "btn-filter")
        })
      )
    })
    
    observe({
      req(available_years())
      years <- available_years()
      for (year in years) {
        local({
          y <- year
          btn_id <- ns(paste0("btn_year_", y))
          if (!is.null(input[[btn_id]])) {
            observeEvent(input[[btn_id]], {
              year_key <- as.character(y)
              current_state <- isTRUE(values$yearActive[[year_key]])
              values$yearActive[[year_key]] <- !current_state
              
              if (current_state) {
                values$selectedYears <- setdiff(values$selectedYears, y)
              } else {
                values$selectedYears <- union(values$selectedYears, y)
              }
            }, ignoreInit = TRUE)
          }
        })
      }
    })
    
    observe({
      req(available_years())
      years <- available_years()
      for (year in years) {
        year_char <- as.character(year)
        btn_id <- ns(paste0("btn_year_", year_char))
        state <- values$yearActive[[year_char]]
        if (!is.null(state)) {
          shinyjs::toggleClass(id = btn_id, class = "active-filter", condition = state)
        }
      }
    })
    
    output$voteTypeUI <- renderUI({
      req(values$selectedVoteTypes)
      choices <- c('Voted Against Both Parties', 'Voted With Opposing Party', 'Voted With Own Party')
      selected <- intersect(values$selectedVoteTypes, choices)
      checkboxGroupInput(ns("voteType"), "Vote Type:", choices = choices, selected = selected)
    })
    
    observe({
      req(input$voteType)
      values$selectedVoteTypes <- input$voteType
    })
    
    # Reset filter if legislator changes
    observeEvent(selected_legislator(), {
      last_legislator(selected_legislator())
    }, ignoreInit = TRUE)
    
    filtered_voting_data <- reactive({
      req(selected_legislator())
      
      data <- as.data.table(app02_leg_activity) %>%
        filter(legislator_name == selected_legislator())
      
      if (length(values$selectedYears) > 0) {
        data <- data %>% filter(session_year %in% values$selectedYears)
      }
      
      if (!is.null(input$searchText) && input$searchText != "") {
        search_pattern <- tolower(input$searchText)
        # data.table syntax for fast search
        data <- data[tolower(bill_title) %like% search_pattern |
                       tolower(bill_desc) %like% search_pattern]
      }
      
      if (length(values$selectedVoteTypes) > 0) {
        data <- data %>%
          filter(
            ('Voted Against Both Parties' %in% values$selectedVoteTypes & vote_with_neither == 1) |
              ('Voted With Opposing Party' %in% values$selectedVoteTypes & maverick_votes == 1) |
              ('Voted With Own Party' %in% values$selectedVoteTypes &
                 maverick_votes == 0 & vote_with_neither == 0 & vote_with_same == 1)
          )
      }
      
      
      data
    })
    
    vote_counts <- reactive({
      req(selected_legislator())
      data <- as.data.table(app02_leg_activity) %>%
        filter(legislator_name == selected_legislator())
      list(
        'Voted Against Both Parties' = sum(data$vote_with_neither == 1, na.rm = TRUE),
        'Voted With Opposing Party' = sum(data$maverick_votes == 1, na.rm = TRUE),
        'Voted With Own Party' = sum(data$maverick_votes == 0 & data$vote_with_neither == 0 & data$vote_with_same == 1, na.rm = TRUE)
      )
    })
    
    observe({
      counts <- vote_counts()
      req(counts)
      
      total_votes <- sum(unlist(counts))
      
      display_labels <- sapply(names(counts), function(type) {
        count <- counts[[type]]
        percentage <- if (total_votes > 0) round(count / total_votes * 100, 1) else 0
        paste0(type, " (", count, " - ", percentage, "%)")
      }, USE.NAMES = FALSE)
      
      internal_values <- names(counts)
      
      if (length(display_labels) == length(internal_values) && length(internal_values) > 0) {
        choices_vector <- setNames(internal_values, display_labels)
        updateCheckboxGroupInput(session, "voteType", choices = choices_vector, selected = values$selectedVoteTypes)
      } else {
        updateCheckboxGroupInput(session, "voteType", choices = list(), selected = character(0))
      }
    })
    
    current_page <- reactiveVal(1)
    
    paginated_data <- reactive({
      data <- filtered_voting_data()
      data <- data %>%
        arrange(desc(ymd(roll_call_date)), desc(vote_with_neither), desc(maverick_votes))
      
      items_per_page <- as.numeric(input$items_per_page)
      start_index <- (current_page() - 1) * items_per_page + 1
      end_index <- min(nrow(data), start_index + items_per_page - 1)
      
      if (start_index > nrow(data)) {
        data[0, ]
      } else {
        data[start_index:end_index, ]
      }
    })
    
    output$votesDisplay <- renderUI({
      data <- paginated_data()
      
      if (nrow(data) == 0) {
        return(div(class = "no-data", "No bills available for display."))
      }
      
      ui_elements <- lapply(seq_len(nrow(data)), function(i) {
        bill_data <- data[i, ]
        bill_id <- gsub("[^A-Za-z0-9]", "", bill_data$bill_number)
        
        div(class = 'bill-container',
            h4(bill_data$bill_title, " - ",
               a(href = bill_data$bill_url, target = "_blank", bill_data$bill_number)),
            h5(bill_data$session),
            p(bill_data$bill_desc),
            actionButton(inputId = ns(sprintf('vote-details-link-%s', bill_id)),
                         label = "Vote Details Info",
                         class = "btn btn-info vote-details-button")
        )
      })
      
      do.call(tagList, ui_elements)
    })
    
    observe({
      lapply(isolate(values$popupObservers), function(obs) {
        if (!is.null(obs)) obs$destroy()
      })
      isolate(values$popupObservers <- list())
      
      data <- paginated_data()
      lapply(seq_len(nrow(data)), function(i) {
        bill_data <- data[i, ]
        bill_id <- gsub("[^A-Za-z0-9]", "", bill_data$bill_number)
        button_id <- sprintf('vote-details-link-%s', bill_id)
        
        obs <- observeEvent(input[[button_id]], {
          rc_id <- bill_data$roll_call_id
          rc_votes <- app02_leg_activity[app02_leg_activity$roll_call_id == rc_id, ]
          
          # Democratic breakdown
          D_present <- sum(rc_votes$party == "D")
          D_yea <- sum(rc_votes$party == "D" & rc_votes$vote_text == "Yea")
          D_nay <- sum(rc_votes$party == "D" & rc_votes$vote_text == "Nay")
          D_yea_pct <- if (D_present > 0) round(100 * D_yea / D_present, 0) else NA
          D_nay_pct <- if (D_present > 0) round(100 * D_nay / D_present, 0) else NA
          
          # Republican breakdown
          R_present <- sum(rc_votes$party == "R")
          R_yea <- sum(rc_votes$party == "R" & rc_votes$vote_text == "Yea")
          R_nay <- sum(rc_votes$party == "R" & rc_votes$vote_text == "Nay")
          R_yea_pct <- if (R_present > 0) round(100 * R_yea / R_present, 0) else NA
          R_nay_pct <- if (R_present > 0) round(100 * R_nay / R_present, 0) else NA
          
          showModal(modalDialog(
            title = paste("Vote Details for Bill", bill_data$bill_number),
            p(bill_data$roll_call_desc, " - ", strong(format(as.Date(bill_data$roll_call_date), "%b %d, %Y"))),
            p(paste0(round(bill_data$pct_of_total * 100, 1), "% of legislators voted Yea.")),
            # --- Party Breakdown Table ---
            HTML(sprintf(
              "<b>Party Breakdown:</b>
   <table class='vote-table' style='margin-bottom:12px;'>
     <tr>
       <th style='text-align:left;'></th>
       <th>Yea</th>
       <th>Nay</th>
     </tr>
     <tr>
       <td style='text-align:left;'><span style='color:#4575b4;'>Democrats</span></td>
       <td>%d / %d (%.1f%%)</td>
       <td>%d / %d (%.1f%%)</td>
     </tr>
     <tr>
       <td style='text-align:left;'><span style='color:#d73027;'>Republicans</span></td>
       <td>%d / %d (%.1f%%)</td>
       <td>%d / %d (%.1f%%)</td>
     </tr>
   </table>",
              D_yea, D_present, D_yea_pct, D_nay, D_present, D_nay_pct,
              R_yea, R_present, R_yea_pct, R_nay, R_present, R_nay_pct
            )),
            p(HTML(paste0(
              ifelse(bill_data$vote_with_neither == 1,
                     "This legislator voted <b><i>against</i></b> the majorities of both parties.",
                     ifelse(bill_data$maverick_votes == 1,
                            sprintf("This legislator voted <b><i>against</i></b> the majority of their party (%s) and <b><i>with</i></b> the majority of the other party.", bill_data$party),
                            "This legislator voted with their party majority."))
            ))),
            p(paste(bill_data$legislator_name, "voted", bill_data$vote_text)),
            p(class = 'disclaimer',
              HTML(paste0("This vote wasn't necessarily a vote of the bill, and it could have been a vote on an amendment. ",
                          "For more details, examine the bill's <a href='", bill_data$state_link,
                          "' target='_blank'>vote information</a> or the ",
                          "<a href='", bill_data$bill_url, "' target='_blank'>bill page.</a>"))),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }, ignoreInit = TRUE)
        
        isolate(values$popupObservers[[button_id]] <- obs)
      })
    })
    
    output$paginationControls <- renderUI({
      total_items <- nrow(filtered_voting_data())
      items_per_page <- as.numeric(input$items_per_page)
      total_pages <- ceiling(total_items / items_per_page)
      
      tagList(
        actionButton(ns("first_page"), "First"),
        actionButton(ns("prev_page"), "Previous"),
        span(paste("Page", current_page(), "of", total_pages)),
        actionButton(ns("next_page"), "Next"),
        actionButton(ns("last_page"), "Last")
      )
    })
    
    observeEvent(input$first_page, { current_page(1) })
    observeEvent(input$prev_page, { if (current_page() > 1) current_page(current_page() - 1) })
    observeEvent(input$next_page, {
      total_pages <- ceiling(nrow(filtered_voting_data()) / as.numeric(input$items_per_page))
      if (current_page() < total_pages) current_page(current_page() + 1)
    })
    observeEvent(input$last_page, {
      total_pages <- ceiling(nrow(filtered_voting_data()) / as.numeric(input$items_per_page))
      current_page(total_pages)
    })
  })
}
