# voting_history_module.R

library(shiny)
library(dplyr)
library(lubridate)
library(shinyjs)

votingHistoryUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "header-section", "Voting History"),
    div(class="flex-section",
        checkboxGroupInput(ns("voteType"), "Vote Type:",
                           choices = c("Independent", "Maverick", "Normal"),
                           selected = c("Independent", "Maverick", "Normal")),
        textInput(ns("searchText"), "Search Bills:", ""),
        actionButton(ns("btn_year_2023"), "2023"),
        actionButton(ns("btn_year_2024"), "2024"),
        selectInput(ns("items_per_page"), "Items per page:",
                    choices = c(10, 25, 50, 100),
                    selected = 25)),
    uiOutput(ns("filterCounts")),
    uiOutput(ns("votesDisplay")),
    uiOutput(ns("paginationControls"))
  )
}

votingHistoryServer <- function(id, selected_legislator) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    values <- reactiveValues(
      selectedYears = c("2023", "2024"),
      year2023Active = TRUE,
      year2024Active = TRUE
    )
    
    observeEvent(input$btn_year_2023, {
      values$year2023Active <- !values$year2023Active
      if("2023" %in% values$selectedYears) {
        values$selectedYears <- setdiff(values$selectedYears, "2023")
      } else {
        values$selectedYears <- c(values$selectedYears, "2023")
      }
    })
    
    observeEvent(input$btn_year_2024, {
      values$year2024Active <- !values$year2024Active
      if("2024" %in% values$selectedYears) {
        values$selectedYears <- setdiff(values$selectedYears, "2024")
      } else {
        values$selectedYears <- c(values$selectedYears, "2024")
      }
    })
    
    observe({
      shinyjs::toggleClass(ns("btn_year_2023"), "active-filter", values$year2023Active)
      shinyjs::toggleClass(ns("btn_year_2024"), "active-filter", values$year2024Active)
    })
    
    filtered_voting_data <- reactive({
      req(selected_legislator())
      
      data <- app02_leg_activity %>%
        filter(legislator_name == selected_legislator())
      
      if (length(values$selectedYears) > 0) {
        data <- data %>% filter(session_year %in% values$selectedYears)
      }
      
      if (!is.null(input$searchText) && input$searchText != "") {
        search_pattern <- tolower(input$searchText)
        data <- data %>% 
          filter(
            grepl(search_pattern, tolower(bill_title), fixed = TRUE) | 
              grepl(search_pattern, tolower(bill_desc), fixed = TRUE)
          )
      }
      
      data
    })
    
    vote_counts <- reactive({
      data <- filtered_voting_data()
      list(
        Independent = sum(data$vote_with_neither == 1, na.rm = TRUE),
        Maverick = sum(data$maverick_votes == 1, na.rm = TRUE),
        Normal = sum(data$maverick_votes == 0 & data$vote_with_neither == 0, na.rm = TRUE)
      )
    })
    
    observe({
      counts <- vote_counts()
      total_votes <- sum(unlist(counts))
      
      choices <- lapply(names(counts), function(type) {
        count <- counts[[type]]
        percentage <- if(total_votes > 0) round(count / total_votes * 100, 1) else 0
        paste0(type, " (", count, " - ", percentage, "%)")
      })
      
      updateCheckboxGroupInput(session, "voteType",
                               choices = setNames(names(counts), choices),
                               selected = input$voteType)
    })
    
    current_page <- reactiveVal(1)
    
    paginated_data <- reactive({
      data <- filtered_voting_data()
      
      if (!is.null(input$voteType) && length(input$voteType) > 0) {
        data <- data %>%
          filter(
            ("Independent" %in% input$voteType & vote_with_neither == 1) |
              ("Maverick" %in% input$voteType & maverick_votes == 1) |
              ("Normal" %in% input$voteType & maverick_votes == 0 & vote_with_neither == 0)
          )
      }
      
      data <- data %>% arrange(desc(ymd(roll_call_date)), desc(vote_with_neither), desc(maverick_votes))
      
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
        details_id <- sprintf('vote-details-%s', gsub("[^A-Za-z0-9]", "", bill_data$bill_number))
        
        div(class = 'bill-container',
            h4(bill_data$bill_title, " - ", 
               a(href = bill_data$bill_url, target = "_blank", bill_data$bill_number)),
            h5(bill_data$session),
            p(bill_data$bill_desc),
            actionButton(inputId = ns(sprintf('vote-details-link-%s', gsub("[^A-Za-z0-9]", "", bill_data$bill_number))), 
                         label = "Vote Details Info", 
                         class = "btn btn-info vote-details-button"),
            shinyjs::hidden(
              div(id = ns(details_id),
                  class = 'vote-details',
                  p(bill_data$roll_call_desc, " - ", strong(format(as.Date(bill_data$roll_call_date), "%b %d, %Y"))),
                  p(paste0(round(bill_data$pct_of_total * 100, 2), "% of legislators voted Yea.")),
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
                                "' target='_blank'>vote information</a> on the Legislature's website or examine the ",
                                "<a href='", bill_data$bill_url, "' target='_blank'>bill page.</a>")))
              )
            )
        )
      })
      do.call(tagList, ui_elements)
    })
    observe({
      lapply(seq_len(nrow(paginated_data())), function(i) {
        bill_data <- paginated_data()[i, ]
        button_id <- sprintf('vote-details-link-%s', gsub("[^A-Za-z0-9]", "", bill_data$bill_number))
        details_id <- sprintf('vote-details-%s', gsub("[^A-Za-z0-9]", "", bill_data$bill_number))
        
        observeEvent(input[[button_id]], {
          shinyjs::toggle(id = details_id)
        })
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