# SERVER3_DISTRICT_CONTEXT.R
# This app shows demographic and electoral political leaning characteristics of each district compared to legislators' voting records

########################################
#                                      #  
# app 3: district context              #
#                                      #
########################################
source("servers/voting_history_module.R")
plain_english_election_label <- function(code) {
  # Pattern: <YY>_<TYPE>
  # e.g., "24_PRES", "22_GOV", "16_SEN", "18_AG"
  if (!grepl("^[0-9]{2}_[A-Z]+", code)) return(code)
  year <- paste0("20", substr(code, 1, 2))
  type <- sub("^[0-9]{2}_", "", code)
  type_map <- c(
    PRES = "U.S. Presidential Election",
    SEN = "U.S. Senate race",
    GOV = "Governor's race",
    AG = "Attorney General's race",
    AUD = "State Auditor's race",
    SOS = "Secretary of state's race",
    TREAS = "Treasurer's race",
    CMPTR = "Comptroller's race",
    CONG = "U.S. House results within this district",
    COMP = "composite vote"
    # Add more as needed!
  )
  if (type %in% names(type_map)) {
    return(paste(year, type_map[[type]]))
  } else if (grepl("^SC", type)) {
    seat <- sub("^SC", "", type)
    return(paste(year, "state supreme court seat", seat))
  } else {
    # fallback, just use code as is
    return(paste(year, type))
  }
}

get_election_results_list <- function(data_district) {
  d_cols <- grep("^pct_D_", names(data_district), value = TRUE)
  elections <- gsub("^pct_D_", "", d_cols)
  election_items <- sapply(elections, function(elec) {
    pct_D <- data_district[[paste0("pct_D_", elec)]]
    pct_R <- data_district[[paste0("pct_R_", elec)]]
    pct_D_fmt <- if (!is.na(pct_D)) paste0(sprintf("%.1f", 100 * pct_D), "%") else "N/A"
    pct_R_fmt <- if (!is.na(pct_R)) paste0(sprintf("%.1f", 100 * pct_R), "%") else "N/A"
    label <- plain_english_election_label(elec)
    paste0(
      '<li><strong>', label, ':</strong> ',
      'Republican ', pct_R_fmt, ', Democratic ', pct_D_fmt, '</li>'
    )
  })
  paste(election_items, collapse = "\n")
}

get_election_results_table <- function(data_district) {
  d_cols <- grep("^pct_D_", names(data_district), value = TRUE)
  elections <- gsub("^pct_D_", "", d_cols)
  rows <- sapply(elections, function(elec) {
    pct_D <- data_district[[paste0("pct_D_", elec)]]
    pct_R <- data_district[[paste0("pct_R_", elec)]]
    pct_D_fmt <- if (!is.na(pct_D)) paste0(sprintf("%.1f", 100 * pct_D), "%") else "N/A"
    pct_R_fmt <- if (!is.na(pct_R)) paste0(sprintf("%.1f", 100 * pct_R), "%") else "N/A"
    label <- plain_english_election_label(elec)
    paste0(
      paste0(
        '<tr>',
        '<td><strong>', label, ':</strong></td>',
        '<td><span class="badge-rep">Republican <span class="badge-pct">', pct_R_fmt, '</span></span></td>',
        '<td><span class="badge-dem">Democratic <span class="badge-pct">', pct_D_fmt, '</span></span></td>',
        '</tr>'
      )
    )
  })
  paste0('<table class="election-results"><tbody>', paste(rows, collapse = "\n"), '</tbody></table>')
}

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
    if (length(years) == 1) {as.character(years)}
    else if (length(years) == 2) {paste(years[1], "and", years[2])}
    else {if (all(diff(years) == 1)) {paste0(min(years), "-", max(years))}
      else {paste0(min(years), "-", max(years))      } # Or alternatively, list them: paste(paste(head(years, -1), collapse=", "), "and", tail(years, 1))
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
  sort_legislators_by_last_name <- function(legislators) {
    last_names <- sapply(strsplit(legislators, " "), function(x) x[length(x)])
    legislators[order(last_names)]
  }
  
  
  count_legislators_in_party <- function(data, party, chamber) {
    data %>%
      filter(party == !!party, chamber == !!chamber) %>%
      tally()
  }
  
  # --- Dynamic filter UI ---
  output$dynamicFilters3 <- renderUI({
    chambers <- unique(app03_district_context$chamber)
    current_chamber <- if (!is.null(pending_selection$chamber)) {
      pending_selection$chamber
    } else if (!is.null(input$chamber3)) {
      input$chamber3
    } else {
      chambers[1]
    }
    
    legis_filtered <- app03_district_context %>%
      filter(chamber == current_chamber) %>%
      arrange(legislator_name)
    sorted_legislators <- sort_legislators_by_last_name(legis_filtered$legislator_name)
    legis_choices <- setNames(sorted_legislators, legis_filtered$label[match(sorted_legislators, legis_filtered$legislator_name)])
    current_legislator <- if (!is.null(pending_selection$legislator) && pending_selection$legislator %in% sorted_legislators) {
      pending_selection$legislator
    } else if (!is.null(input$legislator3) && input$legislator3 %in% sorted_legislators) {
      input$legislator3
    } else if (length(sorted_legislators) > 0) {
      sorted_legislators[1]
    } else {
      NA_character_
    }
    
    req(nrow(legis_filtered) > 0)
    
    div(
      class = "d-flex justify-content-center align-items-center gap-2",    style = "padding-top: 10px;",  # gap-2 for tight spacing; increase if needed
      div(style = "min-width: 220px;", selectInput("chamber3", "Chamber", choices = chambers, selected = current_chamber)),
      div(style = "min-width: 220px;", selectInput("legislator3", "Legislator", choices = legis_choices, selected = current_legislator))
    )
    
  })
  
  # --- Observers ---
  
  
  
  # --- Main reactive to retrieve selected district's row ---
  qry_demo_district <- reactive({
    req(input$legislator3, input$chamber3)
    row <- app03_district_context %>%
      filter(
        legislator_name == input$legislator3,
        chamber == input$chamber3
      )
    req(nrow(row) >= 1)
    if (nrow(row) > 1) print(row)
    
    if (nrow(row) > 1) {
      warning(paste("Multiple entries for legislator", input$legislator3, "in chamber", input$chamber3, "- using first."))
    }
    row[1, ]  # Return just the first row
    
  })
  ########################################
  #                                      #  
  # comparative partisanship             #
  #                                      #
  ######################################## 
  
  output$helper3_party_loyalty <- renderUI({
    data_district <- qry_demo_district()
    same_party <- if (data_district$party == "R") "Republican" else "Democrat"
    same_party_adj <- if (data_district$party == "R") "Republican" else "Democratic"
    indep_rank <- if (data_district$party == "R") data_district$rank_independent_R else data_district$rank_independent_D
    
    n_legislators_in_party <- count_legislators_in_party(app03_district_context, data_district$party, data_district$chamber)$n
    rank_leg <- if (data_district$party == "R") data_district$rank_partisan_leg_R else data_district$rank_partisan_leg_D
    
    
    HTML(paste0(
      '<div class="flex-item legislative-voting">',
      '<h3 class="flex-header-section">DISTRICT POLITICAL LEAN</h3>',
      '<h4 class="legislator-name">', data_district$legislator_name, '\'S VOTING RECORD:</h4>',
      '<ul class="main-list">',
      '<li><div>Party Loyalty Ranking:</div> <div><span class="stat-bold">#', rank_leg, '</span> most loyal out of <span class="stat-bold">', n_legislators_in_party, '</span> ', data_district$chamber, ' ', same_party, 's</div></li>',
      
      '<li><div>Independence Ranking:</div> ',
      '<div><span class="stat-bold">#', indep_rank, '</span> most independent out of <span class="stat-bold">', n_legislators_in_party, '</span> ',
      data_district$chamber, ' ', same_party, 's</div>',
      '<div>(Based on <span class="stat-bold">', data_district$leg_n_votes_independent, '</span> votes against bipartisan majorities.)</div></li>',
      '<li><div>Overall Independence Rank:</div> ',
      '<div><span class="stat-bold">#', data_district$rank_independent_all, '</span> most independent in the ',data_district$chamber, '</div>',
      '<div>(Measures how often they voted against both party majorities.)</div></li>',
      '<li>Based on <span class="stat-bold">', data_district$leg_n_votes_denom_loyalty, '</span> key votes:',
      '<ul>',
      '<li>Voted with the ', same_party_adj, ' Party: <span class="stat-bold">', data_district$leg_n_votes_party_line_partisan, '</span> ',
      '(<span class="percentage">', percent(data_district$leg_n_votes_party_line_partisan / data_district$leg_n_votes_denom_loyalty, accuracy = 0.1), '</span>)</li>',
      '<li>Voted against the ', same_party_adj, ' Party: <span class="stat-bold">', data_district$leg_n_votes_cross_party, '</span> ',
      '(<span class="percentage">', percent(data_district$leg_n_votes_cross_party / data_district$leg_n_votes_denom_loyalty, accuracy = 0.1), '</span>)</li>',
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
  
  output$helper3_district_lean <- renderUI({
    data_district <- qry_demo_district()
    req(nrow(data_district) > 0,
        !is.na(data_district$avg_party_lean),
        !is.na(data_district$avg_party_lean_points_abs),
        !is.na(data_district$avg_pct_R),
        !is.na(data_district$avg_pct_D))
    
    district_lean_party_abbr <- data_district$avg_party_lean
    district_lean_party_name <- ifelse(district_lean_party_abbr == "R", "Republican", "Democratic")
    district_rank_col <- ifelse(district_lean_party_abbr == "R", data_district$rank_partisan_dist_R, data_district$rank_partisan_dist_D)
    district_rank_formatted <- ifelse(is.na(district_rank_col), "?", as.integer(district_rank_col))
    
    lean_value_abs_formatted <- format(round(data_district$avg_party_lean_points_abs, 1), nsmall = 1)
    lean_display_string <- paste0(district_lean_party_abbr, "+", lean_value_abs_formatted)
    winning_party_name_desc <- ifelse(district_lean_party_abbr == "R", "Republicans", "Democrats")
    lean_description <- paste0(
      "This means ", winning_party_name_desc,
      " on average won recent statewide elections by ",
      lean_value_abs_formatted, " percentage points in this district."
    )
    n_districts <- if (data_district$chamber == "House") 120 else 40
    
    avg_pct_R_formatted <- scales::percent(data_district$avg_pct_R, accuracy = 0.1)
    avg_pct_D_formatted <- scales::percent(data_district$avg_pct_D, accuracy = 0.1)
    # Get ordinal suffix (e.g., "1st", "2nd", "3rd", etc.)
    ordinal_rank <- scales::ordinal(district_rank_formatted)
    
    # Get the proper phrase: "Democratic" or "Republican"
    lean_phrase <- ifelse(district_lean_party_abbr == "R", "Republican", "Democratic")
    
    # The chamber (e.g., "Senate" or "House")
    chamber <- data_district$chamber
    
    HTML(paste0(
      '<div class="flex-item population-voting">',
      '<h3 class="flex-header-section">DISTRICT POLITICAL LEAN</h3>',
      '<ul class="main-list">',
      '<li>District Partisanship: This is the <span class="stat-bold">',
      ordinal_rank, ' most ', lean_phrase,
      '</span> district out of all <span class="stat-bold">',
      n_districts, '</span> ', chamber, ' districts.</li>',
      '<li>Partisan lean: <span class="stat-bold">', lean_display_string, '</span></li>',
      '<li>', lean_description, '</li>',
      '<li>Recent Election Results:</li>',
      get_election_results_table(data_district),
      '</ul>',
      '</div>'
    ))
  })
  
  #-------------------------------
  # 6. Helper: Demographics Plot
  #-------------------------------
  output$helper3_demographics <- renderUI({
    tagList(
      div(class = "flex-item district-demographics",
          h3(class = "flex-header-section", "DISTRICT DEMOGRAPHICS"),
          p(style="margin-bottom:10px;",
            "Demographic statistics reflect voting-age citizen population (CVAP) from the American Community Survey, 5-year sample ending in 2022. CVAP better reflects eligible voters than total population. Values are rounded to one decimal."),
          plotOutput("demographicsPlot", width = "100%", height = "auto")
      )
    )
  })
  
  output$demographicsPlot <- renderPlot({
    demo_district <- qry_demo_district()
    demo_state <- app03_district_context_state
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
                  hjust = -.05,
                  size = 5,
                  family = "Archivo") +
        scale_fill_manual(values = c(setNames(c("#098677", "#cccccc"), c(district_name, "Florida")))) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.075)) +
        labs(title = demo, x = "", y = "") +
        theme_minimal(base_size = 14, base_family = "Archivo") +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = "#064875"),
          axis.text.y = element_text(size = 14),
          plot.margin = margin(0, 0, 0, 0)#,
          #panel.background = element_rect(fill = "#f9f9f9", colour = NA),
          #plot.background = element_rect(fill = "#ffffff", colour = NA)
        ) +
        coord_flip()
    }
    
    plots <- lapply(c("White", "Black", "Asian", "Hispanic"), create_plot)
    do.call(gridExtra::grid.arrange,
            c(plots, list(ncol=1, heights=rep(1, length(plots)), padding=grid::unit(0, "line"))))
    
    
  },
  width = 400, height = 600)
  
  #-------------------------------
  # 7. Combined Output Section
  #-------------------------------
  output$dynamicContextComparison <- renderUI({
    tagList(
      HTML('<div class="flex-section">'),
      uiOutput("helper3_party_loyalty"),
      uiOutput("helper3_district_lean"),
      uiOutput("helper3_demographics"),
      HTML('</div>')
    )
  })
  
  #-------------------------------
  # 8. Ballotpedia and Methodology Footer
  #-------------------------------
  selected_legislator <- reactive({
    data <- qry_demo_district()
    print(paste("Selected legislator:", data$legislator_name))
    data$legislator_name
  })
  votingHistoryServer("votingHistory", selected_legislator)
  
  output$staticMethodology3 <- renderUI({
    year_text <- methodology_year_string()
    HTML(paste0(
      '<hr>',
      '<div class="header-section"><h3>Methodology</h3></div>',
      '<div class="methodology-notes">',
      '<p>*Other votes include those marked absent or "no vote", voting with party when party is equally divided, and voting against party when oppo is equally divided.</p>',
      '<p><strong>Legislator Party Loyalty:</strong> Calculated using all votes from ', year_text, ' legislative sessions where parties disagreed.</p>',
      '<p>Scores range from 0 to 1, where 1 indicates always voting with party majority and 0 indicates always voting against.</p>',
      '<p><strong>District Partisan Lean:</strong> Based on a weighted average of recent election results:</p>',
      '<ul>',
      '<li>2024 Presidential Election (50% weight)</li>',
      '<li>2022 Gubernatorial Election (25% weight)</li>',
      '<li>2020 Presidential Election (25% weight)</li>',
      '</ul>',
      '<strong>Data sources:</strong>',
      '<ul>',
      '<li>Legislator voting info from <a href="https://legiscan.com/FL/datasets">LegiScan\'s Florida Legislative Datasets for ', year_text, ' Regular Session</a>.</li>',
      '<li>District demographics and election results curated by <a href="https://davesredistricting.org/maps#state::FL">Dave\'s Redistricting</a>.</li>',
      '</ul>',
      '<br></div>'
    ))
  })
  
}) # END OBSERVER EVENT