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
    legislators <- unique(app03_district_context$legislator_name)
    
    div(class = "filter-row query-input",
        radioButtons("filter_method", "Filter By:", choices = c("District", "Legislator Name"), selected = "District"),
        conditionalPanel(
          condition = "input.filter_method == 'District'",
          createFilterBox("chamber3", "Select Chamber:", c("House", "Senate"), selected = "House"),
          createFilterBox("district3", "Select District:", 1:120, selected = 1)
        ),
        conditionalPanel(
          condition = "input.filter_method == 'Legislator Name'",
          selectInput("legislator3", "Select Legislator:", choices = legislators, selected = legislators[1])
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
  
  ########################################
  #                                      #  
  # comparative partisanship             #
  #                                      #
  ######################################## 
  
  output$dynamicPartisanship <- renderUI({
    data_district <- qry_demo_district()
    # data_district <- data_district$data
    
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
      count_legislators_in_party(app03_district_context, "R", input$chamber3)$n
    } else if (same_party == "Democrat") {
      count_legislators_in_party(app03_district_context, "D", input$chamber3)$n
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
        '<table style="width:100%; border-collapse: collapse;">',
        '<tr>',
        '<th style="width:50%; text-align:left; border-right: 1px solid black; padding-right: 10px;"><span class="header-section">Legislative Voting</span></th>',
        '<th style="width:50%; text-align:left; padding-left: 10px;"><span class="header-section">Population Voting</span></th>',
        '</tr>',
        '<tr>',
        '<td style="vertical-align:top; width:50%; border-right: 1px solid black; padding-right: 10px;" align=left>',
        '<span class="stat-bold">', data_district$legislator_name, '\'s</span> voting record is ranked #<span class="stat-bold">', rank_leg,
        '</span> most ',same_party , '-leaning amongst <span class="stat-bold">', n_legislators_in_party, '</span> ', input$chamber3, ' legislators in the ', same_party, ' party.<br>',
        '<br>Of their ', data_district$leg_n_votes_denom_loyalty, ' votes included in calculating party loyalty:<br>',
        'Party Line Votes: <span class="stat-bold">', data_district$leg_n_votes_party_line, ' (', percent(data_district$leg_n_votes_party_line/data_district$leg_n_votes_denom_loyalty,accuracy = 0.1), ')</span><br>',
        'Cross Party Votes: <span class="stat-bold">', data_district$leg_n_votes_cross_party, ' (', percent(data_district$leg_n_votes_cross_party/data_district$leg_n_votes_denom_loyalty, accuracy = 0.1), ')</span><br>',
        '<br>Not included in the loyalty calculation:<br>',
        'Independent Votes: <span class="stat-bold">', data_district$leg_n_votes_independent, '</span><br>',
        '*Other Votes: <span class="stat-bold">', data_district$leg_n_votes_other, '</span><br>',
        '<a href = "', data_district$ballotpedia ,'" target="_blank">', data_district$legislator_name, ' on Ballotpedia</a>',
        '</td>',
        '<td style="vertical-align:top; width:50%; padding-left: 10px;"  align=left>',
        'In comparison, this district is ranked #<span class="stat-bold">', rank_dist, '</span> most ', same_party,
        '-leaning of <span class="stat-bold">', n_districts, '</span> ', data_district$chamber, ' districts:<br>',
        '<span class="stat-bold">', data_district$party_lean, ' + ', data_district$party_lean_points_abs, '</span><br>',
        '<span class="stat-bold">', percent(data_district$pct_R), '</span> Republican<br>',
        '<span class="stat-bold">', percent(data_district$pct_D), '</span> Democrat<br>',
        '</td>',
        '</tr>',
        '</table>',
        '<hr>'
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
    req(qry_demo_district)
    demo_district <- qry_demo_district()
    demo_state <- app03_district_context_state
    
    # need to have 5x2 for each of category, percent, and demographic
    data <- data.frame(
      Category = factor(rep(c("District", "State"), 5), levels = c("State", "District")),  # Reverse factor levels
      Percent = c(
        demo_district$pct_white, demo_state$pct_white,
        demo_district$pct_black, demo_state$pct_black,
        demo_district$pct_asian, demo_state$pct_asian,
        demo_district$pct_hispanic, demo_state$pct_hispanic,
        demo_district$pct_napi, demo_state$pct_napi
      ),
      Demographic = rep(c("White", "Black", "Asian", "Hispanic", "Native American/Pacific Islander"), each = 2)
    )
    
    create_plot <- function(demo) {
      ggplot(subset(data, Demographic == demo), aes(x = Category, y = Percent, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge", width=1) +
        geom_text(aes(label = scales::percent(Percent,accuracy=0.1)), 
                  position = position_dodge(width = 1), 
                  vjust = 0.5,
                  hjust = -0.1,
                  size = 7) +
        scale_fill_manual(values = c("District" = "#17becf", "State" = "#dfdfdf")) +
        scale_y_continuous(labels = percent_format(),, limits = c(0, 1)) +
        labs(title = demo, x = "", y = "") +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12, hjust = 1, margin = margin(r = -5))
        ) +
        coord_flip()
    }
    
    # Create plots for each demographic
    plot_white <- create_plot("White")
    plot_black <- create_plot("Black")
    plot_asian <- create_plot("Asian")
    plot_hispanic <- create_plot("Hispanic")
    plot_napi <- create_plot("Native American/Pacific Islander")
    
    # Combine plots using patchwork
    plot_white / plot_black/ plot_asian/ plot_hispanic/ plot_napi
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