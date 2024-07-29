# server4_partisanship_scatterplot.R


observeEvent(input$navbar_page == "app4", {
  
  filtered_data <- reactive({
    data <- app03_district_context
    if (input$chamber4 != "All") {
      data <- data %>% filter(chamber == input$chamber4)
    }
    if (input$party4 != "All") {
      data <- data %>% filter(party == input$party4)
    }
    data
  })
  
  median_data <- reactive({
    data <- filtered_data()
    median_lean <- median(data$avg_party_lean_points_R)
    median_districts <- data %>%
      arrange(abs(avg_party_lean_points_R - median_lean)) %>%
      slice(1:2)
    list(
      median_lean = median_lean,
      districts = median_districts
    )
  })
  
  output$scatterplot <- renderPlotly({
    data <- filtered_data()
    median_info <- median_data()
    
    p <- ggplot(data, aes(x = avg_party_lean_points_R, y = leg_party_loyalty, 
                          color = party, 
                          text = paste(legislator_name, 
                                       "<br>District: ", district_number,
                                       "<br>District Lean: ", round(avg_party_lean_points_R, 2),
                                       "<br>Party Loyalty: ", round(leg_party_loyalty, 2)))) +
      geom_point() +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
      geom_vline(xintercept = median_info$median_lean, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      scale_color_manual(values = c("D" = "#4575b4", "R" = "#d73027")) +
      labs(x = "District Partisan Lean (R+)", 
           y = "Legislator Party Loyalty",
           color = "Party") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        plot.background = element_rect(fill = "#f0f0f0", color = NA)
      ) +
      annotate("text", x = median_info$median_lean, y = 0.5, label = "Median", angle = 90, vjust = -0.5)
    
    ggplotly(p, tooltip = "text") %>%
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  
  output$medianInfo <- renderUI({
    median_info <- median_data()
    districts <- median_info$districts
    
    HTML(paste0(
      '<div class="header-section">Median Districts</div>',
      '<p>The median district lean is <span class="stat-bold">', round(median_info$median_lean, 2), ' (R+)</span>. The median districts are:<br>',
      '<strong>', districts$legislator_name[1], '</strong> (', districts$party[1], ') - ',districts$chamber[1],' District ', districts$district_number[1], 
      ' - Lean: <span class="stat-bold">', round(districts$avg_party_lean_points_R[1], 2), '</span>, Loyalty: <span class="stat-bold">', round(districts$leg_party_loyalty[1], 2), '</span><br>',
      '<strong>', districts$legislator_name[2], '</strong> (', districts$party[2], ') - ',districts$chamber[2],' District ', districts$district_number[2], 
      ' - Lean: <span class="stat-bold">', round(districts$avg_party_lean_points_R[2], 2), '</span>, Loyalty: <span class="stat-bold">', round(districts$leg_party_loyalty[2], 2), '</span></p>'
    ))
  })
  
  output$explanationText <- renderUI({
    HTML('
    <div class="header-section">Methodology</div>
    <div class="methodology-notes">
      <p><strong>Party Loyalty:</strong> Calculated as the proportion of a legislator\'s votes that align with their party\'s majority on bills where a majority of Democrats and a majority of Republicans vote differently from one another. This excludes unanimous and bipartisan votes. A score of 1 indicates perfect alignment with the party, while 0 indicates consistent voting against the party.</p>
      <p><strong>Bills Analyzed:</strong> This analysis includes only bills where a majority of Democrats and a majority of Republicans voted differently, highlighting partisan disagreements.</p>
      <p><strong>District Partisan Lean:</strong> Calculated from the weighted average of the following election results:</p>
      <ul>
        <li>2016 Presidential Election (10% weight)</li>
        <li>2018 Gubernatorial Election (10% weight)</li>
        <li>2020 Presidential Election (50% weight)</li>
        <li>2022 Gubernatorial Election (30% weight)</li>
      </ul>
      <p>Positive values indicate a Republican lean, negative values a Democratic lean.</p>
      <p><strong>Median District:</strong> The district with the median partisan lean value, representing the "middle" of the political spectrum for the selected districts.</p>
    </div>
    ')
  })
})