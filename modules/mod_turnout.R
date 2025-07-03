turnout_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "turnout",
    fluidRow(
      box(title = "Controls", width = 3,
          selectInput(ns("turnout_year"), "Select Year:",
                      choices = unique(election_data$year)),
          selectInput(ns("turnout_region"), "Select Region:",
                      choices = c("All", unique(election_data$rid)))
      ),
      box(title = "Voter Turnout by Region", width = 9,
          plotlyOutput(ns("turnout_region_plot")))
    ),
    fluidRow(
      box(title = "Voter Turnout by Province (Top 20)", width = 6,
          plotlyOutput(ns("turnout_province_plot"))),
      box(title = "Turnout vs Dynasty Presence", width = 6,
          plotlyOutput(ns("turnout_dynasty_plot")))
    ),
    fluidRow(
      box(title = "Turnout Analysis Results", width = 12,
          verbatimTextOutput(ns("turnout_summary")))
    )
  )
}

turnout_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Process turnout data from election_dataWorksheet.csv
    processed_turnout_data <- reactive({
      election_data %>%
        filter(year == input$turnout_year) %>%
        mutate(
          last_name = str_trim(str_extract(candidate, "^[^, ]+")),
          is_dynasty = ifelse(duplicated(last_name) | duplicated(last_name, fromLast = TRUE),
                              "Dynastic", "Non-Dynastic")
        ) %>%
        filter(if (input$turnout_region != "All") rid == input$turnout_region else TRUE)
    })
    
    # Voter turnout by region
    output$turnout_region_plot <- renderPlotly({
      turnout_by_region <- processed_turnout_data() %>%
        group_by(rid) %>%
        summarise(
          total_votes = sum(votes, na.rm = TRUE),
          registered_voters = sum(total, na.rm = TRUE),
          turnout = ifelse(registered_voters > 0, total_votes / registered_voters, NA),
          .groups = "drop"
        ) %>%
        filter(!is.na(turnout))
      
      validate(need(nrow(turnout_by_region) > 0, "No turnout data available for selected region/year"))
      
      p <- ggplot(turnout_by_region, aes(x = reorder(rid, -turnout), y = turnout)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        scale_y_continuous(labels = scales::percent) +
        labs(title = paste("Voter Turnout by Region (", input$turnout_year, ")"),
             x = "Region", y = "Turnout Percentage") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Voter turnout by province (top 20)
    output$turnout_province_plot <- renderPlotly({
      turnout_by_province <- processed_turnout_data() %>%
        group_by(city) %>%
        summarise(
          total_votes = sum(votes, na.rm = TRUE),
          registered_voters = sum(total, na.rm = TRUE),
          turnout = ifelse(registered_voters > 0, total_votes / registered_voters, NA),
          .groups = "drop"
        ) %>%
        filter(!is.na(turnout)) %>%
        arrange(desc(turnout)) %>%
        head(20)
      
      validate(need(nrow(turnout_by_province) > 0, "No turnout data available for selected region/year"))
      
      p <- ggplot(turnout_by_province, aes(x = reorder(city, -turnout), y = turnout)) +
        geom_bar(stat = "identity", fill = "darkgreen") +
        scale_y_continuous(labels = scales::percent) +
        labs(title = paste("Top 20 Provinces by Turnout (", input$turnout_year, ")"),
             x = "Province", y = "Turnout Percentage") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Turnout vs dynasty presence - FIXED GROUPING FACTOR ISSUE
    output$turnout_dynasty_plot <- renderPlotly({
      turnout_dynasty <- processed_turnout_data() %>%
        group_by(city) %>%
        summarise(
          total_votes = sum(votes, na.rm = TRUE),
          registered_voters = sum(total, na.rm = TRUE),
          turnout = ifelse(registered_voters > 0, total_votes / registered_voters, NA),
          dynasty_votes = sum(votes[is_dynasty == "Dynastic"], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          dynasty_share = ifelse(total_votes > 0, dynasty_votes / total_votes, NA),
          dynasty_presence = case_when(
            dynasty_share >= 0.5 ~ "High",
            dynasty_share <= 0.2 ~ "Low",
            TRUE ~ "Medium"
          )
        ) %>%
        filter(!is.na(turnout), !is.na(dynasty_presence))
      
      # Ensure we only have High and Low for the plot (remove Medium)
      plot_data <- turnout_dynasty %>%
        filter(dynasty_presence %in% c("High", "Low"))
      
      validate(
        need(nrow(plot_data) > 0, "No turnout data available for dynasty comparison"),
        need(length(unique(plot_data$dynasty_presence)) == 2, 
             "Need both High and Low dynasty presence for comparison")
      )
      
      p <- ggplot(plot_data, aes(x = dynasty_presence, y = turnout, fill = dynasty_presence)) +
        geom_boxplot() +
        scale_y_continuous(labels = scales::percent) +
        labs(title = paste("Voter Turnout by Dynasty Presence (", input$turnout_year, ")"),
             x = "Dynastic Presence", y = "Turnout Percentage")
      
      ggplotly(p)
    })
    
    # Statistical summary - FIXED GROUPING FACTOR ISSUE
    output$turnout_summary <- renderPrint({
      turnout_dynasty <- processed_turnout_data() %>%
        group_by(city) %>%
        summarise(
          total_votes = sum(votes, na.rm = TRUE),
          registered_voters = sum(total, na.rm = TRUE),
          turnout = ifelse(registered_voters > 0, total_votes / registered_voters, NA),
          dynasty_votes = sum(votes[is_dynasty == "Dynastic"], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          dynasty_share = ifelse(total_votes > 0, dynasty_votes / total_votes, NA),
          dynasty_presence = ifelse(dynasty_share >= 0.5, "High", "Low")
        ) %>%
        filter(!is.na(turnout), !is.na(dynasty_presence))
      
      # Filter to only High and Low for statistical tests
      test_data <- turnout_dynasty %>%
        filter(dynasty_presence %in% c("High", "Low"))
      
      cat("=== Turnout Comparison by Dynasty Presence ===\n\n")
      
      if (length(unique(test_data$dynasty_presence)) == 2) {
        cat("Summary Statistics:\n")
        print(
          test_data %>%
            group_by(dynasty_presence) %>%
            summarise(
              avg_turnout = mean(turnout, na.rm = TRUE),
              sd_turnout = sd(turnout, na.rm = TRUE),
              n = n()
            )
        )
        
        cat("\nT-test Results:\n")
        t_test_result <- t.test(turnout ~ dynasty_presence, data = test_data)
        print(t_test_result)
        
        cat("\nLinear Model Results:\n")
        lm_model <- lm(turnout ~ dynasty_presence, data = test_data)
        print(summary(lm_model))
      } else {
        cat("\nCannot perform statistical tests - need both High and Low dynasty presence areas.\n")
        cat("Available groups:", paste(unique(test_data$dynasty_presence), collapse = ", "), "\n")
      }
      
      # Show full distribution summary
      cat("\n\n=== Full Turnout Distribution ===")
      print(summary(turnout_dynasty$turnout))
    })
  })
}