dynasties_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "dynasties",
    fluidRow(
      box(title = "Controls", width = 3,
          selectInput(ns("dynasty_year"), "Select Year:",
                      choices = c("All", unique(election_data$year))),
          selectInput(ns("dynasty_region"), "Select Region:",
                      choices = c("All", unique(election_data$rid)))
      ),
      box(title = "Dynastic vs Non-Dynastic Candidates by Region", width = 9,
          plotlyOutput(ns("dynasty_region_plot")))
    ),
    fluidRow(
      box(title = "Win Rate by Dynasty Status", width = 6,
          plotlyOutput(ns("dynasty_winrate_plot"))),
      box(title = "Dynasty Presence Metrics Over Time", width = 6,
          plotlyOutput(ns("dynasty_trend_plot")))
    ),
    fluidRow(
      box(title = "Dynasty Analysis Results", width = 12,
          verbatimTextOutput(ns("dynasty_summary")))
    )
  )
}

dynasties_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Process dynasty data (from your original code)
    processed_dynasty_data <- reactive({
      # Clean and prepare the data
      election_data %>%
        mutate(
          last_name = str_trim(str_extract(candidate, "^[^, ]+"))
        ) %>%
        group_by(last_name) %>%
        mutate(
          appearances = n_distinct(paste(year, position, city)),
          is_dynasty = ifelse(appearances > 1, "Dynastic", "Non-Dynastic")
        ) %>%
        ungroup() %>%
        mutate(region = rid) %>%
        filter(if (input$dynasty_year != "All") year == input$dynasty_year else TRUE,
               if (input$dynasty_region != "All") region == input$dynasty_region else TRUE)
    })
    
    # Dynasty distribution by region
    output$dynasty_region_plot <- renderPlotly({
      dynasty_dist <- processed_dynasty_data() %>%
        group_by(region, is_dynasty) %>%
        summarise(num_candidates = n_distinct(candidate), .groups = "drop")
      
      p <- ggplot(dynasty_dist, aes(x = region, y = num_candidates, fill = is_dynasty)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(title = "Distribution of Dynastic vs Non-Dynastic Candidates by Region",
             x = "Region", y = "Number of Candidates", fill = "Candidate Type") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Win rate by dynasty status
    output$dynasty_winrate_plot <- renderPlotly({
      win_rates <- processed_dynasty_data() %>%
        group_by(is_dynasty) %>%
        summarise(
          win_rate = mean(won, na.rm = TRUE),
          count = n()
        )
      
      p <- ggplot(win_rates, aes(x = is_dynasty, y = win_rate, fill = is_dynasty)) +
        geom_col() +
        labs(title = "Win Rate by Dynasty Status",
             y = "Proportion of Wins", x = "Candidate Type") +
        scale_y_continuous(labels = scales::percent)
      
      ggplotly(p)
    })
    
    # Dynasty trends over time
    output$dynasty_trend_plot <- renderPlotly({
      # From your Problem Question 5 analysis
      fiscal_elections <- fiscal_data %>%
        filter(!is.na(elecyr)) %>%
        select(year = elecyr, lgu, region, pi, sumpi2, ENC_lt, ENC_gol) %>%
        filter(!is.na(pi) & !is.na(ENC_lt))
      
      concentration_trends <- fiscal_elections %>%
        group_by(year) %>%
        summarise(
          avg_pi = mean(pi, na.rm = TRUE),
          avg_sumpi2 = mean(sumpi2, na.rm = TRUE),
          avg_ENC_lt = mean(ENC_lt, na.rm = TRUE),
          avg_ENC_gol = mean(ENC_gol, na.rm = TRUE)
        )
      
      long_conc <- pivot_longer(concentration_trends, cols = -year, 
                                names_to = "metric", values_to = "value")
      
      p <- ggplot(long_conc, aes(x = year, y = value, color = metric)) +
        geom_line() +
        geom_point() +
        labs(title = "Evolution of Dynasty Dominance (1992-2022)",
             y = "Concentration Index Value", x = "Election Year") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # Dynasty analysis summary
    output$dynasty_summary <- renderPrint({
      # From your Problem Question 2 analysis
      cat("=== Dynasty Win Rate Analysis ===\n")
      election_data_model <- processed_dynasty_data() %>%
        filter(!is.na(won)) %>%
        mutate(
          is_dynasty_binary = ifelse(is_dynasty == "Dynastic", 1, 0)
        )
      
      logit_model <- glm(won ~ is_dynasty_binary, 
                         data = election_data_model, 
                         family = binomial)
      
      cat("\nLogistic Regression Results:\n")
      print(summary(logit_model))
      
      odds_ratio <- exp(coef(logit_model))
      confint_vals <- exp(confint(logit_model))
      
      cat("\nOdds Ratios:\n")
      print(data.frame(
        Term = names(odds_ratio),
        OddsRatio = odds_ratio,
        CI_Lower = confint_vals[, 1],
        CI_Upper = confint_vals[, 2]
      ))
      
      # From your Problem Question 4 analysis
      cat("\n\n=== Fiscal Impact Analysis ===\n")
      dynasty_agg <- processed_dynasty_data() %>%
        group_by(rid, city, year) %>%
        summarise(
          dynastic_winners = sum(won * (is_dynasty == "Dynastic")),
          total_winners = sum(won),
          dynasty_rate = dynastic_winners / total_winners,
          .groups = "drop"
        ) %>%
        mutate(dynasty_group = ifelse(dynasty_rate >= 0.5, "High", "Low"))
      
      fiscal_data_clean <- fiscal_data %>%
        mutate(city = toupper(lgu)) %>%
        inner_join(dynasty_agg, by = c("region" = "rid", "city", "year"))
      
      dev_data <- fiscal_data_clean %>%
        select(region, city, year, dynasty_group,
               loggovexp, logsocserv, logaveira, logavegenpub) %>%
        mutate(dynasty_group = as.factor(dynasty_group)) %>%
        drop_na()
      
      cat("\nGovernment Expenditure by Dynasty Presence:\n")
      print(summary(lm(loggovexp ~ dynasty_group, data = dev_data)))
      
      cat("\nSocial Services by Dynasty Presence:\n")
      print(summary(lm(logsocserv ~ dynasty_group, data = dev_data)))
    })
  })
}

