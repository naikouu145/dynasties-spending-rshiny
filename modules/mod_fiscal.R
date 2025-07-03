fiscal_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "fiscal",
    fluidRow(
      box(title = "Controls", width = 3,
          selectInput(ns("fiscal_year"), "Select Year:",
                      choices = unique(fiscal_data$year)),
          selectInput(ns("fiscal_lgutype"), "Select LGU Type:",
                      choices = c("All", unique(na.omit(fiscal_data$lgutype))))
      ),
      box(title = "Fiscal Indicators by LGU Type", width = 9,
          plotlyOutput(ns("fiscal_lgutype_plot")))
    ),
    fluidRow(
      box(title = "Fiscal Indicators Distribution", width = 6,
          plotlyOutput(ns("fiscal_dist_plot"))),
      box(title = "Fiscal Indicators Correlation", width = 6,
          plotOutput(ns("fiscal_corr_plot")))
    ),
    fluidRow(
      box(title = "Fiscal Indicators vs Dynasty Presence", width = 12,
          plotlyOutput(ns("fiscal_dynasty_plot")))
    ),
    fluidRow(
      box(title = "Fiscal Data Analysis Results", width = 12,
          verbatimTextOutput(ns("fiscal_summary")))
    )
  )
}

fiscal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Process fiscal data - ensure numeric columns are properly converted
    processed_fiscal_data <- reactive({
      fd <- fiscal_data %>%
        filter(year == input$fiscal_year) %>%
        mutate(across(c(ira, govexp, lgusincome, tottax), as.numeric))  # Ensure numeric
      
      if (input$fiscal_lgutype != "All") {
        fd <- fd %>% filter(lgutype == input$fiscal_lgutype)
      }
      
      fd
    })
    
    # Fiscal indicators by LGU type
    output$fiscal_lgutype_plot <- renderPlotly({
      validate(
        need(nrow(processed_fiscal_data()) > 0, "No data available for selected filters")
      )
      
      p <- ggplot(processed_fiscal_data(), aes(x = lgutype)) +
        geom_bar(fill = "steelblue") +
        labs(title = paste("Distribution of LGU Types (", input$fiscal_year, ")"),
             x = "LGU Type", y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Fiscal indicators distribution - with proper numeric handling
    output$fiscal_dist_plot <- renderPlotly({
      fd <- processed_fiscal_data()
      
      validate(
        need(nrow(fd) > 0, "No data available for selected filters"),
        need(all(c("ira", "govexp", "lgusincome", "tottax") %in% names(fd)), 
             "Required columns not found")
      )
      
      # Create safe log plots with NA handling
      safe_log <- function(x) {
        ifelse(x > 0, log10(x), NA)
      }
      
      p1 <- ggplot(fd, aes(x = safe_log(ira))) +
        geom_histogram(bins = 50, fill = "steelblue", color = "white", na.rm = TRUE) +
        labs(title = "IRA Distribution", x = "IRA (log10)", y = "Count")
      
      p2 <- ggplot(fd, aes(x = safe_log(govexp))) +
        geom_histogram(bins = 50, fill = "darkgreen", color = "white", na.rm = TRUE) +
        labs(title = "Government Expenditure", x = "GovExp (log10)", y = "Count")
      
      p3 <- ggplot(fd, aes(x = safe_log(lgusincome))) +
        geom_histogram(bins = 50, fill = "orange", color = "white", na.rm = TRUE) +
        labs(title = "LGU Income", x = "Income (log10)", y = "Count")
      
      p4 <- ggplot(fd, aes(x = safe_log(tottax))) +
        geom_histogram(bins = 50, fill = "purple", color = "white", na.rm = TRUE) +
        labs(title = "Total Tax", x = "Tax (log10)", y = "Count")
      
      subplot(ggplotly(p1), ggplotly(p2), ggplotly(p3), ggplotly(p4),
              nrows = 2, margin = 0.05)
    })
    
    # Fiscal indicators correlation - with proper numeric handling
    output$fiscal_corr_plot <- renderPlot({
      fd <- processed_fiscal_data()
      
      validate(
        need(nrow(fd) > 0, "No data available for selected filters"),
        need(all(c("ira", "govexp", "lgusincome", "tottax") %in% names(fd)), 
             "Required columns not found")
      )
      
      numeric_data <- fd %>%
        select(ira, govexp, lgusincome, tottax) %>%
        mutate(across(everything(), as.numeric)) %>%
        na.omit()
      
      validate(
        need(nrow(numeric_data) > 1, "Insufficient data for correlation")
      )
      
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      corrplot(cor_matrix, method = "color", tl.cex = 0.8)
    })
    
    # Fiscal indicators vs dynasty presence - with proper numeric handling
    output$fiscal_dynasty_plot <- renderPlotly({
      # First ensure we have numeric fiscal data
      fd <- processed_fiscal_data() %>%
        mutate(across(c(ira, govexp, lgusincome, tottax), as.numeric))
      
      # Process dynasty data
      dynasty_data <- election_data %>%
        mutate(last_name = str_trim(str_extract(candidate, "^[^, ]+"))) %>%
        group_by(last_name) %>%
        mutate(appearances = n_distinct(paste(year, city, position))) %>%
        ungroup() %>%
        mutate(is_dynasty = ifelse(appearances > 1, "Dynastic", "Non-Dynastic")) %>%
        group_by(city, year) %>%
        summarise(
          dynasty_rate = mean(is_dynasty == "Dynastic", na.rm = TRUE),
          .groups = "drop"
        )
      
      # Combine data with proper joining
      combined_data <- fd %>%
        mutate(city = toupper(lgu)) %>%
        left_join(dynasty_data, by = c("city", "year")) %>%
        filter(!is.na(dynasty_rate), !is.na(govexp))
      
      validate(
        need(nrow(combined_data) > 1, "Insufficient data for plot")
      )
      
      p <- ggplot(combined_data, aes(x = dynasty_rate, y = log10(govexp))) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm") +
        labs(title = "Government Expenditure vs Dynasty Presence",
             x = "Dynasty Presence Rate", y = "Log Government Expenditure")
      
      ggplotly(p)
    })
    
    # Fiscal analysis summary - with proper numeric handling
    output$fiscal_summary <- renderPrint({
      fd <- processed_fiscal_data()
      
      # First print basic summary
      cat("=== Fiscal Data Summary ===\n")
      print(summary(fd %>% select(ira, govexp, lgusincome, tottax)))
      
      # Then print dynasty analysis if possible
      tryCatch({
        dynasty_agg <- election_data %>%
          mutate(last_name = str_trim(str_extract(candidate, "^[^, ]+"))) %>%
          group_by(last_name) %>%
          mutate(appearances = n_distinct(paste(year, city, position))) %>%
          ungroup() %>%
          mutate(is_dynasty = ifelse(appearances > 1, "Dynastic", "Non-Dynastic")) %>%
          group_by(rid, city, year) %>%
          summarise(
            dynastic_winners = sum(won * (is_dynasty == "Dynastic"), na.rm = TRUE),
            total_winners = sum(won, na.rm = TRUE),
            dynasty_rate = ifelse(total_winners > 0, 
                                  dynastic_winners / total_winners, 
                                  NA),
            .groups = "drop"
          ) %>%
          mutate(dynasty_group = ifelse(dynasty_rate >= 0.5, "High", "Low"))
        
        fiscal_dynasty <- fd %>%
          mutate(city = toupper(lgu), region = as.character(region)) %>%
          inner_join(dynasty_agg, by = c("region" = "rid", "city", "year")) %>%
          select(region, city, year, dynasty_group,
                 loggovexp, logsocserv, logaveira, logavegenpub) %>%
          mutate(dynasty_group = as.factor(dynasty_group)) %>%
          drop_na()
        
        if (nrow(fiscal_dynasty) > 1) {
          cat("\n\n=== Fiscal Indicators by Dynasty Presence ===\n")
          cat("\nGovernment Expenditure:\n")
          print(summary(lm(loggovexp ~ dynasty_group, data = fiscal_dynasty)))
          
          cat("\nSocial Services:\n")
          print(summary(lm(logsocserv ~ dynasty_group, data = fiscal_dynasty)))
        }
      }, error = function(e) {
        cat("\n\nCould not complete dynasty analysis:", conditionMessage(e))
      })
    })
  })
}