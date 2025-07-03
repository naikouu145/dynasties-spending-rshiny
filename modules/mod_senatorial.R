senatorial_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "sen",
    fluidRow(
      box(title = "Controls", width = 3,
          selectInput(ns("sen_year"), "Election Year:",
                      choices = c("2022", "2025")),
          selectInput(ns("sen_region"), "Select Region:",
                      choices = c("All", unique(sen_votes$region)))
      ),
      box(title = "Top Senatorial Candidates", width = 9,
          plotlyOutput(ns("sen_votes_plot")))
    ),
    fluidRow(
      box(title = "Regions Won by Top Candidates", width = 6,
          plotlyOutput(ns("sen_regions_won"))),
      box(title = "Provinces Won by Top Candidates", width = 6,
          plotlyOutput(ns("sen_provinces_won")))
    ),
    fluidRow(
      box(title = "Senatorial Election Data", width = 12,
          DTOutput(ns("sen_data_table")))
    )
  )
}

senatorial_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    sen_candidates_2022 <- colnames(sen_votes)[grepl("^sen_", colnames(sen_votes))]
    sen_candidates_2025 <- colnames(sen_votes_2025)[grepl("^X\\d+\\.\\.", colnames(sen_votes_2025))]
    
    current_data <- reactive({
      if (input$sen_year == "2022") {
        list(data = sen_votes, candidates = sen_candidates_2022, type = "sen")
      } else {
        list(data = sen_votes_2025, candidates = sen_candidates_2025, type = "sen25")
      }
    })
    
    filtered_data <- reactive({
      dat <- current_data()$data
      if (input$sen_region != "All") {
        dat[dat$region == input$sen_region, ]
      } else {
        dat
      }
    })
    
    output$sen_votes_plot <- renderPlotly({
      dat <- current_data()
      data <- filtered_data()
      top_candidates <- get_top_candidates(data, dat$candidates, n = 20)
      top_candidates$Candidate <- clean_candidate_names(top_candidates$Candidate, dat$type)
      
      create_candidate_plot(top_candidates, paste("Top 20 Senatorial Candidates", input$sen_year), "steelblue")
    })
    
    output$sen_regions_won <- renderPlotly({
      dat <- current_data()
      regions_won <- get_regions_won(dat$data, dat$candidates)
      regions_won$Var1 <- clean_candidate_names(regions_won$Var1, dat$type)
      
      create_winners_plot(regions_won[1:10, ], "Regions Won by Top 10 Candidates", "darkred")
    })
    
    output$sen_provinces_won <- renderPlotly({
      dat <- current_data()
      provinces_won <- get_provinces_won(dat$data, dat$candidates)
      provinces_won$Var1 <- clean_candidate_names(provinces_won$Var1, dat$type)
      
      create_winners_plot(provinces_won[1:10, ], "Provinces Won by Top 10 Candidates", "darkgreen")
    })
    
    output$sen_data_table <- renderDT({
      datatable(filtered_data(), options = list(scrollX = TRUE))
    })
  })
}