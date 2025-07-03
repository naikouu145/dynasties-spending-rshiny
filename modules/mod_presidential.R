presidential_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "pres",
    fluidRow(
      box(title = "Controls", width = 3,
          selectInput(ns("pres_region"), "Select Region:", 
                      choices = c("All", unique(votes$region)))
      ),
      box(title = "Presidential Candidates Vote Share", width = 9,
          plotlyOutput(ns("pres_votes_plot")))
    ),
    fluidRow(
      box(title = "Regions Won by Each Candidate", width = 6,
          plotlyOutput(ns("pres_regions_won"))),
      box(title = "Provinces Won by Each Candidate", width = 6,
          plotlyOutput(ns("pres_provinces_won")))
    ),
    fluidRow(
      box(title = "Presidential Election Data", width = 12,
          DTOutput(ns("pres_data_table")))
    )
  )
}

presidential_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pres_candidates <- c("pres_1_abella", "pres_2_de_guzman", "pres_3_domagoso",
                         "pres_4_gonzales", "pres_5_lacson", "pres_6_mangondato",
                         "pres_7_marcos", "pres_8_montemayor", "pres_9_pacquiao",
                         "pres_10_robredo")
    
    filtered_data <- reactive({
      if (input$pres_region != "All") {
        votes[votes$region == input$pres_region, ]
      } else {
        votes
      }
    })
    
    output$pres_votes_plot <- renderPlotly({
      data <- filtered_data()
      top_candidates <- get_top_candidates(data, pres_candidates, n = 10)
      top_candidates$Candidate <- clean_candidate_names(top_candidates$Candidate, "pres")
      
      create_candidate_plot(top_candidates, "Presidential Candidates", "steelblue")
    })
    
    output$pres_regions_won <- renderPlotly({
      regions_won <- get_regions_won(votes, pres_candidates)
      regions_won$Var1 <- clean_candidate_names(regions_won$Var1, "pres")
      
      create_winners_plot(regions_won, "Regions Won by Presidential Candidates", "darkred")
    })
    
    output$pres_provinces_won <- renderPlotly({
      provinces_won <- get_provinces_won(votes, pres_candidates)
      provinces_won$Var1 <- clean_candidate_names(provinces_won$Var1, "pres")
      
      create_winners_plot(provinces_won, "Provinces Won by Presidential Candidates", "darkgreen")
    })
    
    output$pres_data_table <- renderDT({
      datatable(filtered_data(), options = list(scrollX = TRUE))
    })
  })
}