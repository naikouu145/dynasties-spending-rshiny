vice_presidential_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "vp",
    fluidRow(
      box(title = "Controls", width = 3,
          selectInput(ns("vp_region"), "Select Region:", 
                      choices = c("All", unique(vp_votes$region)))
      ),
      box(title = "Vice Presidential Candidates Vote Share", width = 9,
          plotlyOutput(ns("vp_votes_plot")))
    ),
    fluidRow(
      box(title = "Regions Won by Each Candidate", width = 6,
          plotlyOutput(ns("vp_regions_won"))),
      box(title = "Provinces Won by Each Candidate", width = 6,
          plotlyOutput(ns("vp_provinces_won")))
    ),
    fluidRow(
      box(title = "Vice Presidential Election Data", width = 12,
          DTOutput(ns("vp_data_table")))
    )
  )
}

vice_presidential_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    vp_candidates <- c("vp_1_atienza", "vp_2_bello", "vp_3_david",
                       "vp_4_duterte", "vp_5_lopez", "vp_6_ong",
                       "vp_7_pangilinan", "vp_8_serapio", "vp_9_sotto")
    
    filtered_data <- reactive({
      if (input$vp_region != "All") {
        vp_votes[vp_votes$region == input$vp_region, ]
      } else {
        vp_votes
      }
    })
    
    output$vp_votes_plot <- renderPlotly({
      data <- filtered_data()
      top_candidates <- get_top_candidates(data, vp_candidates, n = 9)
      top_candidates$Candidate <- clean_candidate_names(top_candidates$Candidate, "vp")
      
      create_candidate_plot(top_candidates, "Vice Presidential Candidates", "purple")
    })
    
    output$vp_regions_won <- renderPlotly({
      regions_won <- get_regions_won(vp_votes, vp_candidates)
      regions_won$Var1 <- clean_candidate_names(regions_won$Var1, "vp")
      
      create_winners_plot(regions_won, "Regions Won by VP Candidates", "darkorange")
    })
    
    output$vp_provinces_won <- renderPlotly({
      provinces_won <- get_provinces_won(vp_votes, vp_candidates)
      provinces_won$Var1 <- clean_candidate_names(provinces_won$Var1, "vp")
      
      create_winners_plot(provinces_won, "Provinces Won by VP Candidates", "darkcyan")
    })
    
    output$vp_data_table <- renderDT({
      datatable(filtered_data(), options = list(scrollX = TRUE))
    })
  })
}