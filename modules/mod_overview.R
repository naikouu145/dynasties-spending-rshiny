overview_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "overview",
    fluidRow(
      box(title = "About This Dashboard", width = 12,
          p("This dashboard provides an interactive visualization of Philippine election data from 2022 and 2025, including presidential, vice-presidential, and senatorial races. It also examines voter turnout patterns and political dynasty influence."),
          p("Use the tabs on the left to explore different aspects of the election results.")
      )
    ),
    fluidRow(
      box(title = "Top Regions by Voter Count", plotlyOutput(ns("top_regions_plot")), width = 6),
      box(title = "Top Provinces by Voter Count", plotlyOutput(ns("top_provinces_plot")), width = 6)
    ),
    fluidRow(
      box(title = "Data Summary", width = 12,
          verbatimTextOutput(ns("data_summary"))
      )
    )
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$top_regions_plot <- renderPlotly({
      total_by_region <- aggregate(actualVoters ~ region, data = sen_votes_2025, sum)
      total_by_region_sorted <- total_by_region[order(-total_by_region$actualVoters), ]
      top5_region <- total_by_region_sorted[1:5, ]
      
      create_bar_plot(top5_region, "region", "actualVoters", 
                      "Top 5 Regions by Voter Count", "Region", "Total Voters", "darkblue")
    })
    
    output$top_provinces_plot <- renderPlotly({
      total_by_province <- aggregate(actualVoters ~ province, data = sen_votes_2025, sum)
      total_by_province_sorted <- total_by_province[order(-total_by_province$actualVoters), ]
      top10_province <- total_by_province_sorted[1:10, ]
      
      create_bar_plot(top10_province, "province", "actualVoters", 
                      "Top 10 Provinces by Voter Count", "Province", "Total Voters", "darkgreen")
    })
    
    output$data_summary <- renderPrint({
      cat("Presidential Election Data:\n")
      str(votes)
      cat("\nVice Presidential Election Data:\n")
      str(vp_votes)
      cat("\nSenatorial Election Data (2022):\n")
      str(sen_votes)
      cat("\nSenatorial Election Data (2025):\n")
      str(sen_votes_2025)
    })
  })
}