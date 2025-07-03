# Main application file
source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Philippine Elections Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Presidential Election", tabName = "pres", icon = icon("user")),
      menuItem("Vice Presidential", tabName = "vp", icon = icon("user")),
      menuItem("Senatorial Elections", tabName = "sen", icon = icon("users")),
      menuItem("Voter Turnout", tabName = "turnout", icon = icon("chart-bar")),
      menuItem("Political Dynasties", tabName = "dynasties", icon = icon("landmark")),
      menuItem("Fiscal Data", tabName = "fiscal", icon = icon("money-bill-wave"))
    )
  ),
  dashboardBody(
    tabItems(
      overview_ui("overview"),
      presidential_ui("pres"),
      vice_presidential_ui("vp"),
      senatorial_ui("sen"),
      turnout_ui("turnout"),
      dynasties_ui("dynasties"),
      fiscal_ui("fiscal")
    )
  )
)

server <- function(input, output, session) {
  overview_server("overview")
  presidential_server("pres")
  vice_presidential_server("vp")
  senatorial_server("sen")
  turnout_server("turnout")
  dynasties_server("dynasties")
  fiscal_server("fiscal")
}

shinyApp(ui, server)