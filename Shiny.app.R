library(shiny)
library(shinydashboard)
source("Shiny.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Sales Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sales vs Sub-cat", tabName = "Sales", icon = icon("dashboard")),
      menuItem("Profit vs Sub-cat", tabName = "Profit", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Sales",
        fluidRow(
          column(12,
            plotlyOutput("sales")
          )
        )
      ),
      tabItem(
        tabName = "Profit",
        fluidRow(
          column(12,
            plotlyOutput("profit")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$sales <- renderPlotly({
    col_sub_sales()
  })

  output$profit <- renderPlotly({
    col_Sub_profit()
  })
}

shinyApp(ui, server)
