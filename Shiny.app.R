library(shiny)
library(shinydashboard)
source("Shiny.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Sales Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sales Analysis", tabName = "Sales", icon = icon("dashboard")),
      menuItem("Profit Analysis", tabName = "Profit", icon = icon("dashboard")),
      menuItem("Profit vs sales", tabName = "scatter", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Sales",
        wellPanel( fluidRow(
          box(width=6,height = "100%",
                 plotOutput("sales",height = "500px"),
                 title="sales_vs_sub-category"
          ), box(width=6,height = "100%",
                   plotOutput("sales2",height = "500px"),
                   title="sales_vs_region"
                  ),
          box(width=12,height = "100%",
              plotOutput("sales3",height = "500px"),
              title="Monthly trend of Sales"
          )
             )
      )),
      tabItem(
        tabName = "Profit",
        wellPanel( fluidRow(
          box(width=6,height = "100%",
              
                plotOutput("profit",height = "500px"),
                title="profit_vs_sub-category"
              
              
          ), box(width=6,height = "100%",
                 
                   plotOutput("profit2",height = "500px"),
                   title ="profit_vs_region"
                 
                 
          ),
          box(width=12,height = "100%",
              
              plotOutput("profit3",height = "500px"),
              title ="Monthly trend of Profit"
              
              
          )
        )
        
        )
        
      ),
      tabItem(
        tabName = "scatter",
        wellPanel(
          fluidRow(
            box(width = 12,
              plotOutput("scatter",height = "600px"),
              title = "Profit vs Sales"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$sales <- renderPlot({
    col_sub_sales()
  })

  output$profit <- renderPlot({
    col_Sub_profit()
  })
  output$sales2 <- renderPlot({
    col_region_sales()
  })
  
  output$profit2 <- renderPlot({
    col_region_profit()
  })
  
  output$scatter <- renderPlot({
    Scatter_plot
  })
  
  output$sales3 <- renderPlot({
    time_series_sales
  })
  
  output$profit3 <- renderPlot({
    time_series_profit
  })
  
}

shinyApp(ui, server)
