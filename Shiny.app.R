library(shiny)
library(shinydashboard)
source("Shiny.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Sales Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sales Analysis", tabName = "Sales", icon = icon("dashboard")),
      menuItem("Profit Analysis", tabName = "Profit", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Sales",
        wellPanel( fluidRow(
          box(width=6,height = "100%",
            
                 plotOutput("sales"),
                 title="sales_vs_sub-category"
                 
          
      
          ), box(width=6,height = "100%",
                
                   plotOutput("sales2"),
                   title="sales_vs_region"
                 
                 
          )
        )
       
      )),
      tabItem(
        tabName = "Profit",
        wellPanel( fluidRow(
          box(width=6,height = "100%",
              
                plotOutput("profit"),
                title="profit_vs_sub-category"
              
              
          ), box(width=6,height = "100%",
                 
                   plotOutput("profit2"),
                   title ="profit_vs_region"
                 
                 
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
}

shinyApp(ui, server)
