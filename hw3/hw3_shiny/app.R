#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
setwd("/home/zexuan55/biostat-m280-2018-winter/hw3")
payroll <- read_rds("payroll.rds")

# Define UI for application that draws a histogram
ui <- navbarPage("Result Categories",
   
  tabPanel("Most-earning Employee",
    fluidRow(
      column(4,
        selectInput("y1", 
                    "Year: Most-earning Employee", 
                    sort(unique(payroll[[2]]), decreasing = FALSE))
         
      ),
      column(
        sliderInput("n1",
                    "Top Number of Employees",
                    min = 1,
                    max = 100,
                    value = 10)
      )
    )
  ),
  
  tabPanel("Most-earning Department",
    fluidRow(
      column(4,
        selectInput("y2", 
                    "Year: Most-earning Departments", 
                    sort(unique(payroll[[2]]), decreasing = FALSE))
      ),
      column(
        sliderInput("n2",
                    "Top Number of Employees",
                    min = 1,
                    max = 88,
                    value = 5)
      )
    )
  ),
  
  tabPanel("Most-costing Department",
    fluidRow(
      column(4,
        selectInput("y3", 
                    "Year: Most-costing Departments", 
                    sort(unique(payroll[[2]]), decreasing = FALSE))
      ),
      column(4,
             sliderInput("n3",
                         "Top Number of Employees",
                         min = 1,
                         max = 88,
                         value = 5) 
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

