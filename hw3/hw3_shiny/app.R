#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (!"shiny" %in% rownames(installed.packages()))  
  install.packages("shiny", repos="http://cran.rstudio.com/")
if (!"DT" %in% rownames(installed.packages()))  
  install.packages("DT", repos="http://cran.rstudio.com/")

library(shiny)
library(DT)
setwd("/home/zexuan55/biostat-m280-2018-winter/hw3")
payroll <- read_rds("payroll.rds")

# Define UI for application that draws a histogram
ui <- navbarPage("Select a Table",
  tabPanel("Total Payroll",
    fluidRow(tableOutput("table0"))
  ),
  
  tabPanel("Most-earning Employee",
    fluidRow(
      column(4,
        selectInput("y1", 
                    "Year", 
                    sort(unique(payroll[[2]]), decreasing = FALSE),
                    selected = 2017)
         
      ),
      column(4,
        sliderInput("n1",
                    "Top Number of Employees",
                    min = 1,
                    max = 100,
                    value = 10)
      )
    ),
    fluidRow(
      DT::dataTableOutput("table1")
    )
  ),
  
  tabPanel("Most-earning Department",
    fluidRow(
      column(4,
        selectInput("y2", 
                    "Year", 
                    sort(unique(payroll[[2]]), decreasing = FALSE),
                    selected = 2017)
      ),
      column(4,
        sliderInput("n2",
                    "Top Number of Departments",
                    min = 1,
                    max = 88,
                    value = 5)
      )
    ),
    fluidRow(
      DT::dataTableOutput("table2")
    )
  ),
  
  tabPanel("Most-costing Department",
    fluidRow(
      column(4,
        selectInput("y3", 
                    "Year", 
                    sort(unique(payroll[[2]]), decreasing = FALSE),
                    selected = 2017)
      ),
      column(4,
             sliderInput("n3",
                         "Top Number of Departments",
                         min = 1,
                         max = 88,
                         value = 5) 
      )
    ),
    fluidRow(
      DT::dataTableOutput("table3")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$table0 <- renderTable({
      data <- payroll %>%
        select(16, 17, 22, 23) %>%
        summarise("LA Total Pay" = sum(.[[1]], na.rm = TRUE),
                  "LA Total Base Pay" = sum(.[[2]], na.rm = TRUE),
                  "LA Total Overtime Pay" = sum(.[[3]], na.rm = TRUE),
                  "LA Total Other Pay" = sum(.[[4]]), na.rm = TRUE)
      data
  })
  
  output$table1 <- DT::renderDataTable(
    DT::datatable({
      data <- payroll %>%
        select(1, 2, 16, 17, 22, 23) %>%
        filter(Year == as.integer(input$y1), !is.na("Total Payments")) %>%
        arrange(desc(.[[3]])) %>%
        rename("Employee ID" = "Row ID") %>%
        head(input$n1)
      data
    }, options = list(pageLength = 20))
  )
  
  output$table2 <- DT::renderDataTable(
    DT::datatable({
      data <- payroll %>%
        select(3, 2, 16, 17, 22, 23) %>%
        filter(Year == as.integer(input$y2), !is.na("Total Payments")) %>%
        mutate(dp = .[[1]]) %>%
        group_by(dp) %>%
        summarise("Department Mean Total Pay" = mean(.[[3]], na.rm = TRUE),
                  "Department Mean Base Pay" = mean(.[[4]], na.rm = TRUE),
                  "Department Mean Overtime Pay" = mean(.[[5]], na.rm = TRUE),
                  "Department Mean Other Pay" = mean(.[[6]]), na.rm = TRUE) %>%
        arrange(desc("Department Mean Pay")) %>%
        head(input$n2)
      data
    }, options = list(pageLength = 20))
  )
  
  output$table3 <- DT::renderDataTable(
    DT::datatable({
      data <- payroll %>%
        select(3, 2, 16, 17, 22, 23) %>%
        filter(Year == as.integer(input$y3), !is.na("Total Payments")) %>%
        mutate(dp = .[[1]]) %>%
        group_by(dp) %>%
        summarise("Department Total Pay" = sum(.[[3]], na.rm = TRUE),
                  "Department Total Base Pay" = sum(.[[4]], na.rm = TRUE),
                  "Department Total Overtime Pay" = sum(.[[5]], na.rm = TRUE),
                  "Department Total Other Pay" = sum(.[[6]]), na.rm = TRUE) %>%
        arrange(desc("Department Total Pay")) %>%
        head(input$n3)
      data
    }, options = list(pageLength = 20))
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

