#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


## load dataset

#acs <- readRDS(url("https://sscc.wisc.edu/sscc/pubs/dvr/acs.rds"))
acs_small <- readRDS(url("https://sscc.wisc.edu/sscc/pubs/dvr/acs_small.rds"))


library(DT)
library(shiny)

# Define UI for application that draws a histogram
library(shiny)

ui <- fluidPage(
    #Application title
  titlePanel("American Community Survey Report"),
  
  #sidebar layout of application
  sidebarLayout(
      sidebarPanel(
          selectInput("varName",
                      label = "Select Married Status:",
                      choices = sort(unique(acs_small$maritalStatus)),
                      selected = "Now married"),
          selectInput("num1",
                      label = "Select Race:",
                      choices = sort(unique(acs_small$rac)),
                      selected = "White"
                    ),
      ),
      mainPanel(
        DTOutput("table")
      )
  )
 
)

server <- function(input, output, session) {
    
    filterData <- reactive({
        req(input$varName, input$num1)
        subset(acs_small, varName == input$varName & Num1 ==  input$Num1)
    })
  
    output$table <- renderDT({
        filterData()
      })
}

shinyApp(ui, server)