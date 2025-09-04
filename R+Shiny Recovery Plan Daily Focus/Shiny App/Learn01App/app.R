#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
### Learn 001 Shiny  - The Structure of a Shiny App  ###

# load libraries and read the data

library(shiny)

ui <- fluidPage(
  
    titlePanel("Learn 001 Shiny App"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "xvar",
                label = "Pick a variable for x axis:",
                choices = c("wt", "hp"),
                selected = "wt"
            )
            
        ),
        plotOutput("myplot")
    )
    
    
    
)

server <- function(input, output, session) {
  
    
    output$myplot <- renderPlot({
        plot(mtcars[, input$xvar], mtcars$mpg)
    })
}

shinyApp(ui, server)