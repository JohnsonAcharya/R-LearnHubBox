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
                inputId = "var",
                label = "Pick a variable:",
                choices = c("Weight" = "wt", "Horsepower" = "hp"),
                selected = "wt"
            )
            
        ),
        textOutput("mytext")  
    )
    
    
    
)

server <- function(input, output, session) {
  
    
    output$mytext <- renderText({
        paste("Average of ", input$var, "is", mean(mtcars[, input$var]))
    })
}

shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))