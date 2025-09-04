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
          # changes selectInput to RadioButton input control
            radioButtons(
                inputId = "var",
                label = "Select the car attribute to average:", 
                choices = c("Miles/(US) gallon" = "mpg", "Gross horsepower" = "hp",
                                   "	Weight (1000 lbs)" = "wt", "Displacement (cu.in.)" = "disp"), # Added descriptive names of the mtcars
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