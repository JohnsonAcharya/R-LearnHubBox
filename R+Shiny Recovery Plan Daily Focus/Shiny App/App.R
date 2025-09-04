

library(shiny)

ui <- fluidPage(
  titlePanel("Mtcars Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "xvar",
        label = "Choose X variable",
        choices = c("M" = 'mpg',
                    "H" = "hp",
                    "W" = "wt",
                    "D" = "disp"),
        selected = "mpg"
      ),
      sliderInput(inputId = "bin",
                  label = "No of Bin:",
                  min = 5, max = 50, value = 15)
    ),
  
      mainPanel(
        plotOutput(outputId = "hist"),
        br(),
        tableOutput(outputId = "data_tbl")
      )
  )  
)

server <- function(input, output, session) {
  
  output$hist <- renderPlot({
   x <- mtcars[[input$xvar]]
   bin <- seq(min(x, na.rm = TRUE),
             max(x, na.rm = TRUE),
             length.out = input$bin + 1)
   
   hist(x, breaks = bin,
        main = paste("Histogram of", input$xvar),
        xlab = input$xvar)
  })
  
  output$data_tbl <- renderTable({
    head(mtcars)
  }, rownames = T
  )
}

shinyApp(ui, server)