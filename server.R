library(shiny)
# Define server logic required to draw a histogram
function(input, output) {
  output$data <- renderTable({
    mtcars[, c("mpg", input$variable, input$variable2, input$variable3, input$variable4, input$variable5), drop = FALSE]
  }, rownames = TRUE)
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

