library(shiny)
library(stringr)

temp = list.files(pattern="*state.csv")
orgData <- lapply(temp, read.csv)
utility <- do.call(rbind, orgData)

names(utility)[3] <- c("TYPE_OF_PRODUCER") #change header's name
names(utility)[4] <- c("ENERGY_SOURCE")
names(utility)[5] <- c("GENERATION")

utility$GENERATION <- as.numeric(gsub(",", "", utility$GENERATION)) #convert generation from char to number
utility$GENERATION[utility$GENERATION < 0] <- 0

#utility <- utility[order(utility$STATE),]
#df <-data_frame[order(data_frame$c1),]

# Define server logic required to draw a histogram
function(input, output) {
  output$data <- renderTable({
    subset(utility, str_squish(utility$STATE) == "" | is.null(utility$STATE))
  }, rownames = TRUE)
  
  output$data2 <- renderTable({
    utility[1:10,]
  }, rownames = TRUE)
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

