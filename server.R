library(shiny)
library(stringr)
library(ggplot2)
library(tidyverse)
library(fiftystater)

source("global.R")

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

crimes <- subset(crimes, state != "alabama", state != "alaska")
#crimes$state <- state.abb[match(crimes$state, state.name)]
names(crimes)[1] <- c("FFFF") #change header's name

each_energy_per_year <- subset(utility, ENERGY_SOURCE != "Total")
each_energy_per_year$GENERATION_SUM_PER_YEAR <- ave(each_energy_per_year$GENERATION, each_energy_per_year$YEAR, FUN=sum)
each_energy_per_year$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR <- ave(each_energy_per_year$GENERATION, each_energy_per_year$ENERGY_SOURCE, each_energy_per_year$YEAR, FUN=sum)
t <- subset(utility, ENERGY_SOURCE != "Total")
t$TYPE_OF_PRODUCER <- NULL
t$FFFFF <- ave(t$GENERATION, t$STATE, t$YEAR, t$ENERGY_SOURCE, FUN=sum)

x <- subset(t, YEAR == "1999" & ENERGY_SOURCE == "Coal" & STATE != "US-TOTAL" &STATE!= "US-Total")
z <- NULL
for (val in state_dist)
{
  z <- rbind(z, head(subset(x, STATE == val), 1))
}
z$STATE <- state.name[match(z$STATE, state.abb)]
z$STATE <- tolower(z$STATE)




p <- ggplot(z, aes(map_id = STATE)) + 
  geom_map(aes(fill = FFFFF), map = fifty_states, colour = "white") + 
  geom_map(map = subset(fifty_states, id %in% c('illinois', 'new york')),
           fill = NA, colour = "blue") +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() + scale_fill_gradient(low="#ff867c", high="#b61827")


#pn <- ggplot(z, aes(fill = FFFFF, map_id = STATE)) +
#  geom_map(map = fifty_states) +
#  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#  coord_map()

p <- p + scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank()) +
  fifty_states_inset_boxes()


p2 <- ggplot(each_energy_per_year, aes(fill=ENERGY_SOURCE, y=GENERATION, x=YEAR)) +
   geom_bar(position="stack" , stat="identity") + 
   scale_y_continuous(breaks = c(5000000000, 10000000000, 15000000000), labels = c("5,000 M", "10,000 M", "15,000 M")) +
   labs(x="YEAR", y = "AMOUNT", fill = "ENERGY SOURCE")
    #+ theme(legend.position="bottom")


p3 <- ggplot(statistic) + geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR, color=ENERGY_SOURCE))


# Define server logic required to draw a histogram
function(input, output, session) {
    #Data Filter
    
    #Year numeric input end year
    observeEvent(input$startYear, {
        startYear <- input$startYear
        updateNumericInput(session, "endYear", min = ifelse(startYear < 1999, startYear, 1999))
    })
  
    #Year numberic input start year
    observeEvent(input$endYear, {
        endYear <- input$endYear
        updateNumericInput(session, "startYear", max = ifelse(endYear > 1990, endYear, 1990))
    })
  
    #energy source input check box
    observe({
        if(input$energySourceInput == "Select All" || is_empty(input$energySourceInput))  {
            updateCheckboxGroupInput(session,"energySourceInput", selected=c("Select All", energySource_dist))
          
        }
      
        output$lineChart <- renderPlot ({
            ggplot(subset(statistic, (ENERGY_SOURCE %in% input$energySourceInput))) + 
            geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR, color=ENERGY_SOURCE))
        })
        
        output$stackChart <- renderPlot({
          ggplot(subset(each_energy_per_year, (ENERGY_SOURCE %in% input$energySourceInput)), aes(fill=ENERGY_SOURCE, y=GENERATION, x=YEAR)) +
            geom_bar(position="stack" , stat="identity") + 
            scale_y_continuous(breaks = c(5000000000, 10000000000, 15000000000), labels = c("5,000 M", "10,000 M", "15,000 M")) +
            labs(x="YEAR", y = "AMOUNT", fill = "ENERGY SOURCE")
        })
        
        heatMapData <- subset(heatMapData, ENERGY_SOURCE %in% input$energySourceInput)
        heatMapData$GENERATION_SUM_PER_STATE <- ave(heatMapData$GENERATION, heatMapData$STATE, FUN=sum)
        heatMapData$STATE <- state.name[match(heatMapData$STATE, state.abb)]
        heatMapData$STATE <- tolower(heatMapData$STATE)
        heatMapPlot <- ggplot(heatMapData, aes(map_id = STATE)) + 
          geom_map(aes(fill = GENERATION_SUM_PER_STATE), map = fifty_states, colour = "white") + 
          #geom_map(map = subset(fifty_states, id %in% c('illinois', 'new york')),fill = NA, colour = "blue") +
          expand_limits(x = fifty_states$long, y = fifty_states$lat) +
          coord_map() + scale_fill_gradient(low="#ff867c", high="#b61827")
        
        output$usMap <- renderPlot ({
            heatMapPlot + scale_x_continuous(breaks = NULL) + 
              scale_y_continuous(breaks = NULL) +
              labs(x = "", y = "") +
              theme(panel.background = element_blank()) +
              fifty_states_inset_boxes()
        })
    })  
  
    #Table
    #Generation Sum table
    output$myTable <- DT::renderDataTable(
        DT::datatable({ 
            subset(statistic, select = -c(GENERATION_SUM_PER_YEAR))
        }, 
        colnames = c('Year', 'Energy Source', 'Total Generation'),
        options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE, order = list(list(1, 'asc'))), rownames = FALSE ) %>%
          formatCurrency(3, currency = "", interval = 3, mark = ",")%>% 
            formatRound(3, 0)
    )
    
    
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

