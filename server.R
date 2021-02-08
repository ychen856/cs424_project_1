library(shiny)
library(stringr)
library(ggplot2)
library(tidyverse)
library(fiftystater)
library(scales)
source("global.R")



p2 <- ggplot(each_energy_per_year, aes(fill=ENERGY_SOURCE, y=GENERATION, x=YEAR)) +
   geom_bar(position="stack" , stat="identity") + 
   scale_y_continuous(breaks = c(5000000000, 10000000000, 15000000000), labels = c("5,000 M", "10,000 M", "15,000 M")) +
   labs(x="YEAR", y = "AMOUNT", fill = "ENERGY SOURCE")
    #+ theme(legend.position="bottom")


p3 <- ggplot(statistic) + geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR, color=ENERGY_SOURCE))


# Define server logic required to draw a histogram
function(input, output, session) {
    ############## Page 1 (Total Amount)############
    #Data Filter
    #Year numeric input end year
    observeEvent(input$startYear, {
        startYear <- input$startYear
        updateNumericInput(session, "endYear", min = ifelse(startYear < 2019, startYear, 2019))
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
        
        #plot
        #line chart
        output$lineChart <- renderPlot ({
            ggplot(subset(statistic, (ENERGY_SOURCE %in% input$energySourceInput))) + 
            geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/100000, color=ENERGY_SOURCE)) + 
            #scale_y_continuous(breaks = c(0, 1000000000, 2000000000, 3000000000, 4000000000, 5000000000, 6000000000, 7000000000, 8000000000), 
            #                  labels = c("0", "1,000 M", "2,000 M", "3,000 M", "4,000 M", "5,000 M", "6,000M", "7,000 M", "8,000M")) +
            scale_color_manual(values = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#de77ae", "#9970ab", "#f46d43", "#1a9850", "#66c2a5", "#3288bd", "#5e4fa2", "#40004b", "#762a83"), name = "text", labels = c('Coal', 'Hydroelectric Conventional', 'Natural Gas', 'Petroleum', 'Wind', 'Wood and Wood Derived Fuels', 'Nuclear', 'Other Biomass', 'Other Gases', 'Pumped Storage', 'Geothermal', 'Other', 'Solar Thermal and Photovoltaic')) +
            scale_y_continuous(labels = scales::comma) +
            labs(x="YEAR", y = "AMOUNT (Million)", fill = "ENERGY SOURCE")
        })
        
        #stack chart
        output$stackChart <- renderPlot({
          ggplot(subset(each_energy_per_year, (ENERGY_SOURCE %in% input$energySourceInput)), aes(fill=ENERGY_SOURCE, y=GENERATION/100000, x=YEAR)) +
            geom_bar(position="stack" , stat="identity") + 
            #scale_y_continuous(breaks = c(0, 2500000000, 5000000000, 7500000000, 10000000000, 12500000000, 15000000000), labels = c("0", "2,500 M", "5,000 M", "7,500 M", "10,000 M", "12,500 M", "15,000 M")) +
            scale_y_continuous(labels = scales::comma) +
            scale_fill_manual(values = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#de77ae", "#9970ab", "#f46d43", "#1a9850", "#66c2a5", "#3288bd", "#5e4fa2", "#40004b", "#762a83"), name = "text", labels = c('Coal', 'Hydroelectric Conventional', 'Natural Gas', 'Petroleum', 'Wind', 'Wood and Wood Derived Fuels', 'Nuclear', 'Other Biomass', 'Other Gases', 'Pumped Storage', 'Geothermal', 'Other', 'Solar Thermal and Photovoltaic')) +
            labs(x="YEAR", y = "AMOUNT (Million)", fill = "ENERGY SOURCE")
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
            subset(statistic, select = -c(GENERATION_SUM_PER_YEAR, GENERATION_PERCENTAGE_IN_YEAR))
        }, 
        colnames = c('Year', 'Energy Source', 'Total Generation'),
        options = list(searching = FALSE, pageLength = 19, lengthChange = FALSE, order = list(list(1, 'asc'))), rownames = FALSE ) %>%
          formatCurrency(3, currency = "", interval = 3, mark = ",")%>% 
            formatRound(3, 0)
    )
    
    ############## Page 2 (Percentage)############
    #Data Filter
    #energy source input check box
    observe({
      if(input$energySourceInput_per == "Select All" || is_empty(input$energySourceInput_per))  {
        updateCheckboxGroupInput(session,"energySourceInput_per", selected=c("Select All", energySource_dist))
        
      }
      
      #plot
      #line chart
      output$lineChart_per <- renderPlot ({
        ggplot(subset(statistic, (ENERGY_SOURCE %in% input$energySourceInput_per))) + 
          geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/100000, color=ENERGY_SOURCE)) + 
          #scale_y_continuous(breaks = c(0, 1000000000, 2000000000, 3000000000, 4000000000, 5000000000, 6000000000, 7000000000, 8000000000), 
          #                  labels = c("0", "1,000 M", "2,000 M", "3,000 M", "4,000 M", "5,000 M", "6,000M", "7,000 M", "8,000M")) +
          scale_color_manual(values = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#de77ae", "#9970ab", "#f46d43", "#1a9850", "#66c2a5", "#3288bd", "#5e4fa2", "#40004b", "#762a83"), name = "text", labels = c('Coal', 'Hydroelectric Conventional', 'Natural Gas', 'Petroleum', 'Wind', 'Wood and Wood Derived Fuels', 'Nuclear', 'Other Biomass', 'Other Gases', 'Pumped Storage', 'Geothermal', 'Other', 'Solar Thermal and Photovoltaic')) +
          scale_y_continuous(labels = scales::comma) +
          labs(x="YEAR", y = "AMOUNT (Million)", fill = "ENERGY SOURCE")
      })
      
      #stack chart
      output$stackChart_per <- renderPlot({
        ggplot(subset(each_energy_per_year, (ENERGY_SOURCE %in% input$energySourceInput_per)), aes(fill=ENERGY_SOURCE, y=GENERATION/100000, x=YEAR)) +
          geom_bar(position="stack" , stat="identity") + 
          #scale_y_continuous(breaks = c(0, 2500000000, 5000000000, 7500000000, 10000000000, 12500000000, 15000000000), labels = c("0", "2,500 M", "5,000 M", "7,500 M", "10,000 M", "12,500 M", "15,000 M")) +
          scale_y_continuous(labels = scales::comma) +
          scale_fill_manual(values = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#de77ae", "#9970ab", "#f46d43", "#1a9850", "#66c2a5", "#3288bd", "#5e4fa2", "#40004b", "#762a83"), name = "text", labels = c('Coal', 'Hydroelectric Conventional', 'Natural Gas', 'Petroleum', 'Wind', 'Wood and Wood Derived Fuels', 'Nuclear', 'Other Biomass', 'Other Gases', 'Pumped Storage', 'Geothermal', 'Other', 'Solar Thermal and Photovoltaic')) +
          labs(x="YEAR", y = "AMOUNT (Million)", fill = "ENERGY SOURCE")
      })
    })  
    
    #Table
    #Generation Sum table
    #statistic_per$GENERATION_PERCENTAGE <- (statistic$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/statistic$GENERATION_SUM_PER_YEAR)
    output$myTable_per <- DT::renderDataTable(
      DT::datatable({ 
        subset(statistic, select = -c(GENERATION_SUM_PER_YEAR, GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR))
      }, 
      colnames = c('Year', 'Energy Source', 'Generation Ratio'),
      options = list(searching = FALSE, pageLength = 19, lengthChange = FALSE, order = list(list(1, 'asc'))), rownames = FALSE ) %>%
        formatCurrency(3, currency = "", interval = 3, mark = ",")%>% 
        formatPercentage(3, 1)
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

