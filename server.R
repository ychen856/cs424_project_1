library(shiny)
library(stringr)
library(ggplot2)
library(tidyverse)
library(fiftystater)
library(scales)
library(usmap)
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
            geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
            #scale_y_continuous(breaks = c(0, 1000000000, 2000000000, 3000000000, 4000000000, 5000000000, 6000000000, 7000000000, 8000000000), 
            #                  labels = c("0", "1,000 M", "2,000 M", "3,000 M", "4,000 M", "5,000 M", "6,000M", "7,000 M", "8,000M")) +
            scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
            scale_y_continuous(labels = scales::comma) +
            labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE")
        })
        
        #stack chart
        output$stackChart <- renderPlot({
          ggplot(subset(each_energy_per_year, (ENERGY_SOURCE %in% input$energySourceInput)), aes(fill=ENERGY_SOURCE, y=GENERATION/1000000, x=YEAR)) +
            geom_bar(position="stack" , stat="identity") + 
            #scale_y_continuous(breaks = c(0, 2500000000, 5000000000, 7500000000, 10000000000, 12500000000, 15000000000), labels = c("0", "2,500 M", "5,000 M", "7,500 M", "10,000 M", "12,500 M", "15,000 M")) +
            scale_y_continuous(labels = scales::comma) +
            scale_fill_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
            labs(x="YEAR", y = "AMOUNT (Million)", fill = "ENERGY SOURCE")
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
      statistic_per <- statistic
      output$lineChart_per <- renderPlot ({
        ggplot(subset(statistic, (ENERGY_SOURCE %in% input$energySourceInput_per))) + 
          geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/GENERATION_SUM_PER_YEAR, color=ENERGY_SOURCE)) + 
          scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
          scale_y_continuous(labels = scales::percent) +
          labs(x="YEAR", y = "AMOUNT", colour = "ENERGY SOURCE")
      })
      
      #stack chart
      output$stackChart_per <- renderPlot({
        #each_energy_per_year_per <- subset(each_energy_per_year, (ENERGY_SOURCE %in% input$energySourceInput_per))
        #each_energy_per_year_per$GENERATION_SUM_PER_YEAR <- ave(each_energy_per_year_per$GENERATION, each_energy_per_year_per$YEAR, FUN=sum)
        ggplot(each_energy_per_year, aes(fill=ENERGY_SOURCE, y=GENERATION, x=YEAR)) + 
          geom_bar(position="fill" , stat="identity") +
          scale_fill_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
          scale_y_continuous(labels = scales::percent) +
          labs(x="YEAR", y = "AMOUNT", fill = "ENERGY SOURCE")
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
    
    
    
    ############## Comparison page 1 ###############
    observeEvent(input$energySourceInputCom, { 
        generatePlot()
    })
    
    observeEvent(input$fisrtStateInput, {  
        generatePlot()
    })
    
    observeEvent(input$firstYearInput, {
        generatePlot()
    })
    
    observeEvent(input$secondStateInput, {
        generatePlot()
    })
    
    observeEvent(input$secondYearInput, {
        generatePlot()
    })
    
    
    generatePlot <- function(){
        #condition 1: energy source
        if(input$energySourceInputCom == "All")  {
            theSourceInputCom <- energySource_dist
        }
        else {
            theSourceInputCom <- input$energySourceInputCom
        }
      
        #condition 2: state
        #state 1
        if(input$fisrtStateInput == "All States") {
            comparisonTable1 <- each_energy_per_year
        }
        else if (input$fisrtStateInput == "Washington DC"){
            comparisonTable1 <- subset(each_energy_per_year, STATE == "DC")
        }
        else {
            comparisonTable1 <- subset(each_energy_per_year, STATE == (state.abb[which(state.name == input$fisrtStateInput)]))
        }
      
        #state 2
        if(input$secondStateInput == "All States") {
            comparisonTable2 <- each_energy_per_year
        }
        else if (input$secondStateInput == "Washington DC"){
            comparisonTable2 <- subset(each_energy_per_year, STATE == "DC")
        }
        else {
            comparisonTable2 <- subset(each_energy_per_year, STATE == (state.abb[which(state.name == input$secondStateInput)]))
        }
      
      
        #condition 3: year
        #state 1
        
      
        comparisonTable1 <- subset(comparisonTable1, (ENERGY_SOURCE %in% theSourceInputCom))  
        comparisonTable1$GENERATION_SUM_PER_YEAR <- ave(comparisonTable1$GENERATION, comparisonTable1$YEAR, FUN=sum)
        comparisonTable1$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR <- ave(comparisonTable1$GENERATION, comparisonTable1$ENERGY_SOURCE, comparisonTable1$YEAR, FUN=sum)
        
        comparisonTable2 <- subset(comparisonTable2, (ENERGY_SOURCE %in% theSourceInputCom))  
        comparisonTable2$GENERATION_SUM_PER_YEAR <- ave(comparisonTable2$GENERATION, comparisonTable2$YEAR, FUN=sum)
        comparisonTable2$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR <- ave(comparisonTable2$GENERATION, comparisonTable2$ENERGY_SOURCE, comparisonTable2$YEAR, FUN=sum)
        
        
        #calculate ylim
        theLineYlim = ifelse(max(comparisonTable1$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, na.rm = TRUE) > max(comparisonTable2$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/100000, na.rm = TRUE),
                         max(comparisonTable1$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, na.rm = TRUE),
                         max(comparisonTable2$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000))
        
        theStackYlim = ifelse(max(comparisonTable1$GENERATION_SUM_PER_YEAR/1000000, na.rm = TRUE) > max(comparisonTable2$GENERATION_SUM_PER_YEAR/1000000, na.rm = TRUE),
                              max(comparisonTable1$GENERATION_SUM_PER_YEAR/1000000, na.rm = TRUE),
                              max(comparisonTable2$GENERATION_SUM_PER_YEAR/1000000))
        
        #plot first state
        #line chart
        output$firstStateLineChart <- renderPlot ({
          ggplot(comparisonTable1) + 
            geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
            geom_point(data = comparisonTable1[which(comparisonTable1$YEAR == input$firstYearInput),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") +
            scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
            labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
            lims(y=c(0, theLineYlim), x=c(1990, 2019)) +
            scale_y_continuous(labels = scales::comma)
        })
        
        #stack chart
        output$firstStateStackChart <- renderPlot({
            ggplot(comparisonTable1, aes(fill=ENERGY_SOURCE, y=GENERATION/1000000, x=YEAR)) +
              geom_bar(aes(alpha = YEAR == input$firstYearInput), position="stack" , stat="identity") + 
              scale_fill_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", fill = "ENERGY SOURCE") +
              scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = F) +
              lims(y=c(0, theStackYlim)) +
              scale_y_continuous(labels = scales::comma)
        })
        
        
        heatMapDataState1 <- subset(heatMapData, heatMapData$ENERGY_SOURCE %in% theSourceInputCom & heatMapData$YEAR == input$firstYearInput)
        
        #print(heatMapData)
        
        
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
        
        
        
        
        
        #heat map
        output$firstStateHeatMap <- renderPlot({
            plot_usmap(data = heatMapDataState1, values = "GENERATION_SUM_PER_STATE_Milli") + 
              scale_fill_gradient(low="#ff867c", high="#b61827") +
              scale_fill_continuous(
                low = "white", high = "red", name = "AMOUNT (Million)", label = scales::comma
              ) +
            
            scale_fill_continuous(
              low = "white", high = "red", name = "AMOUNT (Million)", label = scales::comma
            ) +
            theme(legend.position = "right")
              #scale_size_continuous(range = c(1, 16), name = "AMOUNT (Million)", label = scales::comma) 
            #print(heatMapData)
        })
        
        
        
        #plot second state
        #line chart
        output$secondStateLineChart <- renderPlot ({
          ggplot(comparisonTable2) + 
            geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
            geom_point(data = comparisonTable2[which(comparisonTable2$YEAR == input$secondYearInput),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") + 
            scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
            labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
            lims(y=c(0, theLineYlim)) +
            scale_y_continuous(labels = scales::comma) 
        })
        
        #stack chart
        output$secondStateStackChart <- renderPlot({
          ggplot(comparisonTable2, aes(fill=ENERGY_SOURCE, y=GENERATION/1000000, x=YEAR)) +
            geom_bar(aes(alpha = YEAR == input$secondYearInput), position="stack" , stat="identity") + 
            scale_fill_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
            labs(x="YEAR", y = "AMOUNT (Million)", fill = "ENERGY SOURCE") +
            scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = F) +
            lims(y=c(0, theStackYlim)) +
            scale_y_continuous(labels = scales::comma)
        })
    }
    
    
    
    
    
    
    
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

