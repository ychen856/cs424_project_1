library(shiny)
library(stringr)
library(ggplot2)
library(tidyverse)
library(fiftystater)
library(scales)
library(usmap)
library(shinycssloaders)
source("global.R")


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
        
        heatMapDataState1 <- subset(heatMapData, heatMapData$ENERGY_SOURCE %in% theSourceInputCom & heatMapData$YEAR == input$firstYearInput)
        heatMapDataState1$GENERATION_SUM_PER_STATE_Milli <- ave(heatMapDataState1$GENERATION_SUM_PER_SOURCE_STATE_Milli, heatMapDataState1$STATE, FUN=sum)
        heatMapDataState1$ENERGY_SOURCE <- NULL
        heatMapDataState1$GENERATION_SUM_PER_SOURCE_STATE <- NULL
        heatMapDataState1$GENERATION_SUM_PER_SOURCE_STATE_Milli <- NULL
        heatMapDataState1 <- heatMapDataState1[!duplicated(heatMapDataState1),]
        
        heatMapDataState2 <- subset(heatMapData, heatMapData$ENERGY_SOURCE %in% theSourceInputCom & heatMapData$YEAR == input$secondYearInput)
        heatMapDataState2$GENERATION_SUM_PER_STATE_Milli <- ave(heatMapDataState2$GENERATION_SUM_PER_SOURCE_STATE_Milli, heatMapDataState2$STATE, FUN=sum)
        heatMapDataState2$ENERGY_SOURCE <- NULL
        heatMapDataState2$GENERATION_SUM_PER_SOURCE_STATE <- NULL
        heatMapDataState2$GENERATION_SUM_PER_SOURCE_STATE_Milli <- NULL
        heatMapDataState2 <- heatMapDataState2[!duplicated(heatMapDataState2),]
        
        
        #calculate ylim
        theLineYlim = ifelse(max(comparisonTable1$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, na.rm = TRUE) > max(comparisonTable2$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/100000, na.rm = TRUE),
                         max(comparisonTable1$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, na.rm = TRUE),
                         max(comparisonTable2$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000))
        
        theStackYlim = ifelse(max(comparisonTable1$GENERATION_SUM_PER_YEAR/1000000, na.rm = TRUE) > max(comparisonTable2$GENERATION_SUM_PER_YEAR/1000000, na.rm = TRUE),
                              max(comparisonTable1$GENERATION_SUM_PER_YEAR/1000000, na.rm = TRUE),
                              max(comparisonTable2$GENERATION_SUM_PER_YEAR/1000000))
        
        theHeatLegendLim = ifelse(max(heatMapDataState1$GENERATION_SUM_PER_STATE_Milli, na.rm = TRUE) > max(heatMapDataState2$GENERATION_SUM_PER_STATE_Milli, na.rm = TRUE),
                                  max(heatMapDataState1$GENERATION_SUM_PER_STATE_Milli, na.rm = TRUE),
                                  max(heatMapDataState2$GENERATION_SUM_PER_STATE_Milli))
        
        
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
        
        #heat map
        output$firstStateHeatMap <- renderPlot({
            p_firstState <- plot_usmap(data = heatMapDataState1, values = "GENERATION_SUM_PER_STATE_Milli") + 
              theme(legend.position = "right")
            heatMapLegend(p_firstState, theHeatLegendLim, "amount")
            
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
        
        #heat map
        output$secondStateHeatMap <- renderPlot({
          p_secondState <- plot_usmap(data = heatMapDataState2, values = "GENERATION_SUM_PER_STATE_Milli") + 
            theme(legend.position = "right")
          heatMapLegend(p_secondState, theHeatLegendLim, "amount")
          
        })
    }
    
    
    
    
    
    ############## Comparison page 2 ###############
    observeEvent(input$energySourceInputCom_per, { 
      generatePlot_per()
    })
    
    observeEvent(input$fisrtStateInput_per, {  
      generatePlot_per()
    })
    
    observeEvent(input$firstYearInput_per, {
      generatePlot_per()
    })
    
    observeEvent(input$secondStateInput_per, {
      generatePlot_per()
    })
    
    observeEvent(input$secondYearInput_per, {
      generatePlot_per()
    })
    
    
    generatePlot_per <- function(){
      #condition 1: energy source
      if(input$energySourceInputCom_per == "All")  {
        theSourceInputCom_per <- energySource_dist
      }
      else {
        theSourceInputCom_per <- input$energySourceInputCom_per
      }
      
      #condition 2: state
      #state 1
      if(input$fisrtStateInput_per == "All States") {
        theFirstStateInput_per <- state_dist
        #comparisonTable1_per <- each_energy_per_year
      }
      else if (input$fisrtStateInput_per == "Washington DC"){
        theFirstStateInput_per <- "DC"
        #comparisonTable1_per <- subset(each_energy_per_year, STATE == "DC")
      }
      else {
        theFirstStateInput_per <- state.abb[which(state.name == input$fisrtStateInput_per)]
        #comparisonTable1_per <- subset(each_energy_per_year, STATE == (state.abb[which(state.name == input$fisrtStateInput_per)]))
      }
      
      #state 2
      if(input$secondStateInput_per == "All States") {
        theSecondStateInput_per <- state_dist
      }
      else if (input$secondStateInput_per == "Washington DC"){
        theSecondStateInput_per <- "DC"
      }
      else {
        theSecondStateInput_per <- state.abb[which(state.name == input$secondStateInput_per)]
      }
      
      
      #condition 3: year
      #state 1
      
      comparisonTable_per <- each_energy_per_year
      comparisonTable_per$GENERATION_SUM_PER_YEAR_STATE_PER <- ave(comparisonTable_per$GENERATION, comparisonTable_per$STATE, comparisonTable_per$YEAR, FUN=sum)
      comparisonTable_per$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER <- ave(comparisonTable_per$GENERATION, comparisonTable_per$STATE, comparisonTable_per$YEAR, comparisonTable_per$ENERGY_SOURCE, FUN=sum)
      
      comparisonTable_per$TYPE_OF_PRODUCER <- NULL
      comparisonTable_per$GENERATION <- NULL
      comparisonTable_per$GENERATION_SUM_PER_YEAR <- NULL
      comparisonTable_per$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR <- NULL
      
      if(input$fisrtStateInput_per != "All States") {
          comparisonTable_per <- subset(comparisonTable_per, STATE %in% theFirstStateInput_per)
          comparisonTable1_per <- comparisonTable_per[!duplicated(comparisonTable_per),]
      }
      else {
          comparisonTable1_per <- statistic
          comparisonTable1_per$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER <- comparisonTable1_per$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR
          comparisonTable1_per$GENERATION_SUM_PER_YEAR_STATE_PER <- comparisonTable1_per$GENERATION_SUM_PER_YEAR
      }
      
      if(input$secondStateInput_per != "All States") {
        comparisonTable_per <- subset(comparisonTable_per, STATE %in% theSecondStateInput_per)
        comparisonTable2_per <- comparisonTable_per[!duplicated(comparisonTable_per),]
      }
      else {
        comparisonTable2_per <- statistic
        comparisonTable2_per$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER <- comparisonTable2_per$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR
        comparisonTable2_per$GENERATION_SUM_PER_YEAR_STATE_PER <- comparisonTable2_per$GENERATION_SUM_PER_YEAR
      }
      
      
      heatMapDataState1_per <- subset(heatMapData, heatMapData$ENERGY_SOURCE %in% theSourceInputCom_per & heatMapData$YEAR == input$firstYearInput_per)
      #heatMapDataState1_per$GENERATION_SUM_PER_STATE_Milli <- ave(heatMapDataState1_per$GENERATION_SUM_PER_SOURCE_STATE_Milli, heatMapDataState1_per$STATE, FUN=sum)
      heatMapDataState1_per$ENERGY_SOURCE <- NULL
      heatMapDataState1_per$GENERATION_SUM_PER_SOURCE_STATE <- NULL
      heatMapDataState1_per$GENERATION_SUM_PER_SOURCE_STATE_Milli <- NULL
      heatMapDataState1_per <- heatMapDataState1_per[!duplicated(heatMapDataState1_per),]
      
      heatMapDataState2_per <- subset(heatMapData, heatMapData$ENERGY_SOURCE %in% theSourceInputCom_per & heatMapData$YEAR == input$secondYearInput_per)
      #heatMapDataState2_per$GENERATION_SUM_PER_STATE_Milli <- ave(heatMapDataState2_per$GENERATION_SUM_PER_SOURCE_STATE_Milli, heatMapDataState2_per$STATE, FUN=sum)
      heatMapDataState2_per$ENERGY_SOURCE <- NULL
      heatMapDataState2_per$GENERATION_SUM_PER_SOURCE_STATE <- NULL
      heatMapDataState2_per$GENERATION_SUM_PER_SOURCE_STATE_Milli <- NULL
      heatMapDataState2_per <- heatMapDataState2_per[!duplicated(heatMapDataState2_per),]
      
      
      #calculate ylim
      theLineYlim_per = ifelse(max(comparisonTable1_per$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/comparisonTable1_per$GENERATION_SUM_PER_YEAR_STATE_PER, na.rm = TRUE) > max(comparisonTable2_per$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/comparisonTable2_per$GENERATION_SUM_PER_YEAR_STATE_PER, na.rm = TRUE),
                           max(comparisonTable1_per$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/comparisonTable1_per$GENERATION_SUM_PER_YEAR_STATE_PER, na.rm = TRUE),
                           max(comparisonTable2_per$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/comparisonTable2_per$GENERATION_SUM_PER_YEAR_STATE_PER))
      
      theHeatLegendLim = ifelse(max(heatMapDataState1_per$GENERATION_RATIO_STATE_SOURCE, na.rm = TRUE) > max(heatMapDataState2_per$GENERATION_RATIO_STATE_SOURCE, na.rm = TRUE),
                                max(heatMapDataState1_per$GENERATION_RATIO_STATE_SOURCE, na.rm = TRUE),
                                max(heatMapDataState2_per$GENERATION_RATIO_STATE_SOURCE))
      
      
      #plot first state
      #line chart
      output$firstStateLineChart_per <- renderPlot ({
          ggplot(subset(comparisonTable1_per, (ENERGY_SOURCE %in% theSourceInputCom_per))) + 
            geom_line(aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER, color=ENERGY_SOURCE)) + 
            geom_point(data = comparisonTable1_per[which(comparisonTable1_per$YEAR == input$firstYearInput_per & comparisonTable1_per$ENERGY_SOURCE %in% theSourceInputCom_per),], aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER), colour = "#004a9f") + 
            scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
            scale_y_continuous(labels = scales::percent, limits=c(0, theLineYlim_per)) +
            labs(x="YEAR", y = "AMOUNT", colour = "ENERGY SOURCE")
      })
      
      #stack chart
      output$firstStateStackChart_per <- renderPlot({
          ggplot(subset(each_energy_per_year, STATE %in% theFirstStateInput_per), aes(alpha = YEAR == input$firstYearInput_per, fill=ENERGY_SOURCE, y=GENERATION, x=YEAR)) + 
            geom_bar(position="fill" , stat="identity") +
            scale_fill_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
            scale_y_continuous(labels = scales::percent) +
            scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = F) +
            labs(x="YEAR", y = "AMOUNT", fill = "ENERGY SOURCE")
      })
      
      #heat map
      output$firstStateHeatMap_per <- renderPlot({
        p_firstState <- plot_usmap(data = heatMapDataState1_per, values = "GENERATION_RATIO_STATE_SOURCE") + 
          theme(legend.position = "right")
        heatMapLegend(p_firstState, theHeatLegendLim, "percentage")
        
      })
      
      
      
      #plot second state
      #line chart
      output$secondStateLineChart_per <- renderPlot ({
        ggplot(subset(comparisonTable2_per, (ENERGY_SOURCE %in% theSourceInputCom_per))) + 
          geom_line(aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER, color=ENERGY_SOURCE)) + 
          geom_point(data = comparisonTable2_per[which(comparisonTable2_per$YEAR == input$secondYearInput_per & comparisonTable2_per$ENERGY_SOURCE %in% theSourceInputCom_per),], aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER), colour = "#004a9f") + 
          scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
          scale_y_continuous(labels = scales::percent, limits=c(0, theLineYlim_per)) +
          labs(x="YEAR", y = "AMOUNT", colour = "ENERGY SOURCE") 
      })
      
      #stack chart
      output$secondStateStackChart_per <- renderPlot({
          ggplot(subset(each_energy_per_year, STATE %in% theSecondStateInput_per), aes(alpha = YEAR == input$secondYearInput_per, fill=ENERGY_SOURCE, y=GENERATION, x=YEAR)) + 
            geom_bar(position="fill" , stat="identity") +
            scale_fill_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
            scale_y_continuous(labels = scales::percent) +
            scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = F) +
            labs(x="YEAR", y = "AMOUNT", fill = "ENERGY SOURCE")
      })
      
      #heat map
      output$secondStateHeatMap_per <- renderPlot({
        p_secondState_per <- plot_usmap(data = heatMapDataState2_per, values = "GENERATION_RATIO_STATE_SOURCE") + 
          theme(legend.position = "right")
          heatMapLegend(p_secondState_per, theHeatLegendLim, "percentage")
        
      })
    }
    
    
    
    
    
    
    heatMapLegend <- function(p_firstState, theHeatLegendLim, type) {
      if(input$energySourceInputCom == "All") {
        p_firstState + scale_fill_continuous(low = "white", high = "#004a9f", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Coal") {
        p_firstState + scale_fill_continuous(low = "white", high = "#9e0142", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Hydroelectric Conventional") {
        p_firstState + scale_fill_continuous(low = "white", high = "#d53e4f", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Natural Gas") {
        p_firstState + scale_fill_continuous(low = "white", high = "#f46d43", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Petroleum") {
        p_firstState + scale_fill_continuous(low = "white", high = "#fdae61", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Wind") {
        p_firstState + scale_fill_continuous(low = "white", high = "#de77ae", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Wood and Wood Derived Fuels") {
        p_firstState + scale_fill_continuous(low = "white", high = "#9970ab", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Nuclear") {
        p_firstState + scale_fill_continuous(low = "white", high = "#f46d43", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Other Biomass") {
        p_firstState + scale_fill_continuous(low = "white", high = "#1a9850", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Other Gases") {
        p_firstState + scale_fill_continuous(low = "white", high = "#66c2a5", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Pumped Storage") {
        p_firstState + scale_fill_continuous(low = "white", high = "#3288bd", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Geothermal") {
        p_firstState + scale_fill_continuous(low = "white", high = "#5e4fa2", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Other") {
        p_firstState + scale_fill_continuous(low = "white", high = "#40004b", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
      else if (input$energySourceInputCom == "Solar Thermal and Photovoltaic") {
        p_firstState + scale_fill_continuous(low = "white", high = "#762a83", name = "AMOUNT (Million)", label = ifelse(type == "amount", scales::comma, scales::percent), limits = c(0, theHeatLegendLim))
      }
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

