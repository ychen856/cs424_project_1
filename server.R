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
            scale_color_manual(values = c("Coal"= "#004949", "Hydroelectric Conventional" = "#009292", "Natural Gas" = "#ff6db6", "Petroleum" = "#ffb6db", "Wind" = "#490092", "Wood and Wood Derived Fuels" = "#006ddb", "Nuclear" = "#b66dff", "Other Biomass" = "#6db6ff", "Other Gases" = "#ffff6d", "Pumped Storage" = "#920000", "Geothermal" = "#924900", "Other" = "#db6d00", "Solar Thermal and Photovoltaic" = "#24ff24")) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, NA), labels = scales::comma) + 
            labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE")
        })
        
        #stack chart
        output$stackChart <- renderPlot({
          ggplot(subset(each_energy_per_year, (ENERGY_SOURCE %in% input$energySourceInput)), aes(fill=ENERGY_SOURCE, y=GENERATION/1000000, x=YEAR)) +
            geom_bar(position="stack" , stat="identity") + 
            scale_y_continuous(labels = scales::comma) +
            scale_fill_manual(values = c("Coal"= "#004949", "Hydroelectric Conventional" = "#009292", "Natural Gas" = "#ff6db6", "Petroleum" = "#ffb6db", "Wind" = "#490092", "Wood and Wood Derived Fuels" = "#006ddb", "Nuclear" = "#b66dff", "Other Biomass" = "#6db6ff", "Other Gases" = "#ffff6d", "Pumped Storage" = "#920000", "Geothermal" = "#924900", "Other" = "#db6d00", "Solar Thermal and Photovoltaic" = "#24ff24")) +
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
        theLineYlim = ifelse(max(comparisonTable1$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, na.rm = TRUE) > max(comparisonTable2$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, na.rm = TRUE),
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
            scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim + 0.1*theLineYlim), labels = scales::comma) + 
            scale_x_continuous(limits = c(1990, 2019))
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
            scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim + 0.1*theLineYlim), labels = scales::comma) + 
            scale_x_continuous(limits = c(1990, 2019))
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
      
      if(input$energySourceInputCom_per == "All") {
          heatMapDataState1_per$GENERATION_RATIO_STATE_SOURCE<- NULL
          heatMapDataState1_per <- heatMapDataState1_per[!duplicated(heatMapDataState1_per),]
          heatMapDataState1_per$US_TOTAL <- ave(heatMapDataState1_per$GENERATION_SUM_PER_STATE, FUN=sum)
          heatMapDataState1_per$GENERATION_RATIO_STATE_SOURCE <- heatMapDataState1_per$GENERATION_SUM_PER_STATE/heatMapDataState1_per$US_TOTAL
        
          heatMapDataState2_per$GENERATION_RATIO_STATE_SOURCE<- NULL
          heatMapDataState2_per <- heatMapDataState2_per[!duplicated(heatMapDataState2_per),]
          heatMapDataState2_per$US_TOTAL <- ave(heatMapDataState2_per$GENERATION_SUM_PER_STATE, FUN=sum)
          heatMapDataState2_per$GENERATION_RATIO_STATE_SOURCE <- heatMapDataState2_per$GENERATION_SUM_PER_STATE/heatMapDataState2_per$US_TOTAL
      }
      #print(heatMapDataState2_per)
      
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
          if(input$energySourceInputCom_per == "All") {
            heatMapLegend(p_firstState, NA, "percentage")
          } 
          else {
            heatMapLegend(p_firstState, theHeatLegendLim, "percentage")
          }
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
        if(input$energySourceInputCom_per == "All") {
            heatMapLegend(p_secondState_per, NA, "percentage")
        } 
        else {
            heatMapLegend(p_secondState_per, theHeatLegendLim, "percentage")
        }
      })
    }
    
    
    
    ################# 5 interesting things ###################
    
    observeEvent(input$btn1, {
        energySource_int <- energySource_dist
        firstState_int <- state_dist
        firstYear_int <- 1990
        secondState_int <- state_dist
        secondYear_int <- 2019
        generatePlot_int(energySource_int, firstState_int, firstYear_int, secondState_int, secondYear_int , 1)
    })
    observeEvent(input$btn2, {
        energySource_int <- c("Natural Gas", "Coal")
        firstState_int <- state_dist
        firstYear_int <- 1990
        secondState_int <- state_dist
        secondYear_int <- 2019
        generatePlot_int(energySource_int, firstState_int, firstYear_int, secondState_int, secondYear_int, 2)
    })
    observeEvent(input$btn3, {
        energySource_int <- "Nuclear"
        firstState_int <- state_dist
        firstYear_int <- 1990
        secondState_int <- state_dist
        secondYear_int <- 2019
        generatePlot_int(energySource_int, firstState_int, firstYear_int, secondState_int, secondYear_int, 3)
    })
    observeEvent(input$btn4, {
        energySource_int <- "Wind"
        firstState_int <- state_dist
        firstYear_int <- 1990
        secondState_int <- state_dist
        secondYear_int <- 2019
        generatePlot_int(energySource_int, firstState_int, firstYear_int, secondState_int, secondYear_int, 4)
    })
    observeEvent(input$btn5, {
        energySource_int <- energySource_dist
        firstState_int <- state_dist
        firstYear_int <- 1990
        secondState_int <- "Texas"
        secondYear_int <- 2019
        generatePlot_int(energySource_int, firstState_int, firstYear_int, secondState_int, secondYear_int, 5)
    })
    
      #condition 3: year
      #state 1
    generatePlot_int <- function(energySource_int, firstState_int, firstYear_int, secondState_int, secondYear_int, opt) {   
      
      #amount chart data
      if(length(firstState_int) != 1) {
        comparisonTable1_int_amt <- each_energy_per_year
      }
      else {
        comparisonTable1_int_amt <- subset(each_energy_per_year, STATE == (state.abb[which(state.name == firstState_int)]))
      }
      
      if(length(secondState_int) != 1) {
        comparisonTable2_int_amt <- each_energy_per_year
      }
      else {
        comparisonTable2_int_amt <- subset(each_energy_per_year, STATE == (state.abb[which(state.name == secondState_int)]))
      }
      
      comparisonTable1_int_amt <- subset(comparisonTable1_int_amt, (ENERGY_SOURCE %in% energySource_int))  
      comparisonTable1_int_amt$GENERATION_SUM_PER_YEAR <- ave(comparisonTable1_int_amt$GENERATION, comparisonTable1_int_amt$YEAR, FUN=sum)
      comparisonTable1_int_amt$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR <- ave(comparisonTable1_int_amt$GENERATION, comparisonTable1_int_amt$ENERGY_SOURCE, comparisonTable1_int_amt$YEAR, FUN=sum)
      
      comparisonTable2_int_amt <- subset(comparisonTable2_int_amt, (ENERGY_SOURCE %in% energySource_int))  
      comparisonTable2_int_amt$GENERATION_SUM_PER_YEAR <- ave(comparisonTable2_int_amt$GENERATION, comparisonTable2_int_amt$YEAR, FUN=sum)
      comparisonTable2_int_amt$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR <- ave(comparisonTable2_int_amt$GENERATION, comparisonTable2_int_amt$ENERGY_SOURCE, comparisonTable2_int_amt$YEAR, FUN=sum)
      
      heatMapDataState1_int_amt <- subset(heatMapData, heatMapData$ENERGY_SOURCE %in% energySource_int & heatMapData$YEAR == firstYear_int)
      heatMapDataState1_int_amt$GENERATION_SUM_PER_STATE_Milli <- ave(heatMapDataState1_int_amt$GENERATION_SUM_PER_SOURCE_STATE_Milli, heatMapDataState1_int_amt$STATE, FUN=sum)
      heatMapDataState1_int_amt$ENERGY_SOURCE <- NULL
      heatMapDataState1_int_amt$GENERATION_SUM_PER_SOURCE_STATE <- NULL
      heatMapDataState1_int_amt$GENERATION_SUM_PER_SOURCE_STATE_Milli <- NULL
      heatMapDataState1_int_amt <- heatMapDataState1_int_amt[!duplicated(heatMapDataState1_int_amt),]
      
      heatMapDataState2_int_amt <- subset(heatMapData, heatMapData$ENERGY_SOURCE %in% energySource_int & heatMapData$YEAR == secondYear_int)
      heatMapDataState2_int_amt$GENERATION_SUM_PER_STATE_Milli <- ave(heatMapDataState2_int_amt$GENERATION_SUM_PER_SOURCE_STATE_Milli, heatMapDataState2_int_amt$STATE, FUN=sum)
      heatMapDataState2_int_amt$ENERGY_SOURCE <- NULL
      heatMapDataState2_int_amt$GENERATION_SUM_PER_SOURCE_STATE <- NULL
      heatMapDataState2_int_amt$GENERATION_SUM_PER_SOURCE_STATE_Milli <- NULL
      heatMapDataState2_int_amt <- heatMapDataState2_int_amt[!duplicated(heatMapDataState2_int_amt),]
      
      #calculate ylim
      theLineYlim_int_amt = ifelse(max(comparisonTable1_int_amt$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, na.rm = TRUE) > max(comparisonTable2_int_amt$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, na.rm = TRUE),
                           max(comparisonTable1_int_amt$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, na.rm = TRUE),
                           max(comparisonTable2_int_amt$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000))
      
      theStackYlim_int_amt = ifelse(max(comparisonTable1_int_amt$GENERATION_SUM_PER_YEAR/1000000, na.rm = TRUE) > max(comparisonTable2_int_amt$GENERATION_SUM_PER_YEAR/1000000, na.rm = TRUE),
                            max(comparisonTable1_int_amt$GENERATION_SUM_PER_YEAR/1000000, na.rm = TRUE),
                            max(comparisonTable2_int_amt$GENERATION_SUM_PER_YEAR/1000000))
      
      theHeatLegendLim_int_amt = ifelse(max(heatMapDataState1_int_amt$GENERATION_SUM_PER_STATE_Milli, na.rm = TRUE) > max(heatMapDataState2_int_amt$GENERATION_SUM_PER_STATE_Milli, na.rm = TRUE),
                                max(heatMapDataState1_int_amt$GENERATION_SUM_PER_STATE_Milli, na.rm = TRUE),
                                max(heatMapDataState2_int_amt$GENERATION_SUM_PER_STATE_Milli))
      
      #percentage chart data
      comparisonTable_int <- each_energy_per_year
      comparisonTable_int$GENERATION_SUM_PER_YEAR_STATE_PER <- ave(comparisonTable_int$GENERATION, comparisonTable_int$STATE, comparisonTable_int$YEAR, FUN=sum)
      comparisonTable_int$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER <- ave(comparisonTable_int$GENERATION, comparisonTable_int$STATE, comparisonTable_int$YEAR, comparisonTable_int$ENERGY_SOURCE, FUN=sum)
      
      comparisonTable_int$TYPE_OF_PRODUCER <- NULL
      comparisonTable_int$GENERATION <- NULL
      comparisonTable_int$GENERATION_SUM_PER_YEAR <- NULL
      comparisonTable_int$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR <- NULL
      
      if(length(firstState_int) == 1) {
        comparisonTable_int <- subset(comparisonTable_int, STATE %in% firstState_int)
        comparisonTable1_int <- comparisonTable_int[!duplicated(comparisonTable_int),]
      }
      else {
        comparisonTable1_int <- statistic
        comparisonTable1_int$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER <- comparisonTable1_int$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR
        comparisonTable1_int$GENERATION_SUM_PER_YEAR_STATE_PER <- comparisonTable1_int$GENERATION_SUM_PER_YEAR
      }
      
      if(length(secondState_int) == 1) {
        comparisonTable_int <- subset(comparisonTable_int, STATE %in% secondState_int)
        comparisonTable2_int <- comparisonTable_int[!duplicated(comparisonTable_int),]
      }
      else {
        comparisonTable2_int <- statistic
        comparisonTable2_int$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER <- comparisonTable2_int$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR
        comparisonTable2_int$GENERATION_SUM_PER_YEAR_STATE_PER <- comparisonTable2_int$GENERATION_SUM_PER_YEAR
      }
      
      
      heatMapDataState1_int <- subset(heatMapData, heatMapData$ENERGY_SOURCE %in% energySource_int & heatMapData$YEAR == firstYear_int)
      heatMapDataState1_int$GENERATION_SUM_PER_STATE_Milli <- ave(heatMapDataState1_int$GENERATION_SUM_PER_SOURCE_STATE_Milli, heatMapDataState1_int$STATE, FUN=sum)
      heatMapDataState1_int$ENERGY_SOURCE <- NULL
      heatMapDataState1_int$GENERATION_SUM_PER_SOURCE_STATE <- NULL
      heatMapDataState1_int$GENERATION_SUM_PER_SOURCE_STATE_Milli <- NULL
      heatMapDataState1_int <- heatMapDataState1_int[!duplicated(heatMapDataState1_int),]
      
      heatMapDataState2_int <- subset(heatMapData, heatMapData$ENERGY_SOURCE %in% energySource_int & heatMapData$YEAR == secondYear_int)
      heatMapDataState2_int$GENERATION_SUM_PER_STATE_Milli <- ave(heatMapDataState2_int$GENERATION_SUM_PER_SOURCE_STATE_Milli, heatMapDataState2_int$STATE, FUN=sum)
      heatMapDataState2_int$ENERGY_SOURCE <- NULL
      heatMapDataState2_int$GENERATION_SUM_PER_SOURCE_STATE <- NULL
      heatMapDataState2_int$GENERATION_SUM_PER_SOURCE_STATE_Milli <- NULL
      heatMapDataState2_int <- heatMapDataState2_int[!duplicated(heatMapDataState2_int),]
      
      if(length(energySource_int) != 1) {
        heatMapDataState1_int$GENERATION_RATIO_STATE_SOURCE<- NULL
        heatMapDataState1_int <- heatMapDataState1_int[!duplicated(heatMapDataState1_int),]
        heatMapDataState1_int$US_TOTAL <- ave(heatMapDataState1_int$GENERATION_SUM_PER_STATE, FUN=sum)
        heatMapDataState1_int$GENERATION_RATIO_STATE_SOURCE <- heatMapDataState1_int$GENERATION_SUM_PER_STATE/heatMapDataState1_int$US_TOTAL
        
        heatMapDataState2_int$GENERATION_RATIO_STATE_SOURCE<- NULL
        heatMapDataState2_int <- heatMapDataState2_int[!duplicated(heatMapDataState2_int),]
        heatMapDataState2_int$US_TOTAL <- ave(heatMapDataState2_int$GENERATION_SUM_PER_STATE, FUN=sum)
        heatMapDataState2_int$GENERATION_RATIO_STATE_SOURCE <- heatMapDataState2_int$GENERATION_SUM_PER_STATE/heatMapDataState2_int$US_TOTAL
      }
      
      #calculate ylim
      theLineYlim_int = ifelse(max(comparisonTable1_int$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/comparisonTable1_int$GENERATION_SUM_PER_YEAR_STATE_PER, na.rm = TRUE) > max(comparisonTable2_int$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/comparisonTable2_int$GENERATION_SUM_PER_YEAR_STATE_PER, na.rm = TRUE),
                               max(comparisonTable1_int$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/comparisonTable1_int$GENERATION_SUM_PER_YEAR_STATE_PER, na.rm = TRUE),
                               max(comparisonTable2_int$GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/comparisonTable2_int$GENERATION_SUM_PER_YEAR_STATE_PER))
      
      theHeatLegendLim_int = ifelse(max(heatMapDataState1_int$GENERATION_RATIO_STATE_SOURCE, na.rm = TRUE) > max(heatMapDataState2_int$GENERATION_RATIO_STATE_SOURCE, na.rm = TRUE),
                                max(heatMapDataState1_int$GENERATION_RATIO_STATE_SOURCE, na.rm = TRUE),
                                max(heatMapDataState2_int$GENERATION_RATIO_STATE_SOURCE))
      
      

        if(opt == 1) {
            output$interestingChart1_1 <- renderPlot({
              ggplot(subset(comparisonTable1_int, (ENERGY_SOURCE %in% energySource_int))) + 
                geom_line(aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER, color=ENERGY_SOURCE)) + 
                geom_point(data = comparisonTable1_int[which(comparisonTable1_int$YEAR == firstYear_int & comparisonTable1_int$ENERGY_SOURCE %in% energySource_int),], aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER), colour = "#004a9f") + 
                scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
                scale_y_continuous(labels = scales::percent, limits=c(0, theLineYlim_int)) +
                labs(x="YEAR", y = "AMOUNT", colour = "ENERGY SOURCE")
            })
            output$interestingChart1_2 <- renderPlot({
                p_firstState <- plot_usmap(data = heatMapDataState1_int, values = "GENERATION_RATIO_STATE_SOURCE") + 
                  theme(legend.position = "right")
              
                heatMapLegend(p_firstState, theHeatLegendLim_int, "percentage")
            })
            output$interestingChart2_1 <- renderPlot({
                ggplot(subset(comparisonTable2_int, (ENERGY_SOURCE %in% energySource_int))) + 
                  geom_line(aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER, color=ENERGY_SOURCE)) + 
                  geom_point(data = comparisonTable2_int[which(comparisonTable2_int$YEAR == secondYear_int & comparisonTable2_int$ENERGY_SOURCE %in% energySource_int),], aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER), colour = "#004a9f") + 
                  scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
                  scale_y_continuous(labels = scales::percent, limits=c(0, theLineYlim_int)) +
                  labs(x="YEAR", y = "AMOUNT", colour = "ENERGY SOURCE") 
            })
            output$interestingChart2_2 <- renderPlot({
                p_secondState_int <- plot_usmap(data = heatMapDataState2_int, values = "GENERATION_RATIO_STATE_SOURCE") + 
                  theme(legend.position = "right")
              
                heatMapLegend(p_secondState_int, theHeatLegendLim_int, "percentage")
            })
        }
        if(opt == 2) {
          output$interestingChart1_1 <- renderPlot({
            ggplot(comparisonTable1_int_amt) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable1_int_amt[which(comparisonTable1_int_amt$YEAR == firstYear_int),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim_int_amt + 0.1*theLineYlim_int_amt), labels = scales::comma) + 
              scale_x_continuous(limits = c(1990, 2019))
          })
          output$interestingChart1_2 <- renderPlot({
            ggplot(subset(comparisonTable1_int, (ENERGY_SOURCE %in% energySource_int))) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable1_int[which(comparisonTable1_int$YEAR == firstYear_int & comparisonTable1_int$ENERGY_SOURCE %in% energySource_int),], aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              scale_y_continuous(labels = scales::percent, limits=c(0, theLineYlim_int)) +
              labs(x="YEAR", y = "AMOUNT", colour = "ENERGY SOURCE")
          })
          output$interestingChart2_1 <- renderPlot({
            ggplot(comparisonTable2_int_amt) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable2_int_amt[which(comparisonTable2_int_amt$YEAR == secondYear_int),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim_int_amt + 0.1*theLineYlim_int_amt), labels = scales::comma) + 
              scale_x_continuous(limits = c(1990, 2019))
          })
          output$interestingChart2_2 <- renderPlot({
            ggplot(subset(comparisonTable2_int, (ENERGY_SOURCE %in% energySource_int))) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable2_int[which(comparisonTable2_int$YEAR == secondYear_int & comparisonTable2_int$ENERGY_SOURCE %in% energySource_int),], aes(x=YEAR, y=GENERATION_SUM_PER_YEAR_STATE_SOURCE_PER/GENERATION_SUM_PER_YEAR_STATE_PER), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              scale_y_continuous(labels = scales::percent, limits=c(0, theLineYlim_int)) +
              labs(x="YEAR", y = "AMOUNT", colour = "ENERGY SOURCE")
          })
        }
        if(opt == 3) {
          output$interestingChart1_1 <- renderPlot({
            ggplot(comparisonTable1_int_amt) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable1_int_amt[which(comparisonTable1_int_amt$YEAR == firstYear_int),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim_int_amt + 0.1*theLineYlim_int_amt), labels = scales::comma) + 
              scale_x_continuous(limits = c(1990, 2019))
          })
          output$interestingChart1_2 <- renderPlot({
            p_secondState <- plot_usmap(data = heatMapDataState1_int_amt, values = "GENERATION_SUM_PER_STATE_Milli") + 
              theme(legend.position = "right")
            heatMapLegend(p_secondState, theHeatLegendLim_int_amt, "amount")
          })
          output$interestingChart2_1 <- renderPlot({
            ggplot(comparisonTable2_int_amt) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable2_int_amt[which(comparisonTable2_int_amt$YEAR == secondYear_int),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim_int_amt + 0.1*theLineYlim_int_amt), labels = scales::comma) + 
              scale_x_continuous(limits = c(1990, 2019))
          })
          output$interestingChart2_2 <- renderPlot({
              p_secondState <- plot_usmap(data = heatMapDataState2_int_amt, values = "GENERATION_SUM_PER_STATE_Milli") + 
                theme(legend.position = "right")
              heatMapLegend(p_secondState, theHeatLegendLim_int_amt, "amount")
          })
        }
        if(opt == 4) {
          output$interestingChart1_1 <- renderPlot({
            ggplot(comparisonTable1_int_amt) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable1_int_amt[which(comparisonTable1_int_amt$YEAR == firstYear_int),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim_int_amt + 0.1*theLineYlim_int_amt), labels = scales::comma) + 
              scale_x_continuous(limits = c(1990, 2019))
          })
          output$interestingChart1_2 <- renderPlot({
            p_secondState <- plot_usmap(data = heatMapDataState1_int_amt, values = "GENERATION_SUM_PER_STATE_Milli") + 
              theme(legend.position = "right")
            heatMapLegend(p_secondState, theHeatLegendLim_int_amt, "amount")
          })
          output$interestingChart2_1 <- renderPlot({
            ggplot(comparisonTable2_int_amt) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable2_int_amt[which(comparisonTable2_int_amt$YEAR == secondYear_int),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim_int_amt + 0.1*theLineYlim_int_amt), labels = scales::comma) + 
              scale_x_continuous(limits = c(1990, 2019))
          })
          output$interestingChart2_2 <- renderPlot({
            p_secondState <- plot_usmap(data = heatMapDataState2_int_amt, values = "GENERATION_SUM_PER_STATE_Milli") + 
              theme(legend.position = "right")
            heatMapLegend(p_secondState, theHeatLegendLim_int_amt, "amount")
          })
        }
        if(opt == 5) {
          output$interestingChart1_1 <- renderPlot({
            ggplot(comparisonTable1_int_amt) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable1_int_amt[which(comparisonTable1_int_amt$YEAR == firstYear_int),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim_int_amt + 0.1*theLineYlim_int_amt), labels = scales::comma) + 
              scale_x_continuous(limits = c(1990, 2019))
          })
          output$interestingChart1_2 <- renderPlot({
            ggplot(comparisonTable1_int_amt, aes(fill=ENERGY_SOURCE, y=GENERATION/1000000, x=YEAR)) +
              geom_bar(aes(alpha = YEAR == firstYear_int), position="stack" , stat="identity") + 
              scale_fill_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", fill = "ENERGY SOURCE") +
              scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = F) +
              lims(y=c(0, theStackYlim_int_amt)) +
              scale_y_continuous(labels = scales::comma)
          })
          output$interestingChart2_1 <- renderPlot({
            ggplot(comparisonTable2_int_amt) + 
              geom_line(aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000, color=ENERGY_SOURCE)) + 
              geom_point(data = comparisonTable2_int_amt[which(comparisonTable2_int_amt$YEAR == secondYear_int),], aes(x=YEAR, y=GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/1000000), colour = "#004a9f") + 
              scale_color_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", colour = "ENERGY SOURCE") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, theLineYlim_int_amt + 0.1*theLineYlim_int_amt), labels = scales::comma) + 
              scale_x_continuous(limits = c(1990, 2019))
          })
          output$interestingChart2_2 <- renderPlot({
            ggplot(comparisonTable2_int_amt, aes(fill=ENERGY_SOURCE, y=GENERATION/1000000, x=YEAR)) +
              geom_bar(aes(alpha = YEAR == secondYear_int), position="stack" , stat="identity") + 
              scale_fill_manual(values = c("Coal"= "#9e0142", "Hydroelectric Conventional" = "#d53e4f", "Natural Gas" = "#f46d43", "Petroleum" = "#fdae61", "Wind" = "#de77ae", "Wood and Wood Derived Fuels" = "#9970ab", "Nuclear" = "#f46d43", "Other Biomass" = "#1a9850", "Other Gases" = "#66c2a5", "Pumped Storage" = "#3288bd", "Geothermal" = "#5e4fa2", "Other" = "#40004b", "Solar Thermal and Photovoltaic" = "#762a83")) +
              labs(x="YEAR", y = "AMOUNT (Million)", fill = "ENERGY SOURCE") +
              scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = F) +
              lims(y=c(0, theStackYlim_int_amt)) +
              scale_y_continuous(labels = scales::comma)
          })
          output$discription <- renderText({ 
            "FFFFFFFFFFFFFFFFFFFFF"
          })
      }
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

