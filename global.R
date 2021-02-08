temp = list.files(pattern="*state.csv")
orgData <- lapply(temp, read.csv)
utility <- do.call(rbind, orgData)

names(utility)[3] <- c("TYPE_OF_PRODUCER") #change header's name
names(utility)[4] <- c("ENERGY_SOURCE")
names(utility)[5] <- c("GENERATION")

utility$GENERATION <- as.numeric(gsub(",", "", utility$GENERATION)) #convert generation from char to number
utility$GENERATION[utility$GENERATION < 0] <- 0
utility <- subset(utility, str_squish(utility$STATE) != "" & !is.null(utility$STATE))

producer_dist <- unique(utility$TYPE_OF_PRODUCER)
energySource_dist <- unique(subset(utility$ENERGY_SOURCE, utility$ENERGY_SOURCE != "Total")) #list all the type of producer
#energySource_dist = energySource_dist[order(unlist(energySource_dist),decreasing = FALSE)]


state_dist <- unique(utility$STATE)
year_dist <- unique(utility$YEAR)

each_energy_per_year <- subset(utility, ENERGY_SOURCE != "Total")
each_energy_per_year$GENERATION_SUM_PER_YEAR <- ave(each_energy_per_year$GENERATION, each_energy_per_year$YEAR, FUN=sum)
each_energy_per_year$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR <- ave(each_energy_per_year$GENERATION, each_energy_per_year$ENERGY_SOURCE, each_energy_per_year$YEAR, FUN=sum)

#statistic
statistic <- NULL
for (val in year_dist)
{
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Coal" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Hydroelectric Conventional" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Petroleum" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Natural Gas" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Wind" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Wood and Wood Derived Fuels" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Nuclear" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Other Biomass" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Other Gases" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Pumped Storage" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Geothermal" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Other" & YEAR == val), 1))
  statistic <- rbind(statistic, head(subset(each_energy_per_year, ENERGY_SOURCE == "Solar Thermal and Photovoltaic" & YEAR == val), 1))
}
statistic$STATE <- NULL
statistic$TYPE_OF_PRODUCER <- NULL
statistic$GENERATION <- NULL 
statistic$GENERATION_PERCENTAGE_IN_YEAR <- (statistic$GENERATION_SUM_BY_CAT_ENERGY_SOURCE_PER_YEAR/statistic$GENERATION_SUM_PER_YEAR)

theTable <- NULL


heatMapData <- subset(utility, ENERGY_SOURCE != "Total")
heatMapData$TYPE_OF_PRODUCER <- NULL
heatMapData <- subset(heatMapData, STATE != "US-TOTAL" &STATE!= "US-Total")
each_energy_per_year$GENERATION_SUM_PER_YEAR <- ave(each_energy_per_year$GENERATION, each_energy_per_year$YEAR, FUN=sum)

scale_fill_manual(values = c("red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "pink"), name = "text", labels = c('Total Electric Power Industry', 'Electric Generators, Electric Utilities', 'Combined Heat and Power, Industrial Power', 'Combined Heat and Power, Commercial Power', 'Electric Generators, Independent Power Producers', 'Combined Heat and Power, Electric Power'))
