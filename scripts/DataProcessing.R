# Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
# This script imports raw data files and processes the data into a standardised format in preparation for plotting on a timeline.
#
# It's likely this file is only run when new data is added.
# The processed data is stored in ./processed_data


#---- Citations for data

# LR04 stack
# Lisiecki, L. E., and M. E. Raymo (2005), A Pliocene-Pleistocene stack of 57 globally distributed benthic d18O records, Paleoceanography,20, PA1003, doi:10.1029/2004PA001071.

# https://www.visualcapitalist.com/history-of-pandemics-deadliest/

# http://www.climatedata.info/

# Additional citations and references are given in individual data files.

# End of citations


#----- set libraries ----
library(tidyverse)
library(lubridate)


#end of libraries



#----Functions ----
# Link to file containing custom functions required to process the data
source("scripts/Functions.R")


# End of function links


#---- data import ----
geoTimeScale <- read.csv(file="raw_data/geological_time_scale.csv", header = TRUE)
phanerozoicCO2 <- read.csv(file="raw_data/phanerozoicCO2.csv", header = TRUE)
CO2_ppm_800000 <- read.csv(file="raw_data/CO2_ppm_800000_ybp.csv", header = TRUE)
tempAnom <- read.csv(file="raw_data/temp_anomaly_800000_ybp.csv", header = TRUE)
historicEvents <- read.csv(file="raw_data/historicEvents.csv", header = TRUE)
monarchs <- read.csv(file="raw_data/monarchs.csv", header = TRUE)
meteorites <- read.csv(file="raw_data/meteorites.csv", header = TRUE)
prehistory <- read.csv(file="raw_data/prehistory.csv", header = TRUE)
historicTimePeriods <- read.csv(file="raw_data/historicTimePeriods.csv", header = TRUE)
LR04 <- read.csv(file="raw_data/LR04_benthic_stack.csv", header = TRUE)
volcanoes <- read.csv(file="raw_data/volcanoes.csv", header = TRUE)
supercontinents <- read.csv(file="raw_data/supercontinents.csv", header = TRUE)
worldPop <- read.csv(file="raw_data/worldPopulation.csv", header = TRUE)
pandemics <- read.csv(file="raw_data/pandemics.csv", header = TRUE)
climateEvents <- read.csv(file="raw_data/climate_events.csv", header = TRUE)
climateEventsYBP <- read.csv(file="raw_data/climate_events_ybp.csv", header = TRUE)
bondEvents <- read.csv(file="raw_data/Bond_events.csv", header = TRUE)
milankovitch <- read.csv(file="raw_data/Milankovitch.csv", header = TRUE)

# Note: more climate data is located in
# data/not used/From Climate Data

# end of data import

#---- set variables ----
# The following variables are stored in Functions.R.

# thisYear
# startYBP
# yearCEtoElapsedTime

# end of variables





#---- start of processing data ----

# Make a simple blank timeline to control the x-axis and separation of layers. Is this necessary?
# Visualise each data set by adding a geom layer


#add extra columns to make a continuous timeline of years ago and years elapsed from earth formation (t=0).
geoTimeScale$Start_years_ago <- geoTimeScale$Start_million_years_ago * 1000000
geoTimeScale$End_years_ago <- geoTimeScale$End_million_years_ago * 1000000
geoTimeScale$Start_elapsed_time <- yearsAgoToEapsedYears(geoTimeScale$Start_years_ago)
geoTimeScale$End_elapsed_time <- yearsAgoToEapsedYears(geoTimeScale$End_years_ago)

write.csv(geoTimeScale, file = "processed_data/geoTimeScale.csv", row.names=FALSE)



# Makes a new dataframe containing only the eons
eonList <- select(geoTimeScale, Eon, Start_million_years_ago, End_million_years_ago, Start_years_ago, End_years_ago, Start_elapsed_time, End_elapsed_time, back_colour)
uniqueEonList <- unique(eonList$Eon)
eonPlot <- NULL #This empties this variable and makes it ready for the following for loop to append to.

for(i in uniqueEonList){
  x <- eonList[which(eonList$Eon == i),]
  eonPlot$Eon <- append(eonPlot$Eon, i)
  eonPlot$Start_million_years_ago <- append(eonPlot$Start_million_years_ago, max(x$Start_million_years_ago))
  eonPlot$End_million_years_ago <- append(eonPlot$End_million_years_ago, min(x$End_million_years_ago))
  eonPlot$Start_years_ago <- append(eonPlot$Start_years_ago, max(x$Start_years_ago))
  eonPlot$End_years_ago <- append(eonPlot$End_years_ago, min(x$End_years_ago))
  eonPlot$Start_elapsed_time <- append(eonPlot$Start_elapsed_time, min(x$Start_elapsed_time))
  eonPlot$End_elapsed_time <- append(eonPlot$End_elapsed_time, max(x$End_elapsed_time))
  eonPlot$back_colour <- append(eonPlot$back_colour,
                                paste0("#",
                                       eonList[which(eonList$Start_million_years_ago == max(x$Start_million_years_ago)),8])
  )
}

eonPlot <- as.data.frame(eonPlot)
print(eonPlot)
write.csv(eonPlot, file = "processed_data/eonPlot.csv", row.names=FALSE)


# Makes a new dataframe containing only the eras
eraList <- select(geoTimeScale, Era, Start_million_years_ago, End_million_years_ago, Start_years_ago, End_years_ago, Start_elapsed_time, End_elapsed_time, back_colour)
uniqueEraList <- unique(eraList$Era)
eraPlot <- NULL #This empties this variable and makes it ready for the following for loop to append to.

for(j in uniqueEraList){
  x <- eraList[which(eraList$Era == j),]
  eraPlot$Era <- append(eraPlot$Era, j)
  eraPlot$End_million_years_ago <- append(eraPlot$End_million_years_ago, min(x$End_million_years_ago))
  eraPlot$Start_million_years_ago <- append(eraPlot$Start_million_years_ago, max(x$Start_million_years_ago))
  eraPlot$Start_years_ago <- append(eraPlot$Start_years_ago, max(x$Start_years_ago))
  eraPlot$End_years_ago <- append(eraPlot$End_years_ago, min(x$End_years_ago))
  eraPlot$Start_elapsed_time <- append(eraPlot$Start_elapsed_time, min(x$Start_elapsed_time))
  eraPlot$End_elapsed_time <- append(eraPlot$End_elapsed_time, max(x$End_elapsed_time))
  eraPlot$back_colour <- append(eraPlot$back_colour,
                                paste0("#",
                                       eraList[which(eraList$Start_million_years_ago == max(x$Start_million_years_ago)),8])
  )
}

eraPlot <- as.data.frame(eraPlot)
print(eraPlot)
write.csv(eraPlot, file = "processed_data/eraPlot.csv", row.names=FALSE)



# Makes a new dataframe containing only the period
periodList <- select(geoTimeScale, Period, Start_million_years_ago, End_million_years_ago, Start_years_ago, End_years_ago, Start_elapsed_time, End_elapsed_time, back_colour)
uniquePeriodList <- unique(periodList$Period)
periodPlot <- NULL #This empties this variable and makes it ready for the following for loop to append to.

for(k in uniquePeriodList){
  x <- periodList[which(periodList$Period == k),]
  periodPlot$Period <- append(periodPlot$Period, k)
  periodPlot$End_million_years_ago <- append(periodPlot$End_million_years_ago, min(x$End_million_years_ago))
  periodPlot$Start_million_years_ago <- append(periodPlot$Start_million_years_ago, max(x$Start_million_years_ago))
  periodPlot$Start_years_ago <- append(periodPlot$Start_years_ago, max(x$Start_years_ago))
  periodPlot$End_years_ago <- append(periodPlot$End_years_ago, min(x$End_years_ago))
  periodPlot$Start_elapsed_time <- append(periodPlot$Start_elapsed_time, min(x$Start_elapsed_time))
  periodPlot$End_elapsed_time <- append(periodPlot$End_elapsed_time, max(x$End_elapsed_time))
  periodPlot$back_colour <- append(periodPlot$back_colour,
                                   paste0("#",
                                          periodList[which(periodList$Start_million_years_ago == max(x$Start_million_years_ago)),8])
  )
}

periodPlot <- as.data.frame(periodPlot)
print(periodPlot)
write.csv(periodPlot, file = "processed_data/periodPlot.csv", row.names=FALSE)




# Makes a new dataframe containing only the period
epochList <- select(geoTimeScale, Epoch, Start_million_years_ago, End_million_years_ago, Start_years_ago, End_years_ago, Start_elapsed_time, End_elapsed_time, back_colour)
uniqueEpochList <- unique(epochList$Epoch)
epochPlot <- NULL #This empties this variable and makes it ready for the following for loop to append to.

for(l in uniqueEpochList){
  x <- epochList[which(epochList$Epoch == l),]
  epochPlot$Epoch <- append(epochPlot$Epoch, l)
  epochPlot$End_million_years_ago <- append(epochPlot$End_million_years_ago, min(x$End_million_years_ago))
  epochPlot$Start_million_years_ago <- append(epochPlot$Start_million_years_ago, max(x$Start_million_years_ago))
  epochPlot$Start_years_ago <- append(epochPlot$Start_years_ago, max(x$Start_years_ago))
  epochPlot$End_years_ago <- append(epochPlot$End_years_ago, min(x$End_years_ago))
  epochPlot$Start_elapsed_time <- append(epochPlot$Start_elapsed_time, min(x$Start_elapsed_time))
  epochPlot$End_elapsed_time <- append(epochPlot$End_elapsed_time, max(x$End_elapsed_time))
  epochPlot$back_colour <- append(epochPlot$back_colour,
                                  paste0("#",
                                         epochList[which(epochList$Start_million_years_ago == max(x$Start_million_years_ago)),8])
  )
}

epochPlot <- as.data.frame(epochPlot)
print(epochPlot)
write.csv(epochPlot, file = "processed_data/epochPlot.csv", row.names=FALSE)


# End of making dataframes containing eon, era, epoch and age time periods.

# Phanerozoic CO2
# convert Age_Ma to millions of years ago, then use yearsAgoToElapsedYears function ready for plotting

phanerozoicCO2$yearsAgo <- phanerozoicCO2$Age_Ma * 1000000
phanerozoicCO2$yearsElapsed <- yearsAgoToEapsedYears(phanerozoicCO2$yearsAgo)
write.csv(phanerozoicCO2, file = "processed_data/phanerozoicCO2.csv", row.names=FALSE)


# End of phanerozoic CO2 processing

# CO2_ppm_800000
# convert from years_before present to yearsElapsed
CO2_ppm_800000$yearsElapsed <- yearsAgoToEapsedYears(CO2_ppm_800000$years_before_present)
write.csv(CO2_ppm_800000, file = "processed_data/CO2_ppm_800000.csv", row.names=FALSE)

# end of CO2_ppm_800000

# tempAnom
tempAnom$yearsElapsed <- yearsAgoToEapsedYears(tempAnom$years_before_present)
write.csv(tempAnom, file = "processed_data/tempAnom.csv", row.names=FALSE)

# end of tempAnom



# Historic events
write.csv(historicEvents, file = "processed_data/historicEvents.csv", row.names=FALSE)

# end of historic events




# monarchs
# Adds a leading zero for lubridate to work
# Todo is to add support for elapsedYears that will be used for plotting on the timeline.
# Adjust variables in this section to make unique for monarchs.

monarch_start_ymd <- NULL

for(i in 1:nrow(monarchs)){
  if(nchar(monarchs$reignStartYear[i]) == 3){
    monarch_start_ymd <- append(monarch_start_ymd, paste0("0", monarchs$reignStartYear[i], "-", monarchs$reignStartMonth[i], "-", monarchs$reignStartDay[i]))
  }
  else{
    monarch_start_ymd <- append(monarch_start_ymd, paste0(monarchs$reignStartYear[i], "-", monarchs$reignStartMonth[i], "-", monarchs$reignStartDay[i]))
  }
}

monarchs$start_ymd <- monarch_start_ymd %>% ymd(.) %>% decimal_date(.) #saves as year decimal

#Repeat for end years.
monarch_end_ymd <- NULL

for(i in 1:nrow(monarchs)){
  if(nchar(monarchs$reignEndYear[i]) == 3){
    monarch_end_ymd <- append(monarch_end_ymd, paste0("0", monarchs$reignEndYear[i], "-", monarchs$reignEndMonth[i], "-", monarchs$reignEndDay[i]))
  }
  else{
    monarch_end_ymd <- append(monarch_end_ymd, paste0(monarchs$reignEndYear[i], "-", monarchs$reignEndMonth[i], "-", monarchs$reignEndDay[i]))
  }
}

monarchs$end_ymd <- monarch_end_ymd %>% ymd(.) %>% decimal_date(.) #saves as year decimal


#End of leading zero dates.

# convert date to yearsElapsed. Expect decimal values due to months.

monarchs$startElapsedYears <- yearCEtoYearsElapsed(monarchs$start_ymd,"CE")
monarchs$endElapsedYears <- yearCEtoYearsElapsed(monarchs$end_ymd,"CE")

write.csv(monarchs, file = "processed_data/monarchs.csv", row.names=FALSE)


# End of monarchs

# meteorites
# Calculates and adds a column for years elapsed since earth formation that will be used to plot on the timeline.


meteorites$yearsElapsed <- (meteorites$Age_Ma * 1000000) %>% yearsAgoToEapsedYears(.)
write.csv(meteorites, file = "processed_data/meteorites.csv", row.names=FALSE)


# end of meteorites

# prehistory
# Calculate yearsElapsed from Mya and save to a new column

prehistory$startYearsElapsed <- (prehistory$start_kya * 1000) %>% yearsAgoToEapsedYears(.)
prehistory$endYearsElapsed <- (prehistory$end_kya * 1000) %>% yearsAgoToEapsedYears(.)
write.csv(prehistory, file = "processed_data/prehistory.csv", row.names=FALSE)

# end of prehistory

# LR04 benthic stack
LR04$yearsElapsed <- (LR04$Time_ka * 1000) %>% yearsAgoToEapsedYears(.)
write.csv(LR04, file = "processed_data/LR04.csv", row.names=FALSE)



# End of LR04 benthic stack

# Volcanoes

# convert from Mya to years elapsed
volcanoes$yearsAgo <- volcanoes$Age_Mya * 1000000
volcanoes$yearsElapsed <- yearsAgoToEapsedYears(volcanoes$yearsAgo)
write.csv(volcanoes, file = "processed_data/volcanoes.csv", row.names=FALSE)

# end of volcanoes


# supercontinents
supercontinents$startElapsedYears <- (supercontinents$startAge_Mya * 1000000) %>% yearsAgoToEapsedYears(.)
supercontinents$endElapsedYears <- (supercontinents$endAge_Mya * 1000000) %>% yearsAgoToEapsedYears(.)
write.csv(supercontinents, file = "processed_data/supercontinents.csv", row.names=FALSE)

# end supercontinents


# world population
# Need to sue a for loop as a simplier input doesn't register the change from CE to BCE.

worldPopYearsElapsed <- NULL

for(i in 1:nrow(worldPop)){
  worldPopYearsElapsed <- yearCEtoYearsElapsed(worldPop$Year[i], worldPop$Era[i]) %>% append(worldPopYearsElapsed, .)
  }

worldPop$yearsElapsed <- worldPopYearsElapsed

write.csv(worldPop, file = "processed_data/worldPop.csv", row.names=FALSE)

# Investigate why the console gives "Date error" when this is successful.
# end world population


# pandemics
pandemics$startYearElapsed <- yearCEtoYearsElapsed(pandemics$startYear,"CE") #All the pandemic events in this file are CE.
pandemics$endYearElapsed <- yearCEtoYearsElapsed(pandemics$endYear,"CE")
write.csv(pandemics, file = "processed_data/pandemics.csv", row.names=FALSE)

# end of pandemics





# historicTimePeriods

# Calculate time elapsed for historic time periods.

historicTimePeriodsStart <- NULL

for(i in 1:nrow(historicTimePeriods)){
  historicTimePeriodsStart <- append(historicTimePeriodsStart, yearCEtoYearsElapsed(historicTimePeriods$start_year[i], historicTimePeriods$start_era[i]))
}
historicTimePeriods$startYearsElapsed <- historicTimePeriodsStart

historicTimePeriodsEnd <- NULL

for(i in 1:nrow(historicTimePeriods)){
  historicTimePeriodsEnd <- append(historicTimePeriodsEnd, yearCEtoYearsElapsed(historicTimePeriods$end_year[i], historicTimePeriods$end_era[i]))
}
historicTimePeriods$endYearsElapsed <- historicTimePeriodsEnd
write.csv(historicTimePeriods, file = "processed_data/historicTimePeriods.csv", row.names=FALSE)

# End of historic time periods



# climate events using BCE/CE notation

climateEvents$startYearsElapsed <- yearCEtoYearsElapsed(climateEvents$startYear,climateEvents$startEra)
climateEvents$endYearsElapsed <- yearCEtoYearsElapsed(climateEvents$endYear,climateEvents$endEra)



# End of climate events using BCE/BC notation


# Climate events using years before present notation
# Calculate yearsElapsed from ya and save to a new column

climateEventsYBP$startYearsElapsed <- (climateEventsYBP$startYearsAgo) %>% yearsAgoToEapsedYears(.)
climateEventsYBP$endYearsElapsed <- (climateEventsYBP$endYearsAgo) %>% yearsAgoToEapsedYears(.)

# end of events using years before present notation

# Merge into a single dataframe ready for plotting.
climateEvents <- merge(climateEvents, climateEventsYBP, all = TRUE)

# Remove now superfluous dataframe
rm(climateEventsYBP)

write.csv(climateEvents, file = "processed_data/climateEvents.csv", row.names=FALSE)

# end of climate events






# Bond events
bondEvents$yearsElapsed <- yearCEtoYearsElapsed(bondEvents$Year, bondEvents$Era)
write.csv(bondEvents, file = "processed_data/bondEvents.csv", row.names=FALSE)



# end of Bond events.






# Milankovitch cycles
# Convert from years before present to ears elapsed

milankovitch$yearsElapsed <- yearsAgoToEapsedYears(milankovitch$yearsBP)
write.csv(milankovitch, file = "processed_data/Milankovitch.csv", row.names=FALSE)

# end of Milankovitch cycles

#----End of processing data ----

