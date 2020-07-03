# Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
# Script to test making a single timeline in ggplot to show key time periods
#
# Imports external data and prepares dataframes to plot.

#---- Citations 
#LR04 stack

# Lisiecki, L. E., and M. E. Raymo (2005), A Pliocene-Pleistocene stack of 57 globally distributed benthic d18O records, Paleoceanography,20, PA1003, doi:10.1029/2004PA001071.

# https://www.visualcapitalist.com/history-of-pandemics-deadliest/

# http://www.climatedata.info/

# Additional citations and references are given in individual data files.

# End of citations


# ----- set libraries ----
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggpubr)

#end of libraries





#---- data import ----
geoTimeScale <- read.csv(file="data/raw/geological_time_scale.csv", header = TRUE)
phanerozoicCO2 <- read.csv(file="data/raw/phanerozoicCO2.csv", header = TRUE)
CO2_ppm_800000 <- read.csv(file="data/raw/CO2_ppm_800000_ybp.csv", header = TRUE)
tempAnom <- read.csv(file="data/raw/temp_anomaly_800000_ybp.csv", header = TRUE)
historicEvents <- read.csv(file="data/raw/historicEvents.csv", header = TRUE)
monarchs <- read.csv(file="data/raw/monarchs.csv", header = TRUE)
meteorites <- read.csv(file="data/raw/meteorites.csv", header = TRUE)
prehistory <- read.csv(file="data/raw/prehistory.csv", header = TRUE)
historicTimePeriods <- read.csv(file="data/raw/historicTimePeriods.csv", header = TRUE)
LR04 <- read.csv(file="data/raw/LR04_benthic_stack.csv", header = TRUE)
volcanoes <- read.csv(file="data/raw/volcanoes.csv", header = TRUE)
supercontinents <- read.csv(file="data/raw/supercontinents.csv", header = TRUE)
worldPop <- read.csv(file="data/raw/worldPopulation.csv", header = TRUE)
pandemics <- read.csv(file="data/raw/pandemics.csv", header = TRUE)
climateEvents <- read.csv(file="data/raw/climate_events.csv", header = TRUE)
climateEventsYBP <- read.csv(file="data/raw/climate_events_ybp.csv", header = TRUE)
bondEvents <- read.csv(file="data/raw/Bond_events.csv", header = TRUE)
milankovitch <- read.csv(file="data/raw/Milankovitch.csv", header = TRUE)

# Note: more climate data is located in
# data/not used/From Climate Data

# end of data import

#---- set variables ----


thisYear <- year(today())
startYBP <- ymd("1950-01-01") #The commencement date for years before present (YBP) 


yearCEtoElapsedTime <- 4600000000 #this is the number of years between years in CE format and years elapsed from the earth's formation.

# end of variables


#---- Functions to convert to/from BCE/CE to years before present.

yearCEtoYearsElapsed <- function(yearCE, CE){ #the CE specifies if the year date is common era (CE/AD) or before common era (BCE/BC). Note that YearCE is used as an input for both CR and BCE years.
  ifelse(CE == "BCE",
    elapsedYear <- yearCEtoElapsedTime - thisYear - yearCE, #subtracts the number of years to account for being before common era (BCE/BC).
    ifelse(CE == "CE",
      elapsedYear <- yearCEtoElapsedTime - thisYear + yearCE,
      print("Date error.")
    )
  )
    return(elapsedYear)
}





yearsElapsedToYearCE <- function(yearsElapsed){
  yearCE <- NULL
  if(yearsElapsed <= yearCEtoYearsElapsed(0,"CE")) #If yearsElapsed is before (less than) 0 CE/AD then print CE era, else print BCE.
     (
       yearCE <- data.frame(
         year = yearCEtoElapsedTime - thisYear - yearsElapsed,
         era = "BCE"
       )
       ) #If yearsElapsed is less than 0 CE then print BCE
     else
       (
         yearCE <- data.frame(
           year = abs(yearCEtoElapsedTime - thisYear - yearsElapsed),
           era = "CE"
         )
       )
     return(yearCE)
}



# function to convert between years ago to years elapsed since the earth formation
yearsAgoToEapsedYears <- function(yearsAgo){
  elapsedYears <- yearCEtoElapsedTime - yearsAgo
  return(elapsedYears)
}








#End of functions.


#---- Adjust x-axis ----

xAxisMin <- 0 # 0 is the formation of the earth. In years.

xAxisMax <- today() %>% ymd(.) %>% decimal_date(.) %>% yearCEtoYearsElapsed(.,"CE") #convert into continious value for x axis time elapsed.

#End of adjusting x-axis

#---- Adjust y-axis ----

yAxisMin <- -800

yAxisMax <- 2000

#End of adjusting y-axis

#----Tidy data ----







#----ggplot prerequisits -----

# Make a simple blank timeline to control the x-axis and separation of layers. Is this necessary?
# Visualise each data set by adding a geom layer


#add extra columns to make a continious timeline of years ago and years elapsed from earth formation (t=0).
geoTimeScale$Start_years_ago <- geoTimeScale$Start_million_years_ago * 1000000
geoTimeScale$End_years_ago <- geoTimeScale$End_million_years_ago * 1000000
geoTimeScale$Start_elapsed_time <- geoTimeScale$Start_elapsed_time_million_years * 1000000
geoTimeScale$End_elapsed_time <- geoTimeScale$End_elapsed_time_million_years * 1000000


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


# End of making dataframes containing eron, era, epoch and age time periods.

# Phanerozoic CO2
# convert Age_Ma to millions of years ago, then use yearsAgoToElapsedYears function ready for plotting

phanerozoicCO2$yearsAgo <- phanerozoicCO2$Age_Ma * 1000000
phanerozoicCO2$yearsElapsed <- yearsAgoToEapsedYears(phanerozoicCO2$yearsAgo)

# End of phanerozoic CO2 processing

# CO2_ppm_800000
# convert from years_before present to yearsElapsed
CO2_ppm_800000$yearsElapsed <- yearsAgoToEapsedYears(CO2_ppm_800000$years_before_present)
# end of CO2_ppm_800000

# tempAnom
tempAnom$yearsElapsed <- yearsAgoToEapsedYears(tempAnom$years_before_present)
# end of tempAnom


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

# End of monarchs

# meteorites
# Calculates and adds a column for years elapsed since earth formation that will be used to plot on the timeline.


meteorites$yearsElapsed <- (meteorites$Age_Ma * 1000000) %>% yearsAgoToEapsedYears(.)

# end of meteorites

# prehistory
# Calculate yearsElapsed from Mya and save to a new column

prehistory$startYearsElapsed <- (prehistory$start_kya * 1000) %>% yearsAgoToEapsedYears(.)
prehistory$endYearsElapsed <- (prehistory$end_kya * 1000) %>% yearsAgoToEapsedYears(.)

# end of prehistory

# LR04 benthic stack
LR04$yearsElapsed <- (LR04$Time_ka * 1000) %>% yearsAgoToEapsedYears(.)



# End of LR04 benthic stack

# Volcanoes

# convert from Mya to years elapsed
volcanoes$yearsAgo <- volcanoes$Age_Mya * 1000000
volcanoes$yearsElapsed <- yearsAgoToEapsedYears(volcanoes$yearsAgo)

# end of volcanoes


# supercontinents
supercontinents$startElapsedYears <- (supercontinents$startAge_Mya * 1000000) %>% yearsAgoToEapsedYears(.)
supercontinents$endElapsedYears <- (supercontinents$endAge_Mya * 1000000) %>% yearsAgoToEapsedYears(.)

# end supercontinents


# world population

worldPop$yearsElapsed <- yearCEtoYearsElapsed(worldPop$Year, worldPop$Era)

# Investigate why the console gives "Date error" when this is successful.
# end world population


# pandemics
pandemics$startYearElapsed <- yearCEtoYearsElapsed(pandemics$startYear,"CE") #All the pandemic events in this file are CE.
pandemics$endYearElapsed <- yearCEtoYearsElapsed(pandemics$endYear,"CE")

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






# Milankovitch cycles
# Convert from years before present to ears elapsed

milankovitch$yearsElapsed <- yearsAgoToEapsedYears(milankovitch$yearsBP)


#----End of tidy data








# Prerequisits for ggplot
# start constructing a ggplot to visualise timeperiods as a timeline

colourList <- paste0("#", geoTimeScale$back_colour)

#End of ggplot prerequisists

#----ggplot timeline----
# Note that meteorites contains one NA observation, so will always cause a warning message when generating the ggplot timeline.

geoTimeTextcolour <- "black"


xAxisBreaks <- as.data.frame(xAxisBreaks)

plotGeoTimeScale <- ggplot() +
  scale_fill_manual(values = colourList) +
  
  geom_segment(data = geoTimeScale, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=-100, yend=-100, size=10), colour = colourList)+
  geom_text(aes(x = xAxisMin, y = -100, label = "Stage"), colour = geoTimeTextcolour)+
  geom_text_repel(data = geoTimeScale, aes(label=Age, x=(Start_elapsed_time + End_elapsed_time)/2, y = -100), max.overlaps = 3) +
  
  geom_segment(data = eonPlot, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=-500, yend=-500, size=10), colour = eonPlot$back_colour)+
  geom_text(aes(x = xAxisMin, y = -500, label = "Eon"), colour = geoTimeTextcolour)+
  geom_text(data = eonPlot, aes(label=Eon, x=(Start_elapsed_time + End_elapsed_time)/2, y = -500), max.overlaps = 3) +
  
  geom_segment(data = eraPlot, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=-400, yend=-400, size=10), colour = eraPlot$back_colour)+
  geom_text(aes(x = xAxisMin, y = -400, label = "Era"), colour = geoTimeTextcolour)+
  geom_text_repel(data = eraPlot, aes(label=Era, x=(Start_elapsed_time + End_elapsed_time)/2, y = -400), max.overlaps = 3) +
  
  geom_segment(data = periodPlot, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=-300, yend=-300, size=10), colour = periodPlot$back_colour)+
  geom_text(aes(x = xAxisMin, y = -300, label = "Period"), colour = geoTimeTextcolour)+
  geom_text_repel(data = periodPlot, aes(label=Period, x=(Start_elapsed_time + End_elapsed_time)/2, y = -300), max.overlaps = 3) +
  
  geom_segment(data = epochPlot, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=-200, yend=-200, size=10), colour = epochPlot$back_colour)+
  geom_text(aes(x = xAxisMin, y = -200, label = "Epoch"), colour = geoTimeTextcolour)+
  geom_text_repel(data = epochPlot, aes(label=Epoch, x=(Start_elapsed_time + End_elapsed_time)/2, y = -200), max.overlaps = 3) +
  
  geom_line(data = phanerozoicCO2, aes(x = yearsElapsed, y = pCO2_probability_maximum), colour = "red")+
  geom_text(aes(x = xAxisMin, y = 700, label = "Phanerozoic CO2 ppm"), colour = "red") +
  
  geom_line(data = CO2_ppm_800000, aes(x = yearsElapsed, y = CO2_ppm), colour = "orange")+
  geom_text(aes(x = xAxisMin, y = 300, label = "Quaternary CO2"), colour = "orange") +
  
  geom_line(data = tempAnom, aes(x = yearsElapsed, y = temp_anomaly_C *100), colour = "green")+
  geom_text(aes(x = xAxisMin, y = 0, label = "Temperature anomaly"), colour = "green") +
  
  geom_segment(data = monarchs, aes(x=startElapsedYears, xend=endElapsedYears, y=500, yend=500, size=10), colour = monarchs$houseColours)+
  geom_text(aes(x = xAxisMin, y = 500, label = "Ruling English monarch"), colour = geoTimeTextcolour) +
  geom_text_repel(data = monarchs, aes(label=monarchTitle, x=(startElapsedYears + endElapsedYears)/2, y = 500), max.overlaps = 3) +
  
  
  geom_point(data = meteorites, aes(x = yearsElapsed, y = 1000, size = Diameter_km), colour = "hotpink")+
  geom_text(aes(x = xAxisMin, y = 1000, label = "Meteorite impacts"), colour = "hotpink") +
  
  geom_segment(data = prehistory, aes(x=startYearsElapsed, xend=endYearsElapsed, y=-600, yend=-600, size=10, colour = Name))+
  geom_text(aes(x = xAxisMin, y = -600, label = "Prehistory"), colour = geoTimeTextcolour)+
  geom_text_repel(data = prehistory, aes(label=Name, x=(startYearsElapsed + endYearsElapsed)/2, y = -600), max.overlaps = 3) +
  
  geom_line(data = LR04, aes(x = yearsElapsed, y = Benthic_d18O_per.mil *100), colour = "brown")+
  geom_text(aes(x = xAxisMin, y = 100, label = "Benthic d18O"), colour = "brown")+
  
  geom_point(data = volcanoes, aes(x = yearsElapsed, y = 1200, size = Volume_km3), colour = "purple3")+
  geom_text(aes(x = xAxisMin, y = 1200, label = "Volcano eruptions"), colour = "purple3") +
  
  geom_segment(data = supercontinents, aes(x=startElapsedYears, xend=endElapsedYears, y=-700, yend=-700, size=10, colour = Supercontinent))+
  geom_text(aes(x = xAxisMin, y = -700, label = "Supercontinents"), colour = geoTimeTextcolour)+
  geom_text_repel(data = supercontinents, aes(label=Supercontinent, x=(startElapsedYears + endElapsedYears)/2, y = -700), max.overlaps = 3) +
  
  geom_segment(data = historicTimePeriods, aes(x=startYearsElapsed, xend=endYearsElapsed, y=-800, yend=-800, size=10, colour = Name))+
  geom_text(aes(x = xAxisMin, y = -800, label = "Historic time periods"), colour = geoTimeTextcolour)+
  geom_text_repel(data = historicTimePeriods, aes(label=Name, x=(startYearsElapsed + endYearsElapsed)/2, y = -800), max.overlaps = 3) +
  
  geom_line(data = worldPop, aes(x = yearsElapsed, y = WorldPopulation), colour = "blue")+
  geom_text(aes(x = xAxisMin, y = 1400, label = "World population"), colour = "blue")+
  
  geom_point(data = pandemics, aes(x = startYearElapsed, y = 1600, size = deathToll), colour = "darkolivegreen")+
  geom_text(aes(x = xAxisMin, y = 1600, label = "Pandemics"), colour = "darkolivegreen") +
  geom_text_repel(data = pandemics, aes(label=Name, x=startYearElapsed, y = 1600), max.overlaps = 3) +
  
  geom_line(data = milankovitch, aes(x = yearsElapsed, y = annual65N), colour = "seagreen")+
  geom_text(aes(x = xAxisMin, y = 200, label = "Solar insolation 65N"), colour = "seagreen")+
  
  
  

  scale_x_continuous(
    limits = c(xAxisMin, xAxisMax)#,
#    breaks = xAxisBreaks
  )+
  
  scale_y_continuous(
    limits = c(yAxisMin, yAxisMax)
  )+

  xlab("Years elapsed") +
#  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),  axis.ticks.y=element_blank())+
  theme(legend.position="none")#+
#  geom_text(aes(label=geoTimeScale$Age, x = (geoTimeScale$End_years_ago + (geoTimeScale$Start_years_ago - geoTimeScale$End_years_ago)/2), y = 0.6, angle=90), colour = geoTimeScale$text_colour)







#End of ggplot timeline----




# Make a second plot
# This is testing multi plots.
# Both plots should be assigned to a value and call ggpubr to display


p2 <- ggplot()+

  geom_line(data = milankovitch, aes(x = yearsElapsed, y = annual65N), colour = "seagreen")+
  geom_text(aes(x = xAxisMin, y = 200, label = "Solar insolation 65N"), colour = "seagreen")+

  scale_x_continuous(
    limits = c(xAxisMin, xAxisMax)#,
    #    breaks = xAxisBreaks
  )+
  
  scale_y_continuous(
    limits = c(yAxisMin, yAxisMax)
  )+
  
  xlab("Years elapsed") +
  
  theme(legend.position="none")


# End of second ggplot




# display the plot stacked vertically and aligned by x-axis (years elapsed)
ggarrange(p1, p2, ncol = 1, nrow = 2, align = "v", heights = c(2, 1))














# Notes for later

# These limits makes an visually interesting plot. Starts at Mesozoic and finished at today.
# xAxisMin <- xAxisMax - 300000000


# Converts elapsed years to BC/BCE friendly years.

#xAxisBreaks <- NULL
#for(i in seq(from = xAxisMin, to = xAxisMax, by = 10000)){
#  xAxisBreaks <- append(xAxisBreaks, yearsElapsedToYearCE(i))
#}

# interesting colour picker
# colour = colors()[1:nrow(prehistory)]


# This plot in years ago, rather than elapsed time
#  geom_segment(data = eonPlot, aes(x=Start_years_ago, xend=End_years_ago, y=0, yend=0), colour = eonPlot$back_colour, size=10, linetype = 1)
#  scale_y_continuous(limits = 0:4)+ #This causes an error with geological timeline blocks. Also, this won't help when plotting CO2 against time.
#  scale_x_reverse( #This makes the plot run time forwards. #Not necessary if using time elapsed from earth formation.
#    limits = c(maxlimit * 1000000, minlimit * 1000000), #This introduces min and max time period limits.
#    breaks = trunc(c(geoTimeScale$End_years_ago, geoTimeScale$Start_years_ago))
#  ) #+

# breaks = trunc(c(geoTimeScale$End_elapsed_time, geoTimeScale$Start_elapsed_time))

# scale_x_continuous( #This makes the plot run time forwards. #Not necessary if using time elapsed from earth formation.
#  limits = c(xAxisMin, xAxisMax)#, #This introduces min and max time period limits.
  #breaks = 
#)+