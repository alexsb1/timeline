#----ggplot timeline----
# Note that meteorites contains one NA observation, so will always cause a warning message when generating the ggplot timeline.

# Load necessary libraries to make this a self sufficient script
library(tidyverse)
library(ggrepel)

source("scripts/Functions.R")


# This imports the dataframes
# ATTENTION

# All the data processing should have already been completed using DataProcessing.R
# Any import here should be ready to plot.
# Any data processing here is computed on the fly, so will affect user experience.

#start of data import

geoTimeScale <- read.csv(file="processed_data/geoTimeScale.csv", header = TRUE)
epochPlot <- read.csv(file="processed_data/epochPlot.csv", header = TRUE)
periodPlot <- read.csv(file="processed_data/periodPlot.csv", header = TRUE)
eraPlot <- read.csv(file="processed_data/eraPlot.csv", header = TRUE)
eonPlot <- read.csv(file="processed_data/eonPlot.csv", header = TRUE)
phanerozoicCO2 <- read.csv(file="processed_data/phanerozoicCO2.csv", header = TRUE)
CO2_ppm_800000 <- read.csv(file="processed_data/CO2_ppm_800000.csv", header = TRUE)
tempAnom <- read.csv(file="processed_data/tempAnom.csv", header = TRUE)
historicEvents <- read.csv(file="processed_data/historicEvents.csv", header = TRUE)
monarchs <- read.csv(file="processed_data/monarchs.csv", header = TRUE)
meteorites <- read.csv(file="processed_data/meteorites.csv", header = TRUE)
prehistory <- read.csv(file="processed_data/prehistory.csv", header = TRUE)
historicTimePeriods <- read.csv(file="processed_data/historicTimePeriods.csv", header = TRUE)
LR04 <- read.csv(file="processed_data/LR04.csv", header = TRUE)
volcanoes <- read.csv(file="processed_data/volcanoes.csv", header = TRUE)
supercontinents <- read.csv(file="processed_data/supercontinents.csv", header = TRUE)
worldPop <- read.csv(file="processed_data/worldPop.csv", header = TRUE)
pandemics <- read.csv(file="processed_data/pandemics.csv", header = TRUE)
climateEvents <- read.csv(file="processed_data/climateEvents.csv", header = TRUE)
bondEvents <- read.csv(file="processed_data/bondEvents.csv", header = TRUE)
milankovitch <- read.csv(file="processed_data/Milankovitch.csv", header = TRUE)


# end of data import


# Preset the x-axis so the timeline can load in full before giving the user the ability to change.

xAxisMin <- 0 # 0 is the formation of the earth. In years.

xAxisMax <- today() %>% ymd(.) %>% decimal_date(.) %>% yearCEtoYearsElapsed(.,"CE") #convert into continuous value for x axis time elapsed.

#End of adjusting x-axis


geoTimeTextcolour <- "black"


# xAxisBreaks <- NULL
colourList <- paste0("#", geoTimeScale$back_colour)



plotGeoTimescale <- ggplot() +
  scale_fill_manual(values = colourList) +
  
  geom_segment(data = geoTimeScale, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=40, yend=40, size=10), colour = colourList)+
  geom_text(aes(x = xAxisMin, y = 40, label = "Stage"), colour = geoTimeTextcolour)+
  geom_text_repel(data = geoTimeScale, aes(label=Age, x=(Start_elapsed_time + End_elapsed_time)/2, y = 40), max.overlaps = 3) +
  
  geom_segment(data = epochPlot, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=30, yend=30, size=10), colour = epochPlot$back_colour)+
  geom_text(aes(x = xAxisMin, y = 30, label = "Epoch"), colour = geoTimeTextcolour)+
  geom_text_repel(data = epochPlot, aes(label=Epoch, x=(Start_elapsed_time + End_elapsed_time)/2, y = 30), max.overlaps = 3) +

  geom_segment(data = periodPlot, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=20, yend=20, size=10), colour = periodPlot$back_colour)+
  geom_text(aes(x = xAxisMin, y = 20, label = "Period"), colour = geoTimeTextcolour)+
  geom_text_repel(data = periodPlot, aes(label=Period, x=(Start_elapsed_time + End_elapsed_time)/2, y = 20), max.overlaps = 3) +

  geom_segment(data = eraPlot, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=10, yend=10, size=10), colour = eraPlot$back_colour)+
  geom_text(aes(x = xAxisMin, y = 10, label = "Era"), colour = geoTimeTextcolour)+
  geom_text_repel(data = eraPlot, aes(label=Era, x=(Start_elapsed_time + End_elapsed_time)/2, y = 10), max.overlaps = 3) +
  
  geom_segment(data = eonPlot, aes(x=Start_elapsed_time, xend=End_elapsed_time, y=0, yend=0, size=10), colour = eonPlot$back_colour)+
  geom_text(aes(x = xAxisMin, y = 0, label = "Eon"), colour = geoTimeTextcolour)+
  geom_text(data = eonPlot, aes(label=Eon, x=(Start_elapsed_time + End_elapsed_time)/2, y = 0), max.overlaps = 3) +

  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
   )+
  
  xlab("Years elapsed") +
  
  theme(legend.position="none",
         axis.title.y = element_blank(), #y axis.
         axis.ticks.y = element_blank(), #remove y axis tick marks.
         axis.text.y = element_blank(), #remove y axis text and values.
         ) #end of theme modification.






plotCO2 <- ggplot()+
  geom_line(data = phanerozoicCO2, aes(x = yearsElapsed, y = pCO2_probability_maximum), colour = "red")+
#  geom_text(aes(x = xAxisMin, y = 1000, label = "Phanerozoic CO2 ppm"), colour = "red") + # The label causes issues with aligning the graphs because of the inclusion of xAxisMin.
  
  geom_line(data = CO2_ppm_800000, aes(x = yearsElapsed, y = CO2_ppm), colour = "orange") +
#  geom_text(aes(x = xAxisMin, y = 300, label = "Quaternary CO2"), colour = "orange") +
  
  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+

  # xlab("Years elapsed") +
    ylab("CO2 /ppm") + 
    theme(legend.position="none",
          axis.title.x = element_blank(), #remove x axis text
          axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
          )
  



plotTemp <- ggplot()+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_line(data = tempAnom, aes(x = yearsElapsed, y = temp_anomaly_C), colour = "green") +
#  geom_text(aes(x = xAxisMin, y = 0, label = "Temperature anomaly"), colour = "green") +
  
  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  ) + 
  
  # xlab("Years elapsed") +
  
  ylab("Temperature anom /C") + 
  theme(legend.position="none",
        axis.title.x = element_blank(), #remove x axis text
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  )





plotMonarchs <- ggplot()+
  geom_segment(data = monarchs, aes(x=startElapsedYears, xend=endElapsedYears, y=0, yend=0, size=10), colour = monarchs$houseColours)+
#  geom_text(aes(x = xAxisMin, y = 0, label = "Ruling English monarch"), colour = geoTimeTextcolour) +
  geom_text_repel(data = monarchs, aes(label=monarchTitle, x=(startElapsedYears + endElapsedYears)/2, y = 0), max.overlaps = 3) +
  
  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("Monarchs") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.ticks.y = element_blank(), #remove y axis tick marks.
        axis.text.y = element_blank(), #remove y axis text and values.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.



plotMeteorites <- ggplot()+
  geom_point(data = meteorites, aes(x = yearsElapsed, y = 0, size = Diameter_km), colour = "hotpink") +
#  geom_text(aes(x = xAxisMin, y = 0, label = "Meteorite impacts"), colour = "hotpink") +
  geom_text_repel(data = meteorites, aes(label=Name, x=yearsElapsed, y = 0), max.overlaps = 3) +
  
  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("Meteorites") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.ticks.y = element_blank(), #remove y axis tick marks.
        axis.text.y = element_blank(), #remove y axis text and values.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.




plotPrehistory <- ggplot()+
  geom_segment(data = prehistory, aes(x=startYearsElapsed, xend=endYearsElapsed, y=0, yend=0, size=10, colour = Name))+
#  geom_text(aes(x = xAxisMin, y = 0, label = "Prehistory"), colour = geoTimeTextcolour)+
  geom_text_repel(data = prehistory, aes(label=Name, x=(startYearsElapsed + endYearsElapsed)/2, y = 0), max.overlaps = 3) +

  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("Prehistory") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.ticks.y = element_blank(), #remove y axis tick marks.
        axis.text.y = element_blank(), #remove y axis text and values.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.




plotLR04 <- ggplot()+
  geom_line(data = LR04, aes(x = yearsElapsed, y = Benthic_d18O_per.mil), colour = "brown") +
#  geom_text(aes(x = xAxisMin, y = 3.5, label = "Benthic d18O"), colour = "brown")+
  
  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("LR04 d18O") +
  
#  labs(title ="LR04") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.






plotVolcanoes <- ggplot()+
  geom_point(data = volcanoes, aes(x = yearsElapsed, y = 0, size = Volume_km3), colour = "purple3") +
#  geom_text(aes(x = xAxisMin, y = 0, label = "Volcano eruptions"), colour = "purple3") +
  geom_text_repel(data = volcanoes, aes(label=Volcano, x=yearsElapsed, y = 0), max.overlaps = 3) +
  
  
  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("Volcano eruptions") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.ticks.y = element_blank(), #remove y axis tick marks.
        axis.text.y = element_blank(), #remove y axis text and values.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.

  #Add more detail using ggrepel such as size of eruption and name.





plotSupercontinents <- ggplot()+
  geom_segment(data = supercontinents, aes(x=startElapsedYears, xend=endElapsedYears, y=0, yend=0, size=10, colour = Supercontinent))+
#  geom_text(aes(x = xAxisMin, y = 0, label = "Supercontinents"), colour = geoTimeTextcolour)+
  geom_text_repel(data = supercontinents, aes(label=Supercontinent, x=(startElapsedYears + endElapsedYears)/2, y = 0), max.overlaps = 3) +

  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("Supercontinents") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.ticks.y = element_blank(), #remove y axis tick marks.
        axis.text.y = element_blank(), #remove y axis text and values.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.





plotHistoricTimePeriods <- ggplot()+
  geom_segment(data = historicTimePeriods, aes(x=startYearsElapsed, xend=endYearsElapsed, y=0, yend=0, size=10, colour = Name))+
#  geom_text(aes(x = xAxisMin, y=0, label = "Historic time periods"), colour = geoTimeTextcolour)+
  geom_text_repel(data = historicTimePeriods, aes(label=Name, x=(startYearsElapsed + endYearsElapsed)/2, y = 0), max.overlaps = 3) +
  
  scale_x_continuous( #force x-axis scale
  limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("Historic time periods") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.ticks.y = element_blank(), #remove y axis tick marks.
        axis.text.y = element_blank(), #remove y axis text and values.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.





plotWorldPop <- ggplot()+
  geom_line(data = worldPop, aes(x = yearsElapsed, y = WorldPopulation), colour = "blue") +
#  geom_text(aes(x = xAxisMin, y = 0, label = "World population"), colour = "blue") +
  
  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("Global population") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.





plotPandemics <- ggplot()+
  geom_point(data = pandemics, aes(x = startYearElapsed, y = 0, size = deathToll), colour = "darkolivegreen")+
#  geom_text(aes(x = xAxisMin, y = 0, label = "Pandemics"), colour = "darkolivegreen") +
  geom_text_repel(data = pandemics, aes(label=Name, x=startYearElapsed, y = 0), max.overlaps = 3) +
  
  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("Pandemics") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.ticks.y = element_blank(), #remove y axis tick marks.
        axis.text.y = element_blank(), #remove y axis text and values.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.




plotMilankovitch <- ggplot()+
  geom_line(data = milankovitch, aes(x = yearsElapsed, y = annual65N), colour = "seagreen") +
#  geom_text(aes(x = xAxisMin, y = 220, label = "Solar insolation 65N"), colour = "seagreen") +
  
  scale_x_continuous( #force x-axis scale
    limits = c(xAxisMin, xAxisMax)
  )+
  
  # xlab("Years elapsed") +
  ylab("65N") +
  
  theme(legend.position="none",
        axis.title.x = element_blank(), #both x and y axis.
        axis.text.x = element_blank() #remove timeline x-axis text. Currently still displaying x-axis tick marks.
  ) #end of theme modification.






#End of ggplot timelines----

