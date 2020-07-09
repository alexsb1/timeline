# Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
# This file aggregates a list of sources used in this Timeline project
#

library(tidyverse)

# Load Individual plots to import the dataframes used.
# ATTENTION - take care to avoid this overwriting newer dataframes with the same name.
source("scripts/IndividualPlots.R")

manualRefs <- c("LR04 stack, Lisiecki, L. E., and M. E. Raymo (2005), A Pliocene-Pleistocene stack of 57 globally distributed benthic d18O records, Paleoceanography,20, PA1003, doi:10.1029/2004PA001071.",
                "https://www.visualcapitalist.com/history-of-pandemics-deadliest/",
                "http://www.climatedata.info/"
) 


#Not yet included

# CO2_ppm_800000 #no reference column
# eonPlot #no reference column
# epochPlot #no reference column
# eraPlot #no reference column
# geoTimeScale #no reference column
# historicEvents #no reference column
# LR04 #no reference column
# pandemics #no reference column
# phanerozoicCO2 #no reference column
# supercontinents #no reference column
# volcanoes #no reference column
# worldPop #no reference column


# construct a single reference list
referenceList <-NULL

referenceList <- bondEvents$Reference %>% append(referenceList, .)
referenceList <- climateEvents$reference %>% append(referenceList, .)
referenceList <- historicTimePeriods$Reference %>% append(referenceList, .)
referenceList <- meteorites$Reference %>% append(referenceList, .)
referenceList <- milankovitch$Source %>% append(referenceList, .)
referenceList <- monarchs$reference %>% append(referenceList, .)
referenceList <- tempAnom$reference %>% append(referenceList, .)
referenceList <- manualRefs %>% append(referenceList, .)

referenceList <- referenceList %>% unique(.) %>% as.list(.)
