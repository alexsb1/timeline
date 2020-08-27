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

#It is necessary to manually update these values - otherwise they remain interactive (and return NA) on shinyapps.io!
paste("Shiny upload date", now())
paste("Git commit version", system("git rev-parse --short HEAD", intern = TRUE))
paste("Shiny bundleID", deployments(appPath = ".")[8] %>% as.numeric(.))
paste("Shiny upload", deployments(appPath = ".")[10] %>% as.numeric(.))



uploadDate <- NULL
uploadDate <- paste("Shiny upload date 2020-08-26 17:56:47")%>% append(uploadDate, .)
uploadDate <- paste("Git commit version 58944a5") %>% append(uploadDate, .)
uploadDate <- paste("Shiny bundleID 3556124") %>% append(uploadDate, .)
uploadDate <- paste("Shiny upload 1598368987.80569") %>% append(uploadDate, .)



manualRefs <- c("LR04 stack, Lisiecki, L. E., and M. E. Raymo (2005), A Pliocene-Pleistocene stack of 57 globally distributed benthic d18O records, Paleoceanography,20, PA1003, doi:10.1029/2004PA001071.",
                "https://www.visualcapitalist.com/history-of-pandemics-deadliest; date accessed 21 July 2020",
                "http://www.climatedata.info; date accessed 21 July 2020"
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
referenceList <- volcanoes$Reference %>% append(referenceList, .)
referenceList <- manualRefs %>% append(referenceList, .)
referenceList <- uploadDate %>% append(referenceList, .)

referenceList <- referenceList %>% unique(.) %>% as.list(.)

