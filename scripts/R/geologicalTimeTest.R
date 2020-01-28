# Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
# Script to test timelines with the geoTimeScale.csv file.
# geological time scale
#

# ----- set libraries ----library(ggplot2)
library(tidyverse)
#end of libraries

#---- set variables ----


# end of variables

#---- data import ----

geoTimeScale <- read.csv("data/raw/geological_time_scale.csv")

# end of data import

#add extra columns to make a continious timeline of years ago and years elapsed from earth formation (t=0).
geoTimeScale$Start_years_ago <- geoTimeScale$Start_million_years_ago * 1000000
geoTimeScale$End_years_ago <- geoTimeScale$End_million_years_ago * 1000000
geoTimeScale$Start_elapsed_time <- geoTimeScale$Start_elapsed_time_million_years * 1000000
geoTimeScale$End_elapsed_time <- geoTimeScale$End_elapsed_time_million_years * 1000000


#---- timeline code ----


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
  periodPlot$Era <- append(periodPlot$Era, k)
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
  epochPlot$Era <- append(epochPlot$Era, l)
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

# Prerequisits for ggplot
# start constructing a ggplot to visualise timeperiods as a timeline

colourList <- paste0("#", geoTimeScale$back_colour)

minlimit <- min(geoTimeScale$End_million_years_ago)
maxlimit <- max(geoTimeScale$Start_million_years_ago)

# minlimit <- 2000 #manual override
# maxlimit <- 4500 #manual override



ggplot(data = geoTimeScale) +
  geom_segment(aes(x=Start_million_years_ago, xend=End_million_years_ago, y=4, yend=4), colour = colourList, size=10, linetype = 1)+
  scale_fill_manual(values = colourList)+
  geom_segment(data = eonPlot, aes(x=Start_million_years_ago, xend=End_million_years_ago, y=0, yend=0), colour = eonPlot$back_colour, size=10, linetype = 1)+
  geom_segment(data = eraPlot, aes(x=Start_million_years_ago, xend=End_million_years_ago, y=1, yend=1), colour = eraPlot$back_colour, size=10, linetype = 1)+
  geom_segment(data = periodPlot, aes(x=Start_million_years_ago, xend=End_million_years_ago, y=2, yend=2), colour = periodPlot$back_colour, size=10, linetype = 1)+
  geom_segment(data = epochPlot, aes(x=Start_million_years_ago, xend=End_million_years_ago, y=3, yend=3), colour = epochPlot$back_colour, size=10, linetype = 1)+
#  scale_y_continuous(limits = 0:4)+ #This causes an error with geological timeline blocks. Also, this won't help when plotting CO2 against time.
  scale_x_reverse( #This makes the plot run time forwards.
    limits = c(maxlimit, minlimit), #This introduces min and max time period limits.
    breaks = trunc(c(geoTimeScale$End_million_years_ago, geoTimeScale$Start_million_years_ago))
    )+
  xlab("Millions of years ago")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),  axis.ticks.y=element_blank())+
  theme(legend.position="none")+
  geom_text(aes(label=geoTimeScale$Age, x = (geoTimeScale$End_million_years_ago + (geoTimeScale$Start_million_years_ago - geoTimeScale$End_million_years_ago)/2), y = 0.6, angle=90), colour = geoTimeScale$text_colour)
  

# Same plot as above but with years before present

ggplot(data = geoTimeScale) +
  geom_segment(aes(x=Start_years_ago, xend=End_years_ago, y=4, yend=4), colour = colourList, size=10, linetype = 1)+
  scale_fill_manual(values = colourList)+
  geom_segment(data = eonPlot, aes(x=Start_years_ago, xend=End_years_ago, y=0, yend=0), colour = eonPlot$back_colour, size=10, linetype = 1)+
  geom_segment(data = eraPlot, aes(x=Start_years_ago, xend=End_years_ago, y=1, yend=1), colour = eraPlot$back_colour, size=10, linetype = 1)+
  geom_segment(data = periodPlot, aes(x=Start_years_ago, xend=End_years_ago, y=2, yend=2), colour = periodPlot$back_colour, size=10, linetype = 1)+
  geom_segment(data = epochPlot, aes(x=Start_years_ago, xend=End_years_ago, y=3, yend=3), colour = epochPlot$back_colour, size=10, linetype = 1)+
  #  scale_y_continuous(limits = 0:4)+ #This causes an error with geological timeline blocks. Also, this won't help when plotting CO2 against time.
  scale_x_reverse( #This makes the plot run time forwards.
    limits = c(maxlimit * 1000000, minlimit * 1000000), #This introduces min and max time period limits.
    breaks = trunc(c(geoTimeScale$End_years_ago, geoTimeScale$Start_years_ago))
  )+
  xlab("Years ago")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),  axis.ticks.y=element_blank())+
  theme(legend.position="none")+
  geom_text(aes(label=geoTimeScale$Age, x = (geoTimeScale$End_years_ago + (geoTimeScale$Start_years_ago - geoTimeScale$End_years_ago)/2), y = 0.6, angle=90), colour = geoTimeScale$text_colour)

               








#---- End of timeline code ----

