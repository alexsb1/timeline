# Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
# Script to test making a single timeline in ggplot to show key time periods
#
# Imports external data and prepares dataframes to plot.



# ----- set libraries ----
library(tidyverse)
library(lubridate)

#end of libraries






#---- data import ----
geoTimeScale <- read.csv(file="data/raw/geological_time_scale.csv", header = TRUE)
phanerozoicCO2 <- read.csv(file="data/raw/phanerozoicCO2.csv", header = TRUE)
CO2_ppm_800000 <- read.csv(file="data/raw/CO2_ppm_800000_ybp", header = TRUE)
tempAnom <- read.csv(file="data/raw/temp_anomaly_800000_ybp.csv", header = TRUE)
historicEvents <- read.csv(file="data/raw/historicEvents.csv", header = TRUE)
monarchs <- read.csv(file="data/raw/monarchs.csv", header = TRUE)

# end of data import

#---- set variables ----


thisYear <- year(today())
startYBP <- ymd("1950-01-01") #The commencement date for years before present (YBP) 


yearCEtoElapsedTime <- 4600000000 #this is the number of years between years in CE format and years elapsed from the earth's formation.

# end of variables


#---- Functions to convert to/from BCE/CE to years before present.

yearCEtoYBP <- function(yearCE, CE){ #the CE specifies if the year date is common era (CE/AD) or before common era (BCE/BC). Note that YearCE is used as an input for both CR and BCE years.
  ifelse(CE == "BCE",
    elapsedYear <- yearCEtoElapsedTime - thisYear - yearCE, #subtracts the number of years to account for being before common era (BCE/BC).
    ifelse(CE == "CE",
      elapsedYear <- yearCEtoElapsedTime - thisYear + yearCE,
      print("Date error.")
    )
  )
    return(elapsedYear)
}





YBPtoYearCE <- function(YBP){
  yearCE <- NULL
  if(YBP <= yearCEtoYBP(0,"CE")) #If YBP is before (less than) 0 CE/AD then print CE era, else print BCE.
     (
       yearCE <- data.frame(
         year = yearCEtoElapsedTime - thisYear - YBP,
         era = "BCE"
       )
       ) #If YBP is less than 0 CE then print BCE
     else
       (
         yearCE <- data.frame(
           year = abs(yearCEtoElapsedTime - thisYear - YBP),
           era = "CE"
         )
       )
     return(yearCE)
}



# function to convert between years ago to years elapsed since the earth formation
yearsAgoToEapsedyears <- function(yearsAgo){
  elapsedYears <- yearCEtoElapsedTime - yearsAgo
  return(elapsedYears)
}








#End of functions.


#---- Adjust x-axis ----

xAxisMin <- 0 # 0 is the formation of the earth. In years.
xAxisMax <- yearCEtoYBP(thisYear,"CE") #convert into continious value for x axis time elapsed.

#End of adjusting x-axis

#----Tidy data ----










#end of tidying data ----

#----ggplot prerequisits -----

# Make a simple blank timeline to control the x-axis and separation of layers.
# Visualise each data set by adding a geom layer

# Is this necessary?

#End f ggplot prerequisists

#----ggplot timeline----
# This plot is wrong as it contains a mix of years before present and elapsed time.


ggplot() +
  geom_segment(data = geoTimeScale, aes(x=Start_years_ago, xend=End_years_ago, y=4, yend=4), colour = colourList, size=10, linetype = 1)+
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
  geom_line(data = phanerozoicCO2, aes(x = years_elapsed, y = pCO2_probability_maximum), colour = "red")+
  xlab("Years ago")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),  axis.ticks.y=element_blank())+
  theme(legend.position="none")+
  geom_text(aes(label=geoTimeScale$Age, x = (geoTimeScale$End_years_ago + (geoTimeScale$Start_years_ago - geoTimeScale$End_years_ago)/2), y = 0.6, angle=90), colour = geoTimeScale$text_colour)







#End of ggplot timeline----

