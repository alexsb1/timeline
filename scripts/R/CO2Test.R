# Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
# Script to test timelines with the temp_anomaly_800000_ybp.csv file.
# Temperature anomaly from the past 800,000 years.
# Look at Zachos curve. Find data to import into R.
#

# ----- set libraries ----library(ggplot2)
library(tidyverse)
#end of libraries

#---- set variables ----


# end of variables

#---- data import ----
CO2_ppm_800000 <- read.csv("data/raw/CO2_ppm_800000_ybp.csv")
phanerozoicCO2 <- read.csv("data/raw/phanerozoicCO2.csv")

#----End of data import

#---- Clean code

# Add new column for years before present
phanerozoicCO2$years_before_present <- phanerozoicCO2$Age_Ma * 1000000


#---- temperature code ----


ggplot()+
  geom_line(data = tempAnom, aes(x = years_before_present, y = temp_anomaly_C), colour = "black")+
  geom_line(data = phanerozoicCO2, aes(x = years_before_present, y = pCO2_probability_maximum), colour = "red")+
  scale_x_reverse()

#---- End of code ----