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
tempAnom <- read.csv("data/raw/temp_anomaly_800000_ybp.csv")

#----End of data import

#---- Clean code
# 


#---- temperature code

ggplot(data = tempAnom)+
  geom_line(aes(x = years_before_present, y = temp_anomaly_C))+
  scale_x_reverse()
  
