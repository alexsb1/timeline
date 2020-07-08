# Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
# Script to test timelines with the monarchs.csv file.
#

# ----- set libraries ----
library(tidyverse)
library(lubridate)

#end of libraries

#---- set variables ----
minlimit <- 1900
maxlimit <- 2020

# end of variables

#---- data import ----

monarchs <- read.csv(file="data/raw/monarchs.csv", header = TRUE)

# end of data import

#----Tidy data ----

# Adds a leading zero to three character years (i.e. 999 AD and earlier) so that Lubridate works later.
start_ymd <- NULL

for(i in 1:nrow(monarchs)){
  if(nchar(monarchs$reignStartYear[i]) == 3){
    start_ymd <- append(start_ymd, paste0("0", monarchs$reignStartYear[i], "-", monarchs$reignStartMonth[i], "-", monarchs$reignStartDay[i]))
  }
  else{
    start_ymd <- append(start_ymd, paste0(monarchs$reignStartYear[i], "-", monarchs$reignStartMonth[i], "-", monarchs$reignStartDay[i]))
  }
}

monarchs$start_ymd <- as.data.frame(start_ymd)

#Repeat for end years.
end_ymd <- NULL

for(i in 1:nrow(monarchs)){
  if(nchar(monarchs$reignEndYear[i]) == 3){
    end_ymd <- append(end_ymd, paste0("0", monarchs$reignEndYear[i], "-", monarchs$reignEndMonth[i], "-", monarchs$reignEndDay[i]))
  }
  else{
    end_ymd <- append(end_ymd, paste0(monarchs$reignEndYear[i], "-", monarchs$reignEndMonth[i], "-", monarchs$reignEndDay[i]))
  }
}

monarchs$end_ymd <- as.data.frame(end_ymd) 

#End of leading zero dates.



#----End of tidy data
#---- timeline code ----

# Note that to use ymd or as.Date a dataframe column cannot be used. Prehaps try as a string?

ggplot()+
  geom_segment( #Not necessary for data = monarchs
               aes(x = ymd(start_ymd), xend = ymd(end_ymd), y = 0, yend = 0),
               colour = monarchs$houseColours, size = 10)+
  scale_x_date( #This makes the plot run time forwards.
#    limits = c(maxlimit, minlimit), #This introduces min and max time period limits.
    breaks = c(ymd(start_ymd), ymd(end_ymd))
  )+
  xlab("Year AD")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),  axis.ticks.y=element_blank())+
  theme(legend.position="none")+
  geom_text(aes(label=monarchs$monarchTitle, x = (ymd(start_ymd) + ((ymd(end_ymd) - ymd(start_ymd))/2)), y = 0.5, angle=90), colour ="black")

