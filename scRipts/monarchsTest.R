# Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
# Script to test timelines with the monarchs.csv file.
#

# ----- set libraries ----
library(tidyverse)
library(timevis)


#end of libraries

#---- set variables ----


# end of variables

#---- data import ----

monarchs <- read.csv(file="data/monarchs.csv", header = TRUE)

# end of data import

#---- timeline code ----
# This is for R, for shiny see https://daattali.com/shiny/timevis-demo/

# make functions to standardise timeline plotting dates in format YYYY-M-D. There are no buffering zeros.

timelineDates <- function(df) {
  for (i in 1:nrow(df))
    Year <- df$reignStartYear[i]
    Month <- df$reignStartMonth[i]
    Day <- df$reignStartDay[i]
    date <- paste0(Year, "-", Month, "-", Day)
  return(date)
}

# End of functions



timelineStart <- NULL
  for (i in 1:nrow(monarchs)){
    timelineStart <- append(timelineStart, paste0(monarchs$reignStartYear[i], "-", monarchs$reignStartMonth[i], "-", monarchs$reignStartDay[i]))
}
timelineStart <- timelineStart %>% as.data.frame(.)
names(timelineStart) <- "timelineStart"
monarchs <- cbind(monarchs, timelineStart)

timelineEnd <- NULL
for (i in 1:nrow(monarchs)){
  timelineEnd <- append(timelineEnd, paste0(monarchs$reignEndYear[i], "-", monarchs$reignEndMonth[i], "-", monarchs$reignEndDay[i]))
}
timelineEnd <- timelineEnd %>% as.data.frame(.)
names(timelineEnd) <- "timelineEnd"
monarchs <- cbind(monarchs, timelineEnd)

df <- data.frame(
  id = 1:nrow(monarchs),
  content = monarchs$monarchTitle,
  start = monarchs$timelineStart,
  end = monarchs$timelineEnd
)

timevis(df)



#end of timeline code