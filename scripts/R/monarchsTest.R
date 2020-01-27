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


houseColours <- NULL
houseNames <- unique(monarchs$house)
for (i in monarchs$house){
  ifelse(grepl(houseNames[1], i), houseColours <- append(houseColours, paste("blue")), 
         ifelse(
           grepl(houseNames[2], i), houseColours <- append(houseColours, paste("red")), 
           ifelse(
             grepl(houseNames[3], i), houseColours <- append(houseColours, paste("green")), 
             ifelse(
               grepl(houseNames[4], i), houseColours <- append(houseColours, paste("aqua")), 
               ifelse(
                 grepl(houseNames[5], i), houseColours <- append(houseColours, paste("purple")), 
                  ifelse(
                    grepl(houseNames[6], i), houseColours <- append(houseColours, paste("hotpink")), 
                    ifelse(
                      grepl(houseNames[7], i), houseColours <- append(houseColours, paste("yellow")), 
                      ifelse(
                        grepl(houseNames[8], i), houseColours <- append(houseColours, paste("orange")), 
                        ifelse(
                          grepl(houseNames[9], i), houseColours <- append(houseColours, paste("brown")), 
                          ifelse(
                            grepl(houseNames[10], i), houseColours <- append(houseColours, paste("teal")), 
                            ifelse(
                              grepl(houseNames[11], i), houseColours <- append(houseColours, paste("beige")), 
                              ifelse(
                                grepl(houseNames[12], i), houseColours <- append(houseColours, paste("white")), 
                                ifelse(
                                  grepl(houseNames[13], i), houseColours <- append(houseColours, paste("firebrick1")), 
                                  ifelse(
                                    grepl(houseNames[14], i), houseColours <- append(houseColours, paste("lawngreen")), 
                                    ifelse(
                                      grepl(houseNames[15], i), houseColours <- append(houseColours, paste("violet")), 
                                      ifelse(
                                        print("Unknown house. Cannot assign a colour.")
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
               )
             )
           )
         )
  )
  
}

houseColours <- houseColours %>% as.data.frame(.)
names(houseColours) <- "houseColours"
monarchs <- cbind(monarchs, houseColours)




df <- data.frame(
  id = 1:nrow(monarchs),
  content = monarchs$monarchTitle,
  start = monarchs$timelineStart,
  end = monarchs$timelineEnd,
  group = monarchs$house,
  subgroup = monarchs$house,
  title = paste(monarchs$monarchTitle, monarchs$eventDescription),
  style = paste0("background-color:", monarchs$houseColours)
)



timevis(df)



#end of timeline code