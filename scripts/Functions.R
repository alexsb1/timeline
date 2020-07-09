# Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
# This file contains custom functions that may be reused globally in the Timeline project.
#

#---- set libraries----
library(lubridate)

# end of libaries


#---- set variables ----


thisYear <- year(today())
startYBP <- ymd("1950-01-01") #The commencement date for years before present (YBP) 


yearCEtoElapsedTime <- 4600000000 #this is the number of years between years in CE format and years elapsed from the earth's formation.

# end of variables






# Functions to convert to/from BCE/CE to years before present.

yearCEtoYearsElapsed <- function(yearCE, CE){ #the CE specifies if the year date is common era (CE/AD) or before common era (BCE/BC). Note that YearCE is used as an input for both CR and BCE years.
  ifelse(CE == "BCE",
         elapsedYear <- yearCEtoElapsedTime - thisYear - yearCE, #subtracts the number of years to account for being before common era (BCE/BC).
         ifelse(CE == "CE",
                elapsedYear <- yearCEtoElapsedTime - thisYear + yearCE,
                print("Date error.")
         )
  )
  return(elapsedYear)
}





yearsElapsedToYearCE <- function(yearsElapsed){
  yearCE <- NULL
  if(yearsElapsed <= yearCEtoYearsElapsed(0,"CE")) #If yearsElapsed is before (less than) 0 CE/AD then print CE era, else print BCE.
    (
      yearCE <- data.frame(
        year = yearCEtoElapsedTime - thisYear - yearsElapsed,
        era = "BCE"
      )
    ) #If yearsElapsed is less than 0 CE then print BCE
  else
    (
      yearCE <- data.frame(
        year = abs(yearCEtoElapsedTime - thisYear - yearsElapsed),
        era = "CE"
      )
    )
  return(yearCE)
}





# function to convert between years ago to years elapsed since the earth formation
yearsAgoToEapsedYears <- function(yearsAgo){
  elapsedYears <- yearCEtoElapsedTime - yearsAgo
  return(elapsedYears)
}




yearsElapsedToYearsAgo <- function(yearsElapsed){
  yearsAgo <- yearCEtoYearsElapsed(today() %>% decimal_date(.), "CE") - yearsElapsed
  return(yearsAgo)
}


#End of functions.
