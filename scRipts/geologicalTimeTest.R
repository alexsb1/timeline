library(ggplot2)
library(timevis)


timeSeries <- read.csv("data/geological_time_scale.csv")

ggplot(timeSeries, aes(Start_million_years_ago, Age))+
  geom_line()





df <- data.frame(
  id = 1:nrow(timeSeries),
  content = timeSeries$Age,
  start = timeSeries$End_million_years_ago,
  end = timeSeries$Start_million_years_ago,
#  group = timeSeries$Period,
#  subgroup = timeSeries$Epoch,
  title = paste(timeSeries$Age),
  style = paste0("background-color:#", timeSeries$back_colour)
)



timevis(df)
