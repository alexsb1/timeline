library(ggplot2)
library(timevis)
library(ggpmisc)


geoTimeScale <- read.csv("data/raw/geological_time_scale.csv")
colourList <- paste0("#", geoTimeScale$back_colour)

minlimit <- min(geoTimeScale$End_million_years_ago)
maxlimit <- max(geoTimeScale$Start_million_years_ago)

minlimit <- 100
maxlimit <- 130



ggplot(data = geoTimeScale) +
  geom_segment(aes(x=Start_million_years_ago, xend=End_million_years_ago, y=0, yend=0, colour = back_colour), size=100, linetype = 1, show.legend = FALSE)+
  scale_colour_manual(values = colourList)+
  scale_y_continuous(limits = 0:1)+
  scale_x_reverse( #This makes the plot run time forwards.
    limits = c(maxlimit, minlimit), #This line not necessary
#    breaks = c(seq(min(geoTimeScale$End_million_years_ago),max(geoTimeScale$Start_million_years_ago),by=1000)),
#    geoTimeScale$End_million_years_ago,
#    geoTimeScale$Start_million_years_ago
    )+
  xlab("Millions of years ago")+
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),  axis.ticks.y=element_blank())+
  theme(legend.position="none") +
  geom_text(aes(label=geoTimeScale$Age, x = geoTimeScale$Start_million_years_ago, y = 0.6, angle=90), colour = geoTimeScale$text_colour)
  

               
               
               











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
