# Import ggplots

source("scripts/R/IndividualPlots.R")



# Make single multiplot that is aligned by x-axis (years elapsed) ----

# Comment out any unwanted plot
listOfPlots <- list(
  plotGeoTimescale, #Note that this plot includes the x-axis time elapsed values.
  plotCO2,
  plotTemp,
  plotMonarchs,
  plotMeteorites,
  plotPrehistory,
  plotLR04,
  plotVolcanoes,
  plotSupercontinents,
  plotHistoricTimePeriods,
  plotWorldPop,
  plotPandemics,
  plotMilankovitch
)

# The plotGeoTimescale should be 5 times taller than anything else to avoid each row being compressed.
listOfHeights <- c(
  1, #try 5 times taller for plotGeoTimescale
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1
  
)

x <- ggarrange(plotlist = listOfPlots[1:5], ncol = 1, nrow = 5, align = "hv", heights = listOfHeights[1:5], common.legend = TRUE)



# This is form Lorna's suggestion. Doesn't work - same issue.
p1 <- ggarrange(plotlist = listOfPlots[1:5], ncol = 1, nrow = 5)
p2 <- ggarrange(plotlist = listOfPlots[6:10], ncol = 1, nrow = 5)
p3 <- ggarrange(plotlist = listOfPlots[11:13], ncol = 1, nrow = 5)

p4 <- ggarrange(p1,p2,p3, ncol = 1, nrow = 3)





# Saves the combined plot into a series of png images.
for(q in 1:length(x)){
  ggsave(filename = paste0(q,".png"), plot = x[[q]])
}


# End of multiplot ----









# this works because it's not too tall
ggarrange(plotlist = listOfPlots, ncol = 2, nrow = 7, align = "hv", common.legend = TRUE, legend = "none")