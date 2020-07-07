# Timeline of concurrent events and climates
#
# By Alex Searle-Barnes
#
# https://github.com/alexsb1/timeline
#
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#

# ----- set libraries ----
library(shiny)
library(shinythemes)
library(rsconnect)
library(tidyverse)
library(ggrepel)
library(ggpubr) #check is this is still necessary. It's used to plot multiple ggplots on a single canvas.

#end of libraries

# Source Functions.R file so these functions are available here.
source("scripts/R/Functions.R")


#---- data import ----

# ATTENTION

# All the data processing should have already been completed using DataProcessing.R
# Any import here should be ready to plot.
# Any data processing here is computed on the fly, so will affect user experience.

#start of data import

geoTimeScale <- read.csv(file="data/processed/geoTimeScale.csv", header = TRUE)
epochPlot <- read.csv(file="data/processed/epochPlot.csv", header = TRUE)
periodPlot <- read.csv(file="data/processed/periodPlot.csv", header = TRUE)
eraPlot <- read.csv(file="data/processed/eraPlot.csv", header = TRUE)
eonPlot <- read.csv(file="data/processed/eonPlot.csv", header = TRUE)
phanerozoicCO2 <- read.csv(file="data/processed/phanerozoicCO2.csv", header = TRUE)
CO2_ppm_800000 <- read.csv(file="data/processed/CO2_ppm_800000.csv", header = TRUE)
tempAnom <- read.csv(file="data/processed/tempAnom.csv", header = TRUE)
historicEvents <- read.csv(file="data/processed/historicEvents.csv", header = TRUE)
monarchs <- read.csv(file="data/processed/monarchs.csv", header = TRUE)
meteorites <- read.csv(file="data/processed/meteorites.csv", header = TRUE)
prehistory <- read.csv(file="data/processed/prehistory.csv", header = TRUE)
historicTimePeriods <- read.csv(file="data/processed/historicTimePeriods.csv", header = TRUE)
LR04 <- read.csv(file="data/processed/LR04.csv", header = TRUE)
volcanoes <- read.csv(file="data/processed/volcanoes.csv", header = TRUE)
supercontinents <- read.csv(file="data/processed/supercontinents.csv", header = TRUE)
worldPop <- read.csv(file="data/processed/worldPop.csv", header = TRUE)
pandemics <- read.csv(file="data/processed/pandemics.csv", header = TRUE)
climateEvents <- read.csv(file="data/processed/climateEvents.csv", header = TRUE)
bondEvents <- read.csv(file="data/processed/bondEvents.csv", header = TRUE)
milankovitch <- read.csv(file="data/processed/Milankovitch.csv", header = TRUE)


# end of data import


# A friendly-ish list of variable names.
# The order of this list determines the order of plotting in the UI.
listOfDatasets <- c(
    "CO2",
    "GeoTimescale",
    "HistoricTimePeriods",
    "LR04",
    "Meteorites",
    "Milankovitch",
    "Monarchs",
    "Pandemics",
    "Prehistory",
    "Supercontinents",
    "Temp",
    "Volcanoes",
    "WorldPop"
)


listOfPlots <- paste0("plot", listOfDatasets)




#---- Preset x-axis ----
# Preset the x-axis so the timeline can load in full before giving the user the ability to change.

xAxisMin <- 0 # 0 is the formation of the earth. In years.

xAxisMax <- today() %>% ymd(.) %>% decimal_date(.) %>% yearCEtoYearsElapsed(.,"CE") #convert into continuous value for x axis time elapsed.

#End of adjusting x-axis


#---- Individual ggplots ----
# Load in ggplots from external file

source("scripts/R/IndividualPlots.R")


# end of loading plots



# ---- timeline UI ----

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("A timeline of historic events and climate"),
    
    
    mainPanel(
        plotOutput(listOfPlots[1]),
        plotOutput(listOfPlots[2]),
        plotOutput(listOfPlots[3]),
        plotOutput(listOfPlots[4]),
        plotOutput(listOfPlots[5]),
        plotOutput(listOfPlots[6]),
        plotOutput(listOfPlots[7]),
        plotOutput(listOfPlots[8]),
        plotOutput(listOfPlots[9]),
        plotOutput(listOfPlots[10]),
        plotOutput(listOfPlots[11]),
        plotOutput(listOfPlots[12]),
        plotOutput(listOfPlots[13])
        ),
    
    
    fluidRow(
        column(width = 12,
#               h4("Timeline range"),
                    sliderInput("timelineRange", "Timeline range",
                                min = xAxisMin, max = xAxisMax,
                                value = c(xAxisMin, xAxisMax))
               )
    ),

    fluidRow(
        column(width = 6,
               checkboxGroupInput("datasetsChecked", "Select datasets to plot", listOfDatasets, selected = NULL)
               )
    
    )
)

    




# Define server logic required to draw the controls and timelinePlot
server <- function(input, output) {

    output$plotCO2 <- renderPlot({
        plotCO2 +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    output$plotGeoTimescale <- renderPlot({
        plotGeoTimescale +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    

    
    output$plotHistoricTimePeriods <- renderPlot({
        plotHistoricTimePeriods +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    output$plotLR04 <- renderPlot({
        plotLR04 +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    output$plotMeteorites <- renderPlot({
        plotMeteorites +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    

    
    output$plotMilankovitch <- renderPlot({
        plotMilankovitch +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    
    output$plotMonarchs <- renderPlot({
        plotMonarchs +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    
    output$plotPandemics <- renderPlot({
        plotPandemics +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    output$plotPrehistory <- renderPlot({
        plotPrehistory +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    
    output$plotSupercontinents <- renderPlot({
        plotSupercontinents +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    output$plotTemp <- renderPlot({
        plotTemp +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
   
     
    output$plotVolcanoes <- renderPlot({
        plotVolcanoes +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    
    output$plotWorldPop <- renderPlot({
        plotWorldPop +
            scale_x_continuous( #force x-axis scale
                limits = c(input$timelineRange[1], input$timelineRange[2])
            )
        
    })
    
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)


# TODO

# Make the ggplots work - and optimise so they don't take too long to reload.
# Add / remove datasets as selected in the UI
# here is help documentation about reactive expressions. This will be needed to replot for change in xaxis.
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/

