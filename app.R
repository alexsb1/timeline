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
library(lubridate)

#end of libraries

#---- data import ----
# ATTENTION
# All the data processing should have already been completed using DataProcessing.R

# Source Functions.R file so these functions are available here.
source("scripts/Functions.R")

# Source the ggplots
# This contains the default xAxisMin and xAxisMax values that are used to preload the entire graph, to optimise the suer experience.
source("scripts/IndividualPlots.R")

# Source the reference list
# This must be called after ggplots
source("scripts/Sources.R")

#referenceList

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



# ---- timeline UI ----

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("A timeline of historic events and climate"),
    
    mainPanel(
#        column(width = 12,
#               h4("Timeline range"),

        sliderInput("timelineRange", "Timeline range",
                    min = xAxisMin, max = xAxisMax,
                    value = c(xAxisMin, xAxisMax)
                    ),
        
        textOutput("rangeAsYearsElapsed"), #Print the time as years elapsed
        
        textOutput("rangeAsYearsBCE"), #Plan to print the friendly time as years BCE/CE
        
        textOutput("rangeAsYearsAgo") #Plan to print the friendly time as years ago
        
               )
    ,

    fluidRow(
        column(width = 6,
#               checkboxGroupInput("datasetsChecked", "Select datasets to plot", listOfDatasets, selected = TRUE)
               )
    
    ),
    
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
               actionButton("showReferencesButton", "Show reference list")
            ),
        tableOutput("referenceTable")
        )


    


) # End of UI

    




# Define server logic required to draw the controls and timelinePlot
server <- function(input, output) {
    

    # Calculate years elapsed from slider
    output$rangeAsYearsElapsed <- renderText ({
        paste("Shown time range from",
              input$timelineRange[1] %>% format(., big.mark = ",", scientific = FALSE),
              "to", 
              input$timelineRange[2] %>% format(., big.mark = ",", scientific = FALSE),
              "years elapsed since formation")
    })
    
    
    # Calculate years elapsed into friendly years BCE/BC
    output$rangeAsYearsBCE <- renderText ({
        paste("Shown time range from",
              yearsElapsedToYearCE(input$timelineRange[1])[1] %>% floor(.) %>% format(., big.mark = ",", scientific = FALSE), #displays year
              yearsElapsedToYearCE(input$timelineRange[1])[2], #displays BCE/CE
              "to",
              yearsElapsedToYearCE(input$timelineRange[2])[1] %>% floor(.) %>% format(., big.mark = ",", scientific = FALSE),
              yearsElapsedToYearCE(input$timelineRange[2])[2]
              )
    })
    
    # Calculate years elapsed into friendly years ago
    output$rangeAsYearsAgo <- renderText ({
        paste("Shown time range from",
              yearsElapsedToYearsAgo(input$timelineRange[1]) %>% floor(.) %>% format(., big.mark = ",", scientific = FALSE) , #displays year. Make sure format is last operation.
              "years ago to",
              yearsElapsedToYearsAgo(input$timelineRange[2]) %>% trunc(.) %>% format(., big.mark = ",", scientific = FALSE),
              "years ago"
        )
    })
    
    
    
    # Reactive plots
    
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
    
    
    output$referenceTable <- renderPrint({
        referenceList %>% as.character(.)
        })
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)


# TODO

# Add / remove datasets as selected in the UI
# here is help documentation about reactive expressions. This will be needed to replot for change in xaxis.
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/

