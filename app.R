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
    "GeoTimescale",
    "CO2",
    "Temp",
    "LR04",
    "Milankovitch",
    "Supercontinents",
    "Meteorites",
    "Volcanoes",
    "Pandemics",
    "Prehistory",
    "HistoricTimePeriods",
    "Monarchs",
    "WorldPop"
    
)


listOfPlots <- paste0("plot", listOfDatasets)

# This is in preparation for using web address strings to preset values.
enableBookmarking("url")


# ---- timeline UI ----

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("A timeline of climate and historical events"),
    
    #Show GitHub link
    mainPanel(
        fluidRow(width = 12,
                 tags$p("By Alex Searle-Barnes"),
                 tags$a(href = "https://github.com/alexsb1/timeline", target = "_blank", "View source code on GitHub")
        )
    ),
    
    br(),
    
    mainPanel(
        fluidRow(width = 12,
                 h4("Timeline Range"),
                 
                 sliderInput("maRange", "Refine timeline range by millions of years",
                             min = xAxisMin, max = xAxisMax,
                             value = c(xAxisMin, xAxisMax),
                             step = 1000000
                             ),
                 
                 sliderInput("kaRange", "Refine timeline range by thousands of years",
                             min = xAxisMin, max = xAxisMax,
                             value = c(xAxisMin, xAxisMax),
                             step = 1000
                 ),
                 
                 sliderInput("timelineRange", "Refine timeline range by hundreds of years",
                             min = xAxisMin, max = xAxisMax,
                             value = c(xAxisMin, xAxisMax),
                             step = 100
                             ),
                 
                 actionButton("button1kya", "Show last 1,000 years"),
                 actionButton("button5kya", "Show last 5,000 years"),
                 actionButton("button10kya", "Show last 10,000 years"),
                 actionButton("buttonReset", "Reset view"),
                 
                 br() #,
                 
#                 checkboxInput("checkYAxis", "Dynamic y-axis", value = FALSE)
                 
        ),
        
        br(),

        fluidRow(width = 12,
                 textOutput("rangeAsYearsElapsed"), #Print the time as years elapsed
                 textOutput("rangeAsYearsBCE"), #Plan to print the friendly time as years BCE/CE
                 textOutput("rangeAsYearsAgo") #Plan to print the friendly time as years ago
        ),
        
        br(),


        fluidRow(
            column(width = 12#,
#                   checkboxGroupInput("datasetsChecked", "Select datasets to plot", listOfDatasets, selected = TRUE)
            )
        ),

    br(),

    ), # end of main panel

    br(),
    
    mainPanel(
        
#        tableOutput("geoTimeLabels"),
        
        plotOutput(listOfPlots[1]),
        br(),
        plotOutput(listOfPlots[2]),
        br(),
        plotOutput(listOfPlots[3]),
        br(),
        plotOutput(listOfPlots[4]),
        br(),
        plotOutput(listOfPlots[5]),
        br(),
        plotOutput(listOfPlots[6]),
        br(),
        plotOutput(listOfPlots[7]),
        br(),
        plotOutput(listOfPlots[8]),
        br(),
        plotOutput(listOfPlots[9]),
        br(),
        plotOutput(listOfPlots[10]),
        br(),
        plotOutput(listOfPlots[11]),
        br(),
        plotOutput(listOfPlots[12]),
        br(),
        plotOutput(listOfPlots[13])
        
        ),
    
    br(),

    fluidRow(
        column(width = 6,
               actionButton("showReferencesButton", "Show reference list")
            )
        ),

    fluidRow(
        column(width = 6,
               tableOutput("referenceTable")
        )
    )


    


) # End of UI

    




# Define server logic required to draw the controls and timelinePlot
server <- function(input, output, session) {
    
    #Update fine timeline range slider with values from coarse slider
    
    observe({
        updateSliderInput(session, "kaRange",
                      min = input$maRange[1], max = input$maRange[2],
                      value = c(input$maRange[1], input$maRange[2]))
    })
    
    observe({
        updateSliderInput(session, "timelineRange",
                          min = input$kaRange[1], max = input$kaRange[2],
                          value = c(input$kaRange[1], input$kaRange[2]))
    })
    
    
    
    
    
    # Adjust timeline range slider value using action button
    # Take action on click
    observeEvent(input$button1kya, {
        
        updateSliderInput(session, "maRange",
                          min = xAxisMin, max = xAxisMax, # The min and max should not change for the Ma slider.
                          value = c(xAxisMax - 1001, xAxisMax))
        
        updateSliderInput(session, "kaRange",
                          min = xAxisMax - 1500, max = xAxisMax, # This will give 500 years leeway on the minimum
                          value = c(xAxisMax - 1001, xAxisMax))
    })
    
    
    observeEvent(input$button5kya, {
        
        updateSliderInput(session, "maRange",
                          min = xAxisMin, max = xAxisMax, # The min and max should not change for the Ma slider.
                          value = c(xAxisMax - 5001, xAxisMax))
        
        
        updateSliderInput(session, "kaRange",
                          min = xAxisMax - 6000, max = xAxisMax, # This will give 1000 years leeway on the minimum
                          value = c(xAxisMax - 5001, xAxisMax))
    })
    
    observeEvent(input$button10kya, {
        
        updateSliderInput(session, "maRange",
                          min = xAxisMin, max = xAxisMax, # The min and max should not change for the Ma slider.
                          value = c(xAxisMax - 10001, xAxisMax))
        
        
        updateSliderInput(session, "kaRange",
                          min = xAxisMax - 15000, max = xAxisMax, # This will give 1000 years leeway on the minimum
                          value = c(xAxisMax - 10001, xAxisMax))
    })
    
    
    observeEvent(input$buttonReset, {
        updateSliderInput(session, "maRange",
                          min = xAxisMin, max = xAxisMax, # This will reset Ma slider and subsequently all others.
                          value = c(xAxisMin, xAxisMax))
    })
    
    
    
    # Calculate years elapsed from slider
    output$rangeAsYearsElapsed <- renderText ({
        paste("Shown time range from",
              input$timelineRange[1] %>% format(., big.mark = ",", scientific = FALSE),
              "to", 
              input$timelineRange[2] %>% format(., big.mark = ",", scientific = FALSE),
              "years elapsed since Earth's formation")
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
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )
        
        
    })
    

    # Prepare the geoTimescale labels in advance

    geoTimeLabels <- reactive({
        geoTimeScale %>%
            filter(Start_elapsed_time >= input$timelineRange[1] & End_elapsed_time <= input$timelineRange[2]) # only keep the rows of data currently displayed in the timeline.
        })
    
    # geoTimeLabels <- observe({
    #     ifelse(nrow(geoTimeLabels()) == 0,
    #         geoTimeLabels <- rbind(geoTimeLabels, geoTimeScale %>% names(.)), #if true then add an extra row
    #         geoTimeLabels <- geoTimeLabels #otherwise return filtered dataframe
    #     )
    # })
    
    # geoTimeLabels <- reactive({
    # if(nrow(geoTimeLabels) == 0){
    #     geoTimeLabels <- rbind(geoTimeLabels, geoTimeScale %>% names(.))
    # }
    #     })



    output$geoTimeLabels <- renderTable(geoTimeLabels())

    
    
    
    
    
           
    
    output$plotGeoTimescale <- renderPlot({
        plotGeoTimescale +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            ) +
           # geom_text_repel(data = geoTimeScale %>% filter(Start_elapsed_time >= input$timelineRange[1] & End_elapsed_time <= input$timelineRange[2]),
           #                  aes(label=Age, x=((Start_elapsed_time + End_elapsed_time)/2), y = 40),
           #                  max.overlaps = maxTextOver,
           #                  direction = "both",
           #                  ylim = c(38,42)) +
           # 
           #  geom_text_repel(data = epochPlot %>% filter(Start_elapsed_time >= input$timelineRange[1] & End_elapsed_time <= input$timelineRange[2]),
           #                  aes(label=Epoch, x=((Start_elapsed_time + End_elapsed_time)/2), y = 30),
           #                  max.overlaps = maxTextOver,
           #                  direction = "both",
           #                  ylim = c(28,32)) +
            # geom_text_repel(data = periodPlot %>% filter(Start_elapsed_time >= input$timelineRange[1] & End_elapsed_time <= input$timelineRange[2]),
            #                 aes(label=Period, x=((Start_elapsed_time + End_elapsed_time)/2), y = 20),
            #                 max.overlaps = maxTextOver,
            #                 direction = "both",
            #                 ylim = c(18,22)) +
#            geom_text_repel(data = eraPlot %>% filter(Start_elapsed_time >= input$timelineRange[1] & End_elapsed_time <= input$timelineRange[2]), aes(label=Era, x=((Start_elapsed_time + End_elapsed_time)/2), y = 10), max.overlaps = maxTextOver, direction = "both", ylim = c(8,12)) +
#            geom_text_repel(data = eonPlot %>% filter(Start_elapsed_time >= input$timelineRange[1] & End_elapsed_time <= input$timelineRange[2]), aes(label=Eon, x=((Start_elapsed_time + End_elapsed_time)/2), y = 0), max.overlaps = maxTextOver, direction = "both", ylim = c(0,2)) +

            geom_text(aes(x = input$timelineRange[1], y = 40, label = "Stage"), colour = geoTimeTextcolour, hjust = "left")+
            geom_text(aes(x = input$timelineRange[1], y = 30, label = "Epoch"), colour = geoTimeTextcolour, hjust = "left")+
            geom_text(aes(x = input$timelineRange[1], y = 20, label = "Period"), colour = geoTimeTextcolour, hjust = "left")+
            geom_text(aes(x = input$timelineRange[1], y = 10, label = "Era"), colour = geoTimeTextcolour, hjust = "left")+
            geom_text(aes(x = input$timelineRange[1], y = 0, label = "Eon"), colour = geoTimeTextcolour, hjust = "left")
        
    })
    

    
    output$plotHistoricTimePeriods <- renderPlot({
        plotHistoricTimePeriods +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            ) +
          geom_text_repel(data = historicTimePeriods,
                          aes(label=Name, x=(startYearsElapsed + endYearsElapsed)/2, y = 0),
                          max.overlaps = maxTextOver,
                          direction = "x")
        
    })
    
    
    output$plotLR04 <- renderPlot({
        plotLR04 +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )
        
    })
    
    
    output$plotMeteorites <- renderPlot({
        plotMeteorites +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )+
            geom_text_repel(data = meteorites %>% filter(yearsElapsed >= input$timelineRange[1] & yearsElapsed <= input$timelineRange[2]), aes(label=Name, x=yearsElapsed, y = 0), max.overlaps = maxTextOver, direction = "x")
        
    })
    

    
    output$plotMilankovitch <- renderPlot({
        plotMilankovitch +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )
        
    })
    
    
    
    output$plotMonarchs <- renderPlot({
        plotMonarchs +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )+
            geom_text_repel(data = monarchs %>% filter(startElapsedYears >= input$timelineRange[1] & endElapsedYears <= input$timelineRange[2]), aes(label=monarchTitle, x=(startElapsedYears + endElapsedYears)/2, y = 0), max.overlaps = maxTextOver, direction = "x")

    })
    
    
    
    output$plotPandemics <- renderPlot({
        plotPandemics +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )+
            geom_text_repel(data = pandemics %>% filter(startYearElapsed >= input$timelineRange[1] & endYearElapsed <= input$timelineRange[2]), aes(label=Name, x=(startYearElapsed + endYearElapsed)/2, y = 0), max.overlaps = maxTextOver, direction = "x")
        
    })
    
    
    output$plotPrehistory <- renderPlot({
        plotPrehistory +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )+
            geom_text_repel(data = prehistory %>% filter(startYearsElapsed >= input$timelineRange[1] & endYearsElapsed <= input$timelineRange[2]), aes(label=Age, x=(startYearsElapsed + endYearsElapsed)/2, y = 0), max.overlaps = maxTextOver, direction = "x")
        
    })
    
    
    
    output$plotSupercontinents <- renderPlot({
        plotSupercontinents +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )+
            geom_text_repel(data = supercontinents %>% filter(startElapsedYears >= input$timelineRange[1] & endElapsedYears <= input$timelineRange[2]), aes(label=Supercontinent, x=(startElapsedYears + endElapsedYears)/2, y = 0), max.overlaps = maxTextOver, direction = "x")
            
    })
    
    
    output$plotTemp <- renderPlot({
        plotTemp +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )
        
    })
    
   
     
    output$plotVolcanoes <- renderPlot({
        plotVolcanoes +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )+
            geom_text_repel(data = volcanoes %>% filter(yearsElapsed >= input$timelineRange[1] & yearsElapsed <= input$timelineRange[2]), aes(label=Volcano, x=yearsElapsed, y = 0), max.overlaps = maxTextOver, direction = "x")

    })
    
    
    
    output$plotWorldPop <- renderPlot({
        plotWorldPop +
            coord_cartesian( #force x-axis scale
                xlim = c(input$timelineRange[1], input$timelineRange[2]),
                expand = TRUE
            )
        
    })
    
    
    
    #Show reference list on click
    
    observeEvent(input$showReferencesButton, {
        output$referenceTable <- renderPrint({
        referenceList %>% as.character(.)
        })
    })
    
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)


# TODO

# Add / remove datasets as selected in the UI
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/





