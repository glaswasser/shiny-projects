#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(nCov2019)
library(plotly)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    
    sidebarPanel(
        selectInput("plot_types", "Plot Type",
                    choices = "",
                    selected = ""),
        
        sliderInput(inputId = "sliderNum", 
                    label = "Choose Date:", 
                    min = as.Date("2020-02-21"), max = as.Date(Sys.Date()-1), 
                    value = as.Date("2019-02-21"),
                    animate = animationOptions(interval = 900)),
        
        
        # PLOT DESCRIPTION (reactive on plot selected)
        strong("Plot description:"),
        textOutput(outputId = "plot_description")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
        plotlyOutput(outputId = "plot",
                     width = "1000px", height = "750px")
    ))
)
