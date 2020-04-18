#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nCov2019)
library(plotly)
library(tidyverse)



shinyServer(function(input, output, session) {
    
    ###INPUT SELECTION:
    # Reactive list of available plots
    observe(priority = 2, {
        updateSelectInput(session = session,
                          inputId = "plot_types",
                          choices = gsub(list.files(path = file.path("plots"), recursive = TRUE),
                                         pattern = ".R", replacement = ""),
                          selected = gsub(list.files(path = file.path("plots"), recursive = TRUE),
                                          pattern = ".R", replacement = "")[1]
        )})
    
    
    #### LOAD DATA ####
    y <- load_nCov2019(lang = 'en', source='github')
    
    # get global data:
    d = y['global']
    
    
    # get data (reactive):
    ###to do or not necessary here.
    
    observeEvent(input$plot_types,
                 ignoreInit = TRUE, {
                     source(file.path("plots", paste0(input$plot_types, ".R")))
                     
                     # Save description
                     output$plot_description <- renderText(get_description())
                     # Save plot
                     output$plot <- renderPlotly({
                         get_plot(d = d, input = input$sliderNum)
                     })
                 })# END observe event plottype
    
    # credits text:
    output$Text <- renderText("Source: Tianzhi Wu, Erqiang Hu, Xijin Ge*, Guangchuang Yu*. Open-source analytics tools for studying the COVID-19 coronavirus outbreak. medRxiv, 2020.02.25.20027433. doi: https://doi.org/10.1101/2020.02.25.20027433")
    
})
