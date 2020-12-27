#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(plotly)
library(tidyverse)
library(coronavirus)
library(magrittr)
#update_dataset()
library(wpp2019)
library(glue)
data(pop)
data("coronavirus")
# get population info
pop %<>% select(name, `2020`) %>% rename(population = `2020`)
# rename americas
pop$name <- plyr::revalue(pop$name, c("United States of America" = "US"))
# format population:

pop$population <- pop$population*1000

coronavirus %<>% left_join(pop, by = c("country" = "name"))


# create relative cases
coronavirus %<>% mutate(relative_cases = cases/(population/1000))


# create last 24 hours:
coronavirus %>% 
    filter(date == max(date)) %>%
    select(country, type, cases) %>%
    group_by(country, type) %>%
    summarise(total_cases = sum(cases)) %>%
    pivot_wider(names_from = type,
                values_from = total_cases) %>%
    arrange(-confirmed)


shinyServer(function(input, output, session) {
    
    ###INPUT SELECTION:
    # Reactive list of available plots
    observe(priority = 1, {
        updateSelectInput(session = session,
                          inputId = "plot_types",
                          choices = gsub(list.files(path = file.path("plots"), recursive = TRUE),
                                         pattern = ".R", replacement = ""),
                          selected = gsub(list.files(path = file.path("plots"), recursive = TRUE),
                                          pattern = ".R", replacement = "")[1]
        )})
    
    countries <- levels(factor(coronavirus$country))
    # observe input checkbox for countries:
    valuesCheck1 <- reactiveValues(x = NULL)
    observeEvent(input$check1a, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check1a)))
    observeEvent(input$check1b, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check1b)))
    observeEvent(input$check1c, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check1c)))
    observeEvent(input$check2a, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check2a)))
    observeEvent(input$check2b, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check2b)))
    observeEvent(input$check2c, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check2c)))
    observeEvent(input$check3a, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check3a)))
    observeEvent(input$check3b, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check3b)))
    observeEvent(input$check3c, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check3c)))


    observeEvent(input$plot_types,
                 ignoreInit = TRUE, {
                     source(file.path("plots", paste0(input$plot_types, ".R")))

                     # Save description
                     output$plot_description <- renderText(get_description())
                     
                     # Save plot
                     output$plot <- renderPlotly({
                         get_plot(df = coronavirus, input = input$sliderNum,
                                  time_range = input$overTime,
                                  countries = c(input$check1a, input$check1b,
                                                input$check1c, input$check2a,
                                                input$check2b, input$check2c,
                                                input$check3a, input$check3b,
                                                input$check3c),
                                  relative = input$relative)
                     }) # end plotly
                 })# END observe event plottype

})
