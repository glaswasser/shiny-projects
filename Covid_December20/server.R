
# COVID 19 SHINY APP - ALEXANDER HEINZ

library(shiny)
library(scales)
library(plotly)
library(tidyverse)
library(coronavirus)
library(magrittr)
library(wpp2019)
library(glue)
library(wordcloud2)
library(tidytext)
library(newsanchor)
library(lubridate)
library(stopwords)


# create last 24 hours:
#coronavirus %>% 
#   filter(date == max(date)) %>%
#  select(country, type, cases) %>%
# group_by(country, type) %>%
#summarise(total_cases = sum(cases)) %>%
#    pivot_wider(names_from = type,
#               values_from = total_cases) %>%
#  arrange(-confirmed)
#update_dataset(silence = TRUE)


shinyServer(function(input, output, session) {
  
  output$note <- renderText("Note: This app is designed to give a rough overview over time, for the most up-to-date or regional data please use other sources")
  
  
  # DATA PROCESSING...
  withProgress(message = "Loading Data", value = 0, {
    setProgress(value = 0.70, message = "updating data, this may take some seconds...")
    #update_dataset(silence = TRUE)
    coronavirus <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", stringsAsFactors = FALSE)

    coronavirus$date = as.Date(coronavirus$date, origin = "1970-01-01")
    setProgress(value = 0.85, message = "Loading population data...")
    data(pop)
    
    setProgress(value = 0.9, message = "Loading covid-19 data...")
    #data("coronavirus")
    
    setProgress(value = 0.95, message = "processing covid-19 data...")
    # get population info
    pop %<>% select(name, `2020`) %>% rename(population = `2020`)
    # rename americas
    pop$name <- plyr::revalue(pop$name, c("United States of America" = "US"))
    # format population:
    pop$population <- pop$population*1000
    coronavirus %<>% left_join(pop, by = c("country" = "name"))
    # create relative cases
    coronavirus %<>% mutate(relative_cases = cases/(population/1000))
    # fix buggy recovery cases for US: ????
    # coronavirus$cases[coronavirus$date == "2020-12-14" &
    #                          coronavirus$country == "US" &
    #                         coronavirus$type == "recovered"] <- coronavirus$cases[coronavirus$date == "2020-12-14" &
    #                                                                                  coronavirus$country == "US" &
    #                                                                                 coronavirus$type == "recovered"]*-1
    
    showNotification(paste("Data is Ready! Please wait a little while until the plots show up!"), type = "message")
  })
  # END DATA PROCESSING
  
  
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
  # observe input checkbox for countries (one for every letter category):
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
  
 
  # reactive on the wordcloud slider, get the newspaper results of the week before
  for_wordcloud <- reactiveValues(x = NULL)
  observeEvent(input$wordcloud_slider, {
    # set largest possible from and to date back in the days:
    from = as.Date(input$wordcloud_slider) - weeks(1)
    to = as.Date(input$wordcloud_slider)
    
    for_wordcloud$results <- get_everything_all(query = "corona", language = "en",
                                  from = from, to = to,
                                  api_key = "c8919073179f4417aac29c2a5a3481ec",
                                   #sort_by = "popularity",
                                  sources = c("fox-news", "google-news", "daily-mail", "time", "independent",
                                              "the-new-york-times", "the-wall-street-journal", "the-washington-post",
                                              "usa-today"))[[2]]

        
    for_wordcloud$stopwords <- stopwords("en", source = "snowball")
  })
  

  # reactive on plot types, create plots:
  observeEvent(input$plot_types,
               ignoreInit = TRUE, {
                 source(file.path("plots", paste0(input$plot_types, ".R")))
                 
                 # Save description
                 output$plot_description <- renderText(get_description())
                 
                 # Save plot
                 output$plot <- renderPlotly({
                   get_plot(df = coronavirus,
                            # time for single slider
                            input = input$sliderNum,
                            # time range for slider time range
                            time_range = input$overTime,
                            # countries for checkbox dropdown menu
                            countries = c(input$check1a, input$check1b,
                                          input$check1c, input$check2a,
                                          input$check2b, input$check2c,
                                          input$check3a, input$check3b,
                                          input$check3c),
                            # relative checkboxes for the plots
                            relative_cum = input$relative_cum,
                            relative_overtime = input$relative_overtime,
                            # cumulative plot type:
                            cum_type = input$cum_type
                   )
                 }) # end plotly
                 # for wordcloud:
                 output$wordcloud <- renderWordcloud2(get_wordcloud(wordcloud_params = for_wordcloud,
                                                                    wordcloud_slider = input$wordcloud_slider))
               })# END observe event plottype
  
})
  