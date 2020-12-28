# a shiny covid-19-app i created 

library(shiny)
# remotes::install_github("GuangchuangYu/nCov2019")

library(rsconnect)
library(nCov2019)
library(tidyverse)
#install.packages("directlabels")
library(directlabels)
library(ggrepel)
library(plotly)
library(glue)
library(lubridate)

options(scipen=999)



####### DATA:
y <- load_nCov2019(lang = 'en', source='github')

# get global data:
d = y['global']

yesterday <- Sys.Date()-1

#######

# Define UI for application
ui <- fluidPage(

    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Slider for the number of observations to generate ----
            # INIT SLIDER
            # idea: with the slider, you can choose the date range
            sliderInput(inputId = "sliderNum", 
                        label = "Choose Date / End date of plot:", 
                        min = as.Date("2020-02-21"), max = as.Date(Sys.Date()-1), 
                        value = as.Date("2019-02-21"),
                        animate = animationOptions(interval = 900))
        ), # END SIDEBAR PANEL

    mainPanel( # START MAINPANEL
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Cases in Top 10 Countries", plotOutput("CaseBarPlot")),
                    tabPanel("Cases over time", plotOutput("timeplot")),
                    tabPanel("Deaths over time", plotOutput("deathtimeplot")),
                    tabPanel("Deaths over time (barplot)", plotOutput("DeathBarPlot"))
        ) # END TABSET
    ) # END MAINPANEL
    ), # END SIDEBAR

    textOutput("Text"),

    imageOutput(outputId = "gif")
) # END FLUIDPAGE






#DEFINE SERVER:
server <- function(input, output) {
    uiOutput(outputId = "sliderNum")
    
    output$gif <- renderImage( {
        list(src = "confirmed_cases.gif",
             contentType = 'image/gif',
             #width = 1200,
             #height = 900,
             alt = "The gif didn't work, but look at the other stuff :)"
        ) # END LIST
    }, deleteFile = FALSE
    ) # END RENDER IMAGE

    output$Text <- renderText("Source: Tianzhi Wu, Erqiang Hu, Xijin Ge*, Guangchuang Yu*. Open-source analytics tools for studying the COVID-19 coronavirus outbreak. medRxiv, 2020.02.25.20027433. doi: https://doi.org/10.1101/2020.02.25.20027433")

    # create the first plot:
    the_day <- reactive({
        as_date(as.Date(input$sliderNum, origin = "1899-12-30"))
    })

    output$CaseBarPlot <- renderPlot({

         d %>% 
            filter(time == as.Date(input$sliderNum, origin = "1899-12-30")) %>% 
            top_n(n = 10, wt = cum_confirm) %>% 
            ggplot(aes(x = reorder(country, cum_confirm), y = cum_confirm, fill = country)) +
            geom_bar(stat = "identity") +
            guides(fill = FALSE) +
            ggtitle(glue("Confirmed cases in countries (top 10 outside china) on ", the_day())) +
            ylab("cumulative confirmed cases") +
            xlab("Country") +
            coord_flip()

        
    })
    
    output$DeathBarPlot <- renderPlot({
        d %>% 
            filter(time == as.Date(input$sliderNum, origin = "1899-12-30")) %>% 
            top_n(n = 10, wt = cum_dead) %>% 
            ggplot(aes(x = reorder(country, cum_dead), y = cum_dead, fill = country)) +
            geom_bar(stat = "identity") +
            guides(fill = FALSE) +
            ggtitle(glue("Confirmed cases in countries on ", the_day())) +
            ylab("cumulative confirmed cases") +
            xlab("Country") +
            coord_flip()
        
    })

    output$timeplot <- renderPlot({
        # save the top countries
        top <- d %>% 
            filter(time == as.Date(as.Date(input$sliderNum), origin = "1899-12-30")) %>% 
            top_n(10, cum_confirm) %>% 
            pull(country)

        # get the maximum confirmed cases
        max <- d %>% 
            filter(time == as.Date(as.Date(input$sliderNum), origin = "1899-12-30")) %>% 
            select(cum_confirm) %>% 
            max()

        d %>% 
            filter(country %in% top) %>% 
            ggplot() +
            geom_line(mapping = aes(x = as.Date(time), y = cum_confirm, colour = country))+
            scale_colour_discrete(guide = "none") +
            geom_dl(aes(x = as.Date(time), y = cum_confirm, label = country), method = list(dl.combine("last.points"), cex = 0.6)) +
            xlim(c(as.Date("2020-02-21"), as.Date(input$sliderNum)))+
            ylim(c(0, max))+
            ylab("cumulative confirmed cases") +
            xlab("Date") +
            ggtitle("confirmed cases")
        #geom_label_repel(aes(x = time, y = cum_confirm, label = country),
        #na.rm = TRUE)
    })

    output$deathtimeplot <- renderPlot({
        # save the top countries
        top <- d %>% 
            filter(country != "China") %>% 
            filter(time == as.Date(as.Date(input$sliderNum), origin = "1899-12-30")) %>% 
            top_n(10, cum_dead) %>% 
            pull(country)
        
        max <- d %>% 
            filter(country != "China") %>% 
            filter(time == as.Date(as.Date(input$sliderNum), origin = "1899-12-30")) %>% 
            select(cum_dead) %>% 
            max()

        d %>% 
            filter(country %in% top) %>% 
            ggplot() +
            geom_line(mapping = aes(x = as.Date(time), y = cum_dead, colour = country))+
            scale_colour_discrete(guide = "none") +
            geom_dl(aes(x = as.Date(time), y = cum_dead, label = country), method = list(dl.combine("last.points"), cex = 0.6)) +
            xlim(c(as.Date("2020-02-21"), as.Date(input$sliderNum)))+
            ylim(c(0, max))+
            ylab("cumulative death cases") +
            xlab("Date") +
            ggtitle("cumulative death cases")
        #geom_label_repel(aes(x = time, y = cum_confirm, label = country),
        #na.rm = TRUE)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)



