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


####### DATA:
y <- load_nCov2019(lang = 'en', source='github')

# get global data:
d = y['global']

yesterday <- Sys.Date()-1

#######

# Define UI for application
ui <- fluidPage(
    # INIT SLIDER
    sliderInput(inputId = "sliderNum", 
                label = "Choose Date:", 
                min = as.Date("2020-02-21"), max = as.Date(Sys.Date()-1), 
                value = as.Date("2019-02-21"),
                animate = animationOptions(interval = 900)),
    
    plotOutput("plot"),
    
    plotlyOutput(outputId = "plot",
                 width = "1000px", height = "750px"),
    
    sliderInput(inputId = "timeplotDate", 
                label = "Choose End Date of plot:", 
                min = as.Date("2020-02-21"), max = as.Date(Sys.Date()-1), 
                value = as.Date("2020-02-21"),
                animate = animationOptions(interval = 900)),
    
    plotOutput("timeplot"),
    
    sliderInput(inputId = "deathplotDate", 
                label = "Choose End Date of plot:", 
                min = as.Date("2020-02-21"), max = as.Date(Sys.Date()-1), 
                value = as.Date("2020-02-21"),
                animate = animationOptions(interval = 1200)),
    
    plotOutput("deathtimeplot"),
    
    
    
    textOutput("Text")
)






#DEFINE SERVER:
server <- function(input, output) {
    uiOutput(outputId = "sliderNum")
    
    output$Text <- renderText("Source: Tianzhi Wu, Erqiang Hu, Xijin Ge*, Guangchuang Yu*. Open-source analytics tools for studying the COVID-19 coronavirus outbreak. medRxiv, 2020.02.25.20027433. doi: https://doi.org/10.1101/2020.02.25.20027433")
    
    # create the first plot:
    output$plot <- renderPlot({
        d %>% 
            filter(country != "China") %>% 
            filter(time == as.Date(input$sliderNum, origin = "1899-12-30")) %>% 
            top_n(n = 10, wt = cum_confirm) %>% 
            ggplot(aes(x = reorder(country, cum_confirm), y = cum_confirm, fill = country)) +
            geom_bar(stat = "identity") +
            guides(fill = FALSE) +
            ggtitle("Confirmed cases in countries (top 10 outside china)") +
            ylab("cumulative confirmed cases") +
            xlab("Country") +
            coord_flip()
        
        ggplotly(exciting, key = "text")
        
        })
    

    
    
        output$timeplot <- renderPlot({
        # save the top countries
            top <- d %>% 
                filter(country != "China") %>% 
                filter(time == as.Date(as.Date(input$timeplotDate), origin = "1899-12-30")) %>% 
                top_n(10, cum_confirm) %>% 
            pull(country)
        # get the maximum confirmed cases
        max <- d %>% 
            filter(country != "China") %>% 
            filter(time == as.Date(as.Date(input$timeplotDate), origin = "1899-12-30")) %>% 
            select(cum_confirm) %>% 
            max()
        
        d %>% 
            filter(country %in% top) %>% 
            ggplot() +
            geom_line(mapping = aes(x = as.Date(time), y = cum_confirm, colour = country))+
            scale_colour_discrete(guide = "none") +
            geom_dl(aes(x = as.Date(time), y = cum_confirm, label = country), method = list(dl.combine("last.points"), cex = 0.6)) +
            xlim(c(as.Date("2020-02-21"), as.Date(input$timeplotDate)))+
            ylim(c(0, max))+
            ylab("cumulative confirmed cases") +
            xlab("Date") +
            ggtitle("confirmed cases outside china (top 10)")
        #geom_label_repel(aes(x = time, y = cum_confirm, label = country),
        #na.rm = TRUE)
    })
    
    output$deathtimeplot <- renderPlot({
        # save the top countries
        top <- d %>% 
            filter(country != "China") %>% 
            filter(time == as.Date(as.Date(input$deathplotDate), origin = "1899-12-30")) %>% 
            top_n(10, cum_dead) %>% 
            pull(country)
        
        max <- d %>% 
            filter(country != "China") %>% 
            filter(time == as.Date(as.Date(input$deathplotDate), origin = "1899-12-30")) %>% 
            select(cum_dead) %>% 
            max()
        
        
        d %>% 
            filter(country %in% top) %>% 
            ggplot() +
            geom_line(mapping = aes(x = as.Date(time), y = cum_dead, colour = country))+
            scale_colour_discrete(guide = "none") +
            geom_dl(aes(x = as.Date(time), y = cum_dead, label = country), method = list(dl.combine("last.points"), cex = 0.6)) +
            xlim(c(as.Date("2020-02-21"), as.Date(input$deathplotDate)))+
            ylim(c(0, max))+
            ylab("cumulative death cases") +
            xlab("Date") +
            ggtitle("cumulative death cases outside china (top 10)")
        #geom_label_repel(aes(x = time, y = cum_confirm, label = country),
        #na.rm = TRUE)
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)



