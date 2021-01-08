# COVID 19 SHINY APP - ALEXANDER HEINZ


library(plotly)
library(tidyverse)
library(coronavirus)
library(lubridate)
library(shiny)
library(wordcloud2)

# get dropdown button function
source(file.path("functions", "dropdown_button.R"))

# theme for plots
theme_update(axis.line = element_line(),
             plot.title = element_text(hjust = 0.5),
             panel.grid.major = element_line(colour = "gray90"),
             panel.grid.minor = element_line(colour = "gray90"),
             panel.grid = element_blank(),
             panel.background = element_blank(),
             panel.border = element_blank())


# Define UI for application
shinyUI(fluidPage(
    textOutput(outputId = "note"),
    
    sidebarPanel(
        
        selectInput("plot_types", "Plot Type",
                    choices = "",
                    selected = ""),
        
        # get slider for plot conditional on plot type:
        # cumulative plot slider:
        conditionalPanel(condition = "input.plot_types == 'Top_10_cumulative'",
        sliderInput(inputId = "sliderNum", 
                    label = "Choose Date:", 
                    min = as.Date("2020-02-21"), max = as.Date(Sys.Date()-2), 
                    value = as.Date("2020-02-21"),
                    animate = animationOptions(interval = 400)
                    ), # END SLIDER
        selectInput(
            inputId = "cum_type",
            label = "Choose",
            choices = c("confirmed", "death", "recovered"),
            selected = "confirmed",
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
        ),
        # checkbox for relative
        checkboxInput("relative_cum", "Relative Cases per 1000 population", FALSE)
        
        
        ), # END CONDITIONAL PANEL CUMULATIVE PLOT
        

        # START CONDITIONAL PANEL WORDCLOUD
        conditionalPanel(condition = "input.plot_types == 'Wordcloud'",
                         sliderInput(inputId = "wordcloud_slider", 
                                     label = "Choose Date:", 
                                     min = Sys.Date() - months(1) + weeks(1), max = as.Date(Sys.Date()-2), 
                                     value = Sys.Date()-2,
                                     animate = animationOptions(interval = 800)
                         ), # END SLIDER
        ), # END CONDITIONAL PANEL WORDCLOUD
        
        # START CONDITIONAL PANEL OVER TIME PLOT
        # cases over time slider:
        conditionalPanel(condition = "input.plot_types == 'Cases_over_time' || input.plot_types == 'Distribution_over_time'",

        strong("Choose Countries:"),
        # DROPDOWN MENU: 1
        dropdownButton(
            label = "A-F", status = "default", width = 450,
            tags$label("Choose :"),
            fluidRow(
                column(
                    width = 4,
                    checkboxGroupInput(inputId = "check1a", label = NULL, choices = levels(factor(coronavirus$country))[1:22])
                ),
                column(
                    width = 4,
                    checkboxGroupInput(inputId = "check1b", label = NULL, choices = levels(factor(coronavirus$country))[23:43])
                ),
                column(
                  width = 4,
                  checkboxGroupInput(inputId = "check1c", label = NULL, choices = levels(factor(coronavirus$country))[44:63])
                )
                )
            ),# END DROPDOWN BUTTON 
        # DROPDOWN MENU 2
        dropdownButton(
          label = "G-N", status = "default", width = 450,
          tags$label("Choose :"),
          fluidRow(
            column(
              width = 4,
              checkboxGroupInput(inputId = "check2a", label = NULL, choices = levels(factor(coronavirus$country))[64:86])
            ),
            column(
              width = 4,
              checkboxGroupInput(inputId = "check2b", label = NULL, choices = levels(factor(coronavirus$country))[87:109])
            ),
            column(
              width = 4,
              checkboxGroupInput(inputId = "check2c", label = NULL, choices = levels(factor(coronavirus$country))[110:130])
            )
          )
        ),# END DROPDOWN BUTTON
        # DROPDOWN MENU 3
        dropdownButton(
          label = "O-Z", status = "default", width = 450,
          tags$label("Choose :"),
          fluidRow(
            column(
              width = 4,
              checkboxGroupInput(inputId = "check3a", label = NULL, choices = levels(factor(coronavirus$country))[131:149])
            ),
            column(
              width = 4,
              checkboxGroupInput(inputId = "check3b", label = NULL, choices = levels(factor(coronavirus$country))[150:170])
            ),
            column(
              width = 4,
              checkboxGroupInput(inputId = "check3c", label = NULL, choices = levels(factor(coronavirus$country))[171:191])
            )
          )
        ),# END DROPDOWN BUTTON
        
        conditionalPanel(condition = "input.plot_types == 'Cases_over_time'",
                         
        sliderInput(inputId = "overTime", 
                    label = "Choose Date Range:", 
                    min = as.Date("2020-02-21"), max = as.Date(Sys.Date()-6), 
                    value = c(as.Date("2020-02-21"), as.Date(Sys.Date()-6))
        ), # END SLIDER INPUT
        checkboxInput("relative_overtime", "Relative Cases per 1000 population", FALSE),
        ) # END CONDITIONAL PANEL OVER TIME PLOT
        ), # END CONDITIONAL PANEL BOTH OVER TIME PLOTS
        

        
        # PLOT DESCRIPTION (reactive on plot selected)
        strong("Plot description:"),
        textOutput(outputId = "plot_description")
    ),# END SIDEBAR
    
    
    # Show the generated plot
    mainPanel(
        conditionalPanel(condition = "input.plot_types != 'Wordcloud'",
                         plotlyOutput(outputId = "plot", width = "100%"),
                     #width = "1000px", height = "750px"),
        strong("Source:  raw data pulled and arranged by the Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) - using the coronavirus R package, see https://github.com/RamiKrispin/coronavirus - No guarantee for correctness!"),
        ), # end cond. on not word cloud
        conditionalPanel(condition = "input.plot_types == 'Wordcloud'",
                         wordcloud2Output("wordcloud"),
            strong("Source: Raw data ulled using the package \"newsanchor\", which connects to https://newsapi.org - huge thanks to the authors for the free usage options!"),
        ),
                         
    )# END MAIN PANEL
    
    
)# end fluid page
)# end shinyUI



# Source - World Population Prospects: The 2019 Revision. http://population.un.org/wpp., \n