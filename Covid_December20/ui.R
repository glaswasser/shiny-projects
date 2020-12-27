#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(tidyverse)
library(coronavirus)
#update_dataset(silence = TRUE)
library(shiny)




dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
    
    status <- match.arg(status)
    # dropdown button content
    html_ul <- list(
        class = "dropdown-menu",
        style = if (!is.null(width)) 
            paste0("width: ", validateCssUnit(width), ";"),
        lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
    )
    # dropdown button apparence
    html_button <- list(
        class = paste0("btn btn-", status," dropdown-toggle"),
        type = "button", 
        `data-toggle` = "dropdown"
    )
    html_button <- c(html_button, list(label))
    html_button <- c(html_button, list(tags$span(class = "caret")))
    # final result
    tags$div(
        class = "dropdown",
        do.call(tags$button, html_button),
        do.call(tags$ul, html_ul),
        tags$script(
            "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
    )
}

# Define UI for application
shinyUI(fluidPage(

    
    sidebarPanel(
        selectInput("plot_types", "Plot Type",
                    choices = "",
                    selected = ""),
        
        # get slider for plot conditional on plot type:
        # cumulative plot slider:
        conditionalPanel(condition = "input.plot_types == 'cumulative_plot'",
        sliderInput(inputId = "sliderNum", 
                    label = "Choose Date:", 
                    min = as.Date("2020-02-21"), max = as.Date(Sys.Date()-2), 
                    value = as.Date("2020-02-21"),
                    animate = animationOptions(interval = 400)
                    ), # END SLIDER
        # checkbox for relative
        checkboxInput("relative", "Relative Cases per 1000 population", FALSE),
        
        
        ), # END CONDITIONAL PANEL CUMULATIVE PLOT
        
        # START CONDITIONAL PANEL OVER TIME PLOT
        # cases over time slider:
        conditionalPanel(condition = "input.plot_types == 'Cases_over_time'",
                         sliderInput(inputId = "overTime", 
                                     label = "Choose Date Range:", 
                                     min = as.Date("2020-02-21"), max = as.Date(Sys.Date()-2), 
                                     value = c(as.Date("2020-02-21"), as.Date(Sys.Date()-2))
        ), # END SLIDER INPUT
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
        checkboxInput("relative", "Relative Cases per 1000 population", FALSE),
        ), # END CONDITIONAL PANEL OVER TIME PLOT

        # PLOT DESCRIPTION (reactive on plot selected)
        strong("Plot description:"),
        textOutput(outputId = "plot_description")
    ),# END SIDEBAR
    
    
    # Show the generated plot
    mainPanel(
 
      
        plotlyOutput(outputId = "plot",
                     width = "1000px", height = "750px"),

    )# END MAIN PANEL
    
    
)# end fluid page
)# end shinyUI
