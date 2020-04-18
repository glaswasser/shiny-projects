A Corona visualization in Shiny
================

see n-covid-19 folder for the code

Go to <https://glaswasser.shinyapps.io/corona2/> to see the app running

Maybe I’ll add a tutorial here later…



## Structure

Parts:
server.R has all the data parts

ui.R has all the user interface parts

plots folder has all the code for the plots that are called within server.R and displayed in ui.R


The confirmed_over_time is greatly inspired by https://github.com/amrrs/animated_bar_charts_in_R, which on its part is inspired by https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other

