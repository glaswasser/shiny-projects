# shiny-projects
my R shiny projects


## CORONA

using the nCov2019 package from https://github.com/GuangchuangYu/nCov2019


##corona-xt:

an extended version of the corona shiny app, with separate files for server, UI and plots, automatic integration of any new file put in the plots folder.
I learned about this structure in the course of my internship at Oefenweb.
(currently not available in the web, and also not so much in a "presentable" state)

Parts:
server.R has all the data parts

ui.R has all the user interface parts

plots folder has all the code for the plots that are called within server.R and displayed in ui.R



##n-COVID-19:

a more simple and "directly" structured web app of covid-19 data visualisation, available live at 
https://glaswasser.shinyapps.io/corona2

