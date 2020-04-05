# had trouble installing the packages...
install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("glue")
install.packages("fs")
install.packages("shiny")
install.packages("gridExtra")
install.packages("nCov2019")
install.packages("remotes")
library(remote)
remotes::install_github("GuangchuangYu/nCov2019")
library(nCov2019)
nCov2019_set_country(country = 'Italy') # gets italy

latest <- get_nCov2019() # gets latest data

historical <- load_nCov2019()


names(latest)
names(historical)

tail(historical[1])
dashboard()


install.packages("sf")
