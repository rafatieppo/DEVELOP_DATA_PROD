#=================================================================
                                             #Rafael Tieppo
                                             #tiepporc@unemat.br
                                             #02-12-2014
#=================================================================



#Week 1- Developing Data Products

#Getting stared
#install.packages("shiny")

library(shiny)
library(devtools)
library(shinyapps)


# To install DEVTOLLS no linux:
#http://stackoverflow.com/questions/20671814/non-zero-exit-status-r-3-0-1-xml-and-rcurl
#sudo apt-get install libcurl4-openssl-dev
#sudo apt-get install libxml2-dev

#install.packages("devtools", dep=T)
#devtools::install_github('rstudio/shinyapps')

shinyapps::setAccountInfo(name='agcom',
                          token='EE82F02017184885298380D01793BCA0',
                          secret='GzdFppruwDWKir+z2gaKHYBrZlEPxZs69G7bckUx')

shinyapps::deployApp('path/to/your/app')

getwd()
setwd("/home/rafatieppo/Dropbox/COURSERA")

runApp("TT")
runApp("DEVELOP_DATA_PROD")
deployApp()

