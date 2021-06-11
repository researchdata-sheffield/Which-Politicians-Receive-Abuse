library(shiny)
library(shinythemes)


source('processing.R')
source('server.R')
source('ui.R')


##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = appUI, server = appServer)

