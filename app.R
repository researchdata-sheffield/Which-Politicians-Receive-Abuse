#source('processing.R')
#source('server.R')
#source('ui.R')


##################################
##### Call shinyApp function #####
##################################
app <- shinyApp(ui = appUI, server = appServer)

runApp(app)
