##################################
##### Call shinyApp function #####
##################################
options(shiny.reactlog=TRUE) 
app <- shinyApp(ui = appUI, server = appServer)

runApp(app)
