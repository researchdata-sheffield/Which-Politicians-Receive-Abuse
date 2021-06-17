##################################
##### Call shinyApp function #####
##################################
app <- shinyApp(ui = appUI, server = appServer)

runApp(app)
