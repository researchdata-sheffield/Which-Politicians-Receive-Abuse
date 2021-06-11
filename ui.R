library(shinydashboard)
library(leaflet)

header <- dashboardHeader(title = "Which Politicians Receive Abuse?")

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("busmap", height = 500)
           ),
           box(width = NULL,
               uiOutput("numVehiclesTable")
           )
    )
  )
)


appUI <- dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
)

