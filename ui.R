library(shinythemes)
library(shinydashboard)
library(htmlwidgets)

# Header -------------------------------
header <- dashboardHeader(
  title = span(
    "Dataviz.Shef", 
    style = "font-weight: 900; color: #00aeef"
  )
)

headerSubTitle <- list(
  div(
    style = "float:left; height:100%; padding: 10px; font-size: 2rem; font-weight: 600; margin-left: 1rem", 
    "Which Politicians Receive Abuse During 2019 Election Campaign?"
  )
)
header$children[[3]]$children[[4]] <- headerSubTitle


# Sidebar -------------------------------
sidebar <- dashboardSidebar(
  # initialise shinyjs and scripts ---------------------
  shinyjs::useShinyjs(),
  tags$head(includeHTML(("www/google-analytics.html"))),
  tags$script(src = "events.js"),
  
  tags$h3(
    "Make some changes", 
    style = "padding: 0 1.5rem; font-size: 1.7rem; font-weight: bold"
  ),
  # date
  dateRangeInput(
    'dateRange',
    label = paste('Date range'),
    start = "2019-11-17", end = "2019-12-14",
    min = "2019-11-03", max = "2019-12-14",
    separator = " to ", format = "dd/mm/yyyy",
    startview = 'month', weekstart = 1
  ),
  selectizeInput(
    'party', 'filter by parties',
    choices = partyOptions,
    multiple = TRUE,
    options = list(
      placeholder = 'Select a party'
    )
  ),
  selectizeInput(
    'genders', 'filter by gender',
    choices = genderOptions,
    multiple = TRUE,
    options = list(
      placeholder = 'Select a gender'
    )
  ),
  actionButton("applyFilter", "Apply filter options!"),
  tags$h3(
    "About", 
    style = "padding: 0 1.5rem; margin: 5rem auto 1rem auto; font-size: 1.7rem; font-weight: bold"
  ),
  tags$p(
    list(
      "The shiny app is built based on the ",
      tags$a("dataset", href = "https://doi.org/10.15131/shef.data.12340994"),
      "made available by Gorrell, G., Bakir, M., Roberts, I., Greenwood, M., et al. (2020) on ",
      tags$a("Online Research Data", href = "https://orda.shef.ac.uk"),
      "(the University of Sheffield's data repository). You might also be interested in the open access ",
      tags$a("article", href = "https://doi.org/10.1140/epjds/s13688-020-00236-9"),
      "they have published alongside the dataset."
    ),
    style = "padding: 0 1.5rem"
  ),
  tags$div(
    list(
      tags$a(icon("star", "fa-2x"), href = "https://dataviz.shef.ac.uk"),
      tags$a(icon("github", "fa-2x"), href = "https://github.com/researchdata-sheffield/Which-Politicians-Receive-Abuse"),
      tags$a(icon("envelope", "fa-2x"), href = "mailto:y.weng@sheffield.ac.uk"),
      tags$a(icon("slack", "fa-2x"), href = "https://shef-dataviz.slack.com/archives/DRF6V81L0")
    ),
    style = "margin-top: 5rem; padding: 0 1.5rem; display: flex; flex-wrap: wrap; justify-content: space-evenly"
  )
)


# Sidebar -------------------------------------



body <- dashboardBody(
  fluidRow(
    valueBoxOutput("numOfPoliticians", width = 3),
    valueBoxOutput("numOfParties", width = 3),
    valueBoxOutput("totalAbusiveReplies", width = 3),
    valueBoxOutput("percentOfAbusiveReplies", width = 3)
  ),
  fluidRow(
    box(
      width = 12, 
      solidHeader = TRUE,
      title = "Time Series - abusive replies overtime",
      highchartOutput(outputId = "timeSeries", height = "500px")
    )
  ),
  fluidRow(
    column(
      width = 8,
      plotOutput(outputId = "circularPlot", height = "850px")
    ),
    column(
      width = 4,
      fluidRow(
        box(
          width = 12,
          solidHeader = TRUE,
          highchartOutput(outputId = "barChart", height = "400px")
        ),
        box(
          width = 12,
          solidHeader = TRUE,
          plotOutput(outputId = "donutPlot", height = "400px")
        )
      )
    )
  ),
  # fluidRow(
  #   column(
  #     width = 12,
  #     style = "padding: 1rem 5rem; margin: 2rem 0 2rem 1.5rem; display: flex; justify-content: center; background: white",
  #     circlepackeROutput(outputId = "circlePlot", height = "600px", width = "600px")
  #   )
  # )
  fluidRow(
    box(
      style = "padding: 1rem; background: white; margin: 2rem auto",
      width = 12,
      solidHeader = TRUE,
      title = "Treemap - abusive tweets received in total (click to see more details)",
      highchartOutput(outputId = "treemap", height = "800px")
    )
  )
)

appUI <- dashboardPage(
  title="Which Politicians Receive Abuse? | Dataviz.Shef",
  skin = "black",
  header,
  sidebar,
  body
)

