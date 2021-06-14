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
  tags$h3(
    "Make some changes", 
    style = "padding: 0 1.5rem; font-size: 1.7rem; font-weight: bold"
  ),
  # date
  dateRangeInput(
    'dateRange',
    label = paste('Date range'),
    start = "2019-11-03", end = "2019-12-14",
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
      title = "Time Series - abusive reply overtime",
      highchartOutput(outputId = "timeSeries", height = "500px")
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput(outputId = "circularPlot", height = "400px")
    ),
    column(
      width = 6,
      plotOutput(outputId = "circlePlot", height = "200px"),
      plotOutput(outputId = "donutPlot", height = "200px")
    )
  )
)


appUI <- dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
)

