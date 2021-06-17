library(rsconnect)
library(tidyverse)
library(lubridate)

library(ggrepel)
library(leaflet)
library(htmlwidgets)
library(highcharter)

library(shiny)
library(shinyjs)

##### Process string/numeric to date #####
stringToDate <- function(targetString) {
  if (class(targetString) != "character") {
    targetString <- toString(targetString)
  }
  
  newString = paste(
    substring(targetString, 1, 4), 
    substring(targetString, 5, 6), 
    substring(targetString, 7, 8), 
    sep = "-"
  )
  
  as.Date(newString)
}

parseUnicodeChar <- function(char) {
  as.character(
    parse(
      text = shQuote(
        gsub("<U\\+([A-Z0-9]+)>", "\\\\u\\1", char)
      )
    )
  )
}

# Process data from ORDA and save as processedData.csv
# source('processing.R')

# Use saved data to speed up the app -------------------
originData <- read_csv("data/processedData.csv")

### data for options ###
partyOptions <- pull(distinct(originData, party))
genderOptions <- pull(distinct(originData, gender))


library(profvis)

profvis({
  campaignTimeSeries = originData %>%
    group_by(startTime, party) %>%
    summarise(replyToAbusive = sum(replyToAbusive), fill = first(fill), .groups = "keep")
  
  timeSeriesColours = campaignTimeSeries %>% 
    group_by(party) %>%
    summarise(count = sum(replyToAbusive), fill = first(fill)) %>%
    arrange(desc(count)) %>%
    head(20)
  
  # create the chart
  hchartTS <- highchart() %>%
    hc_xAxis(
      title = list(text = "Date"),
      dateTimeLabelFormats = list(day = '%d %b'), 
      type = "datetime",
      plotLines = list(
        list(
          label = list(text = "TV Event (ITV)"),
          color = "#E8E8E8",
          width = 1,
          value = datetime_to_timestamp(as.Date("2019-11-19"))
        ),
        list(
          label = list(text = "TV Event (BBC)"),
          color = "#E8E8E8",
          width = 1,
          value = datetime_to_timestamp(as.Date("2019-11-22"))
        ),
        list(
          label = list(text = "TV Event (Channel 4)"),
          color = "#E8E8E8",
          width = 1,
          value = datetime_to_timestamp(as.Date("2019-11-28"))
        ),
        list(
          label = list(text = "TV Event (BBC)"),
          color = "#E8E8E8",
          width = 1,
          value = datetime_to_timestamp(as.Date("2019-12-06"))
        ),
        list(
          label = list(text = "Election"),
          color = "#E8E8E8",
          width = 1.5,
          value = datetime_to_timestamp(as.Date("2019-12-12"))
        )
      )
    ) %>%
    hc_yAxis(
      title = list(text = "Number of abusive tweets received")
    )
  
  # add line for each party
  for (row in 1:nrow(timeSeriesColours)) {
    hchartTS <- hchartTS %>% 
      hc_add_series(
        data = campaignTimeSeries[campaignTimeSeries$party == timeSeriesColours[row, ]$party, ], 
        type="line", 
        hcaes(x = startTime, y = replyToAbusive),
        marker = list(symbol='circle', radius=1),
        name = timeSeriesColours[row,]$party, 
        color = timeSeriesColours[row,]$fill 
      )
  }
  hchartTS
})
