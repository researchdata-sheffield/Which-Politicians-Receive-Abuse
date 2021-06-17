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


#library(profvis)


