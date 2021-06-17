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


# Read data -------------------------------------
## replyTo = replies received
## replyToAbusive = abusive tweets received
# originData <- read_csv("https://figshare.shef.ac.uk/ndownloader/files/22749029")
# 
# # Preprocess ------------------------------------
# originData <- originData %>%
#   rowwise() %>%
#   mutate(startTime = stringToDate(startTime)) %>%
#   mutate(endTime = stringToDate(endTime)) %>%
#   replace(is.na(.), "unknown") %>%
#   mutate(party = ifelse(party == "Sinn F\\u00e9in", "Sinn Fein", party)) %>%
#   mutate(
#     fill = case_when(
#       party == "Conservative Party" ~ "#0087DC",
#       party == "Labour Party" ~ "#DC241f",
#       party == "Liberal Democrats" ~ "#FDBB30",
#       party == "Scottish National Party" ~ "#FFFF00",
#       party == "Independent" ~ "#DDDDDD",
#       party == "Democratic Unionist Party" ~ "#D46A4C",
#       party == "The Brexit Party" ~ "#12B6CF",
#       party == "Sinn Fein" ~ "#326760",
#       TRUE ~ "#cccccc"
#     )
#   ) %>%
#   mutate(name = iconv(name, 'utf-8', 'ascii', sub=''))
# 
# write.csv(originData, file = "data/processedData.csv", row.names = FALSE)

# Use saved data to speed up the app -------------------
originData <- read_csv("data/processedData.csv")

### data for options ###
partyOptions <- pull(distinct(originData, party))
genderOptions <- pull(distinct(originData, gender))


#library(profvis)

